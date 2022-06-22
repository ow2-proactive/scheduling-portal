/*
 * ProActive Parallel Suite(TM):
 * The Open Source library for parallel and distributed
 * Workflows & Scheduling, Orchestration, Cloud Automation
 * and Big Data Analysis on Enterprise Grids & Clouds.
 *
 * Copyright (c) 2007 - 2017 ActiveEon
 * Contact: contact@activeeon.com
 *
 * This library is free software: you can redistribute it and/or
 * modify it under the terms of the GNU Affero General Public License
 * as published by the Free Software Foundation: version 3 of
 * the License.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 *
 * If needed, contact us to obtain a release under GPL Version 2 or 3
 * or a different license than the AGPL.
 */
package org.ow2.proactive_grid_cloud_portal.scheduler.client.controller;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.ow2.proactive_grid_cloud_portal.common.client.json.JSONUtils;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LogModel;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LoginModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Job;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.JobOutput;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.OutputMode;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Scheduler;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerController;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerModelImpl;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerServiceAsync;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SelectionTarget;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Task;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.OutputModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.OutputView;
import org.ow2.proactive_grid_cloud_portal.scheduler.shared.SchedulerConfig;

import com.google.gwt.http.client.Request;
import com.google.gwt.json.client.JSONObject;
import com.google.gwt.user.client.Timer;
import com.google.gwt.user.client.Window;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.smartgwt.client.widgets.layout.Layout;


/**
 * Controller for an output view.
 * @author the activeeon team.
 *
 */
public class OutputController extends AbstractSelectedTargetController<OutputModel> {

    /** periodically fetches live output */
    private Timer liveOutputUpdater = null;

    /** contains all pending getTaskOutput requests, taskId as key */
    private Map<String, Request> taskOutputRequests = null;

    protected OutputView view;

    public OutputController(SchedulerController parentController) {
        super(parentController);
        SchedulerModelImpl schedulerModel = (SchedulerModelImpl) parentController.getModel();
        this.model = new OutputModel(schedulerModel);

        this.taskOutputRequests = new HashMap<String, Request>();
    }

    public Layout buildView() {
        this.view = new OutputView(this);
        return this.view.build();
    }

    public void refreshOutput() {
        OutputMode outputMode = this.model.getOutputMode();
        String jobId = this.model.getCurrentOutput().getJobId();
        if (outputMode == OutputMode.LOG_FULL) {
            String sessionId = LoginModel.getInstance().getSessionId();
            if (this.model.getSelectionTarget() == SelectionTarget.JOB_TARGET) {
                this.downloadFullJobLogs(sessionId, jobId);
            } else {
                Task task = this.parentController.getSelectedTask();
                downloadFullTaskLogs(sessionId, jobId, task.getName());
            }
        } else {
            if (this.model.getSelectionTarget() == SelectionTarget.JOB_TARGET) {
                this.fetchJobOutput(outputMode);
            } else {
                Task task = this.parentController.getSelectedTask();
                this.fetchTaskOutput(jobId, task, outputMode);
            }
        }
    }

    /**
     * Fetch the output for the currently selected job
     * store the result (or error msg) in the model
     * @param logMode one of {@link SchedulerServiceAsync#LOG_ALL}, {@link SchedulerServiceAsync#LOG_STDERR},
     *   {@link SchedulerServiceAsync#LOG_STDOUT}
     */
    public void fetchJobOutput(OutputMode logMode) {
        JobOutput currentOutput = this.model.getCurrentOutput();
        String jobId = currentOutput.getJobId();

        String sessionId = LoginModel.getInstance().getSessionId();
        SchedulerServiceAsync scheduler = Scheduler.getSchedulerService();

        scheduler.getJobInfo(sessionId, jobId, new AsyncCallback<String>() {

            @Override
            public void onFailure(Throwable caught) {
                String msg = JSONUtils.getJsonErrorMessage(caught);
                if (msg.contains("HTTP 403 Forbidden")) {
                    view.goToNotAuthorized();
                }
            }

            @Override
            public void onSuccess(String result) {
                List<Task> tasks = model.getParentModel().getTasksModel().getTasks();

                for (Task t : tasks) {
                    switch (t.getStatus()) {
                        case SKIPPED:
                        case PENDING:
                        case SUBMITTED:
                        case NOT_STARTED:
                            break;
                        default:
                            fetchTaskOutput(jobId, t, logMode);
                            break;
                    }
                }

                currentOutput.setComplete(true);
            }
        });

    }

    /**
     * Fetch the output for a single task,
     * store the result (or error message) in the model
     * 
     * @param jobId id of the job containing this task
     * @param task task for which the output should be fetched
     * @param logMode one of {@link SchedulerServiceAsync#LOG_ALL}, {@link SchedulerServiceAsync#LOG_STDERR},
     *   {@link SchedulerServiceAsync#LOG_STDOUT}
     */
    public void fetchTaskOutput(final String jobId, final Task task, final OutputMode logMode) {
        SchedulerServiceAsync scheduler = Scheduler.getSchedulerService();
        Request req = scheduler.getTaskOutput(LoginModel.getInstance().getSessionId(),
                                              "" + jobId,
                                              task.getName(),
                                              logMode,
                                              new AsyncCallback<String>() {
                                                  public void onFailure(Throwable caught) {
                                                      String msg = JSONUtils.getJsonErrorMessage(caught);
                                                      // might be an exception
                                                      try {
                                                          JSONObject json = JSONUtils.parseJSON(caught.getMessage())
                                                                                     .isObject();
                                                          if (json.containsKey("stackTrace")) {
                                                              msg = json.get("stackTrace").isString().stringValue();
                                                              msg = msg.replace("\t", "&nbsp;&nbsp;&nbsp;&nbsp;");
                                                              msg = msg.replace("\n", "<br>");
                                                          }
                                                      } catch (Throwable t) {
                                                          // not json
                                                      }
                                                      model.setTaskOutput(jobId,
                                                                          task,
                                                                          "[" + task.getName() +
                                                                                "] <span style='color:red;'>" + msg +
                                                                                "</span>");
                                                      LogModel.getInstance()
                                                              .logMessage("Failed to get output for task " +
                                                                          task.getName() + " in job " +
                                                                          jobId /* + ": " + msg */);

                                                      taskOutputRequests.remove("" + task.getId());
                                                  }

                                                  public void onSuccess(String result) {
                                                      model.setTaskOutput(jobId, task, result);
                                                      LogModel.getInstance()
                                                              .logMessage("Successfully fetched output for task " +
                                                                          task.getName() + " in job " + jobId);

                                                      taskOutputRequests.remove("" + task.getId());
                                                  }
                                              });
        this.taskOutputRequests.put("" + task.getId(), req);
    }

    public void checkLiveEnabled(Job job) {
        if (this.model.isLiveEnabled() && job != null) {
            if (job.isExecuted()) {
                if (this.model.isLive()) {
                    this.model.getCurrentOutput().setLiveEnabled(false);
                } else {
                    this.model.setLiveEnabled(false, true);
                }
            }
        }
    }

    public void toggleLive(boolean live) {
        if (live) {
            this.cancelCurrentRequests();
            this.startLiveOutput();
        } else {
            this.stopLiveOutput();
            JobOutput jobOutput = this.model.getCurrentOutput();
            if (!jobOutput.isComplete()) {
                jobOutput.resetLines();
            }
        }
        this.model.setLive(live, true);
    }

    /**
     * Start the timer that will periodically fetch live logs
     */
    public void startLiveOutput() {
        this.liveOutputUpdater = new Timer() {

            @Override
            public void run() {
                doFetchLiveLog();
            }
        };
        int refreshTime = SchedulerConfig.get().getLivelogsRefreshTime();
        this.liveOutputUpdater.scheduleRepeating(refreshTime);
        this.doFetchLiveLog();
    }

    /**
     * Stop the timer that will periodically fetch live logs
     */
    public void stopLiveOutput() {
        if (this.liveOutputUpdater != null) {
            this.liveOutputUpdater.cancel();
            this.liveOutputUpdater = null;
        }
    }

    public void restartLiveTimer() {
        this.stopLiveOutput();
        this.startLiveOutput();
    }

    protected void doFetchLiveLog() {
        JobOutput currentOutput = this.model.getCurrentOutput();
        final String jobId = currentOutput.getJobId();
        if (!(currentOutput.isLive() && currentOutput.isLiveEnabled())) {
            LogModel.getInstance().logMessage("stop fetching live logs and disable live for job " + jobId);
            return;
        }

        SchedulerServiceAsync scheduler = Scheduler.getSchedulerService();
        scheduler.getLiveLogJob(LoginModel.getInstance().getSessionId(), jobId, new AsyncCallback<String>() {
            public void onSuccess(String result) {
                if (result.length() > 0) {
                    LogModel.getInstance()
                            .logMessage("Fetched livelog chunk for job " + jobId + " (" + result.length() + " chars)");
                    model.appendLiveOutput(jobId, result);
                }
            }

            public void onFailure(Throwable caught) {
                String msg = JSONUtils.getJsonErrorMessage(caught);
                LogModel.getInstance().logImportantMessage("Failed to fetch live log for job " + jobId + ": " + msg);
            }
        });
    }

    public void cancelCurrentRequests() {
        if (this.model.isLive()) {
            this.stopLiveOutput();
        } else {
            for (Request req : this.taskOutputRequests.values()) {
                req.cancel();
            }
            this.taskOutputRequests.clear();
        }
    }

    public void downloadFullJobLogs(String sessionId, String jobId) {
        String url = SchedulerConfig.get().getRestPublicUrlIfDefinedOrOverridden() + "/scheduler/jobs/" + jobId +
                     "/log/full?sessionid=" + sessionId;
        Window.open(url, "_blank", "");
    }

    public void downloadFullTaskLogs(String sessionId, String jobId, String taskName) {
        String url = SchedulerConfig.get().getRestPublicUrlIfDefinedOrOverridden() + "/scheduler/jobs/" + jobId +
                     "/tasks/" + taskName.replace("#", "%23") + "/result/log/full?sessionid=" + sessionId;
        Window.open(url, "_blank", "");
    }

    public void changeJobOutputContext(Job job) {
        this.cancelCurrentRequests();
        if (job == null) {
            this.changeCurrentOutput(null, false);
        } else {
            this.changeCurrentOutput(job.getId().toString(), true);
            this.view.refreshOutputModeSelect(model.getOutputMode());
            this.checkLiveEnabled(job);
            if (this.model.isLive()) {
                this.startLiveOutput();
            }
        }

    }

    public void changeTaskOutputContext(Task task) {
        this.cancelCurrentRequests();
        if (task == null) {
            this.changeCurrentOutput(null, false);
        } else {
            String jobId = Long.toString(task.getJobId());
            this.changeCurrentOutput(jobId, false);
        }
        this.model.setLiveEnabled(false, false);
        this.model.setLive(false, false);
    }

    public void changeOutputMode(OutputMode outputMode) {
        this.cancelCurrentRequests();
        boolean changed = this.model.setOutputMode(outputMode);
        if (changed) {
            this.refreshOutput();
            this.view.refreshOutputModeSelect(outputMode);
        }
    }

    public Collection<List<String>> getLinesToDisplay(JobOutput output) {
        if (this.model.getSelectionTarget() == SelectionTarget.TASK_TARGET) {
            Task task = this.parentController.getSelectedTask();
            List<String> lines = output.getLines(task);

            Collection<List<String>> result = new ArrayList<List<String>>(1);

            if (lines != null) {
                result.add(lines);
            }

            return result;
        } else {
            return output.getLines();
        }
    }

    public void changeCurrentOutput(String jobId, boolean resetIfNotComplete) {
        if (jobId == null) {
            this.model.setCurrentOutput(null);
        } else {
            JobOutput jobOutput = this.model.getJobOutput(jobId, true);
            if (resetIfNotComplete && !jobOutput.isLive() && !jobOutput.isComplete()) {
                jobOutput.resetLines();
            }

            this.model.setCurrentOutput(jobOutput);
        }
    }
}
