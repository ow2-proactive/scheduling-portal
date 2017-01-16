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

import org.ow2.proactive_grid_cloud_portal.common.client.json.JSONUtils;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LogModel;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LoginModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Job;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Scheduler;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerController;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerModelImpl;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerServiceAsync;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SelectionTarget;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Task;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.ServerLogsModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.ServerLogsView;

import com.google.gwt.http.client.Request;
import com.google.gwt.json.client.JSONObject;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.smartgwt.client.widgets.layout.Layout;


/**
 * Controller for the server logs view.
 * @author the activeeon team.
 *
 */
public class ServerLogsController extends AbstractSelectedTargetController<ServerLogsModel> {

    protected ServerLogsView view;

    protected Request currentRequest = null;

    public ServerLogsController(SchedulerController parentController) {
        super(parentController);
        SchedulerModelImpl schedulerModel = (SchedulerModelImpl) parentController.getModel();
        this.model = new ServerLogsModel(schedulerModel);
    }

    public Layout buildView() {
        this.view = new ServerLogsView(this);
        return this.view.build();
    }

    /**
     * Fetch server logs for a single task
     * 
     * @param jobId id of the job containing this task
     * @param taskname task for which the output should be fetched
     * @param logs one of {@link SchedulerServiceAsync#LOG_ALL}, {@link SchedulerServiceAsync#LOG_STDERR},
     *   {@link SchedulerServiceAsync#LOG_STDOUT}
     */
    public void getTaskServerLogs(final int jobId, final String taskname) {
        SchedulerServiceAsync scheduler = Scheduler.getSchedulerService();
        this.currentRequest = scheduler.getTaskServerLogs(LoginModel.getInstance().getSessionId(),
                                                          jobId,
                                                          taskname,
                                                          new AsyncCallback<String>() {
                                                              public void onFailure(Throwable caught) {
                                                                  String msg = JSONUtils.getJsonErrorMessage(caught);
                                                                  // might be an exception
                                                                  try {
                                                                      JSONObject json = JSONUtils.parseJSON(caught.getMessage())
                                                                                                 .isObject();
                                                                      if (json.containsKey("stackTrace")) {
                                                                          msg = json.get("stackTrace")
                                                                                    .isString()
                                                                                    .stringValue();
                                                                          msg = msg.replace("\t",
                                                                                            "&nbsp;&nbsp;&nbsp;&nbsp;");
                                                                          msg = msg.replace("\n", "<br>");
                                                                      }
                                                                  } catch (Throwable t) {
                                                                      // not json
                                                                  }
                                                                  LogModel.getInstance()
                                                                          .logMessage("Failed to get server logs for task " +
                                                                                      taskname + " in job " +
                                                                                      jobId /*
                                                                                             * +
                                                                                             * ": "
                                                                                             * + msg
                                                                                             */);
                                                                  currentRequest = null;
                                                              }

                                                              public void onSuccess(String result) {
                                                                  LogModel.getInstance()
                                                                          .logMessage("Successfully fetched server logs for task " +
                                                                                      taskname + " in job " + jobId);
                                                                  model.setLogs(result, Integer.toString(jobId));
                                                                  currentRequest = null;
                                                              }
                                                          });
    }

    /**
     * Fetch server logs for a single job
     * 
     * @param jobId id of the job containing this task
     * @param logs one of {@link SchedulerServiceAsync#LOG_ALL}, {@link SchedulerServiceAsync#LOG_STDERR},
     *   {@link SchedulerServiceAsync#LOG_STDOUT}
     */
    public void getJobServerLogs(final int jobId) {
        SchedulerServiceAsync scheduler = Scheduler.getSchedulerService();
        this.currentRequest = scheduler.getJobServerLogs(LoginModel.getInstance().getSessionId(),
                                                         jobId,
                                                         new AsyncCallback<String>() {
                                                             public void onFailure(Throwable caught) {
                                                                 String msg = JSONUtils.getJsonErrorMessage(caught);
                                                                 // might be an exception
                                                                 try {
                                                                     JSONObject json = JSONUtils.parseJSON(caught.getMessage())
                                                                                                .isObject();
                                                                     if (json.containsKey("stackTrace")) {
                                                                         msg = json.get("stackTrace")
                                                                                   .isString()
                                                                                   .stringValue();
                                                                         msg = msg.replace("\t",
                                                                                           "&nbsp;&nbsp;&nbsp;&nbsp;");
                                                                         msg = msg.replace("\n", "<br>");
                                                                     }
                                                                 } catch (Throwable t) {
                                                                     // not json
                                                                 }
                                                                 LogModel.getInstance().logMessage(
                                                                                                   "Failed to get server logs for a job " +
                                                                                                   jobId);
                                                                 currentRequest = null;
                                                             }

                                                             public void onSuccess(String result) {
                                                                 LogModel.getInstance().logMessage(
                                                                                                   "Successfully fetched server logs for job " +
                                                                                                   jobId);
                                                                 model.setLogs(result, Integer.toString(jobId));
                                                                 currentRequest = null;
                                                             }
                                                         });
    }

    @Override
    public void changeJobOutputContext(Job job) {
        this.cancelCurrentRequest();
        String jobId = null;
        if (job != null) {
            jobId = job.getId().toString();
        }
        this.model.resetLogs(jobId);
    }

    @Override
    public void changeTaskOutputContext(Task task) {
        this.cancelCurrentRequest();
        String jobId = null;
        if (task != null) {
            jobId = Long.toString(task.getJobId());
        }
        this.model.resetLogs(jobId);
    }

    @Override
    public void refreshOutput() {
        this.cancelCurrentRequest();
        if (this.model.getSelectionTarget() == SelectionTarget.JOB_TARGET) {
            Job job = this.parentController.getSelectedJob();
            if (job != null) {
                int jobId = job.getId();
                this.getJobServerLogs(jobId);
            }
        } else {
            Task task = this.parentController.getSelectedTask();
            if (task != null) {
                String taskName = task.getName();
                int jobId = (int) task.getJobId();
                this.getTaskServerLogs(jobId, taskName);
            }
        }
    }

    protected void cancelCurrentRequest() {
        if (this.currentRequest != null) {
            this.currentRequest.cancel();
            this.currentRequest = null;
        }
    }

}
