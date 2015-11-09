/*
 * ################################################################
 *
 * ProActive Parallel Suite(TM): The Java(TM) library for
 *    Parallel, Distributed, Multi-Core Computing for
 *    Enterprise Grids & Clouds
 *
 * Copyright (C) 1997-2011 INRIA/University of
 *                 Nice-Sophia Antipolis/ActiveEon
 * Contact: proactive@ow2.org or contact@activeeon.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Affero General Public License
 * as published by the Free Software Foundation; version 3 of
 * the License.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
 * USA
 *
 * If needed, contact us to obtain a release under GPL Version 2 or 3
 * or a different license than the AGPL.
 *
 *  Initial developer(s):               The ProActive Team
 *                        http://proactive.inria.fr/team_members.htm
 *  Contributor(s):
 *
 * ################################################################
 * $$PROACTIVE_INITIAL_DEV$$
 */

package org.ow2.proactive_grid_cloud_portal.scheduler.client.controller;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.ow2.proactive_grid_cloud_portal.common.client.json.JSONUtils;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LogModel;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LoginModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Job;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Scheduler;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerController;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerModelImpl;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerServiceAsync;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Task;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.JobsModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.OutputModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.OutputView;
import org.ow2.proactive_grid_cloud_portal.scheduler.shared.SchedulerConfig;

import com.google.gwt.http.client.Request;
import com.google.gwt.json.client.JSONObject;
import com.google.gwt.user.client.Timer;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.smartgwt.client.widgets.layout.Layout;


/**
 * Controller for an output view.
 * @author the activeeon team.
 *
 */
public class OutputController {

    /** ids of jobs we should auto fetch */
    private Set<String> liveOutputJobs = null;
    /** periodically fetches live output */
    private Timer liveOutputUpdater = null;
    
    /** contains all pending getTaskOutput requests, taskId as key */
    private Map<String, Request> taskOutputRequests = null;
    
    protected SchedulerController parentController;
    
    protected OutputModel model;
    
    protected OutputView view;
    
    public OutputController(SchedulerController parentController) {
        this.parentController = parentController;
        SchedulerModelImpl schedulerModel = (SchedulerModelImpl) parentController.getModel();
        this.model = new OutputModel(schedulerModel);
        
        this.taskOutputRequests = new HashMap<String, Request>();
        this.liveOutputJobs = new HashSet<String>();
    }
    
    
    public Layout buildView(){
        this.view = new OutputView(this);
        return this.view.build();
    }
    
    
    /**
     * Fetch the output for the currently selected job
     * store the result (or error msg) in the model
     * @param logMode one of {@link SchedulerServiceAsync#LOG_ALL}, {@link SchedulerServiceAsync#LOG_STDERR},
     *   {@link SchedulerServiceAsync#LOG_STDOUT}
     */
    public void getJobOutput(int logMode) {
        JobsModel jobsModel = this.parentController.getExecutionController().getJobsController().getModel();
        Job j = jobsModel.getSelectedJob();
        if (j == null)
            return;

        List<Task> tasks = this.model.getParentModel().getTasksModel().getTasks();
        if (tasks.isEmpty()) {
            // notify the listeners, they will figure out there is no output
            this.model.updateOutput(j.getId());
        }

        for (Task t : tasks) {
            switch (t.getStatus()) {
            case SKIPPED:
            case PENDING:
            case SUBMITTED:
            case NOT_STARTED:
                break;
            default:
                this.getTaskOutput(j.getId(), t, logMode);
                break;
            }
        }
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
    public void getTaskOutput(final int jobId, final Task task, final int logMode) {
        this.model.setLiveOutput("" + jobId, false);
        this.liveOutputJobs.remove("" + jobId);
        if (this.liveOutputJobs.isEmpty()) {
            stopLiveTimer();
        }

        SchedulerServiceAsync scheduler = Scheduler.getSchedulerService();
        Request req = scheduler.getTaskOutput(LoginModel.getInstance().getSessionId(), "" + jobId, task.getName(), logMode,
                new AsyncCallback<String>() {
            public void onFailure(Throwable caught) {
                String msg = JSONUtils.getJsonErrorMessage(caught);
                // might be an exception
                try {
                    JSONObject json = JSONUtils.parseJSON(caught.getMessage()).isObject();
                    if (json.containsKey("stackTrace")) {
                        msg = json.get("stackTrace").isString().stringValue();
                        msg = msg.replace("\t", "&nbsp;&nbsp;&nbsp;&nbsp;");
                        msg = msg.replace("\n", "<br>");
                    }
                } catch (Throwable t) {
                    // not json
                }
                model.setTaskOutput(jobId, task,
                        "[" + task.getName() + "] <span style='color:red;'>" + msg + "</span>");
                LogModel.getInstance().logMessage("Failed to get output for task " +
                        task.getName() + " in job " + jobId /* + ": " + msg */);

                taskOutputRequests.remove("" + task.getId());
            }

            public void onSuccess(String result) {
                model.setTaskOutput(jobId, task, result);
                LogModel.getInstance().logMessage("Successfully fetched output for task " +
                        task.getName() + " in job " + jobId);

                taskOutputRequests.remove("" + task.getId());
            }
        });
        this.taskOutputRequests.put("" + task.getId(), req);
    }
    
    /**
     * Start the timer that will periodically fetch live logs
     */
    public void startLiveTimer() {
        this.liveOutputUpdater = new Timer() {

            @Override
            public void run() {
                for (Iterator<String> it = liveOutputJobs.iterator(); it.hasNext();) {
                    final String jobId = it.next();
                    doFetchLiveLog(jobId);

                    int id = Integer.parseInt(jobId);
                    Job j = parentController.getExecutionController().getJobsController().getModel().getJobs().get(id);
                    if (j == null || j.isExecuted()) {
                        LogModel.getInstance().logMessage("stop fetching live logs for job " + jobId);
                        it.remove();
                        model.setLiveOutput(jobId, false);
                        model.appendLiveOutput(jobId, "");
                    }
                }
            }
        };
        this.liveOutputUpdater.scheduleRepeating(SchedulerConfig.get().getLivelogsRefreshTime());
    }
    
    
    /**
     * Stop the timer that will periodically fetch live logs
     */
    public void stopLiveTimer() {
        if (this.liveOutputUpdater == null)
            return;

        this.liveOutputUpdater.cancel();
        this.liveOutputUpdater = null;
    }
    
    
    public void restartLiveTimer(){
        this.stopLiveTimer();
        this.startLiveTimer();
    }
    
    
    private void doFetchLiveLog(final String jobId) {
        SchedulerServiceAsync scheduler = Scheduler.getSchedulerService();
        scheduler.getLiveLogJob(LoginModel.getInstance().getSessionId(), jobId,
                new AsyncCallback<String>() {
            public void onSuccess(String result) {
                if (result.length() > 0) {
                    LogModel.getInstance().logMessage("Fetched livelog chunk for job " + jobId + " (" +
                            result.length() + " chars)");
                    model.appendLiveOutput(jobId, result);
                }
            }

            public void onFailure(Throwable caught) {
                String msg = JSONUtils.getJsonErrorMessage(caught);
                LogModel.getInstance().logImportantMessage("Failed to fetch live log for job " + jobId + ": " + msg);
            }
        });
    }
    
    
    /**
     * Auto fetch the output for the currently selected job
     */
    public void getLiveOutput() {
        Job j = this.parentController.getExecutionController().getJobsController().getModel().getSelectedJob();
        if (j == null)
            return;

        if (this.model.getParentModel().getTasksModel().getTasks().isEmpty())
            return;

        final String jobId = "" + j.getId();

        if (this.liveOutputJobs.contains(jobId)) {
            model.appendLiveOutput(jobId, "");
            return;
        }

        this.model.setLiveOutput(jobId, true);
        this.liveOutputJobs.add(jobId);

        if (this.liveOutputUpdater == null) {
            this.startLiveTimer();
        }
    }

    public void deleteLiveLogJob() {
        Job j = this.parentController.getExecutionController().getJobsController().getModel().getSelectedJob();
        if (j == null)
            return;

        if (this.model.getParentModel().getTasksModel().getTasks().isEmpty())
            return;

        final String jobId = String.valueOf(j.getId());
        liveOutputJobs.remove(jobId);
        model.setLiveOutput(jobId, false);
    }
    
    
    public void resetPendingOutputTaskRequest(){
        this.taskOutputRequests.clear();
    }
    
    public void cancelOutputRequests(){
        for (Request req : this.taskOutputRequests.values()) {
            req.cancel();
        }
    }


    public OutputModel getModel() {
        return model;
    }


    public SchedulerController getParentController() {
        return parentController;
    }
    
    
    
}
