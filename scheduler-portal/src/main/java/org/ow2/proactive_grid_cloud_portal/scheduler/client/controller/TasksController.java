package org.ow2.proactive_grid_cloud_portal.scheduler.client.controller;

import java.util.ArrayList;

import org.ow2.proactive_grid_cloud_portal.common.client.json.JSONUtils;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LogModel;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LoginModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Job;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.NoVncUtils;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Scheduler;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerController;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerModelImpl;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerServiceAsync;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Task;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.json.JSONPaginatedTasks;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.json.SchedulerJSONUtils;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.PaginationModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.TasksModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.TasksNavigationModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.AbstractGridItemsView;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.TasksView;

import com.google.gwt.http.client.Request;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.smartgwt.client.widgets.layout.Layout;

public class TasksController {

    /** pending taskUpdate request, or null */
    private Request taskUpdateRequest = null;
    
    protected TasksNavigationController taskNavigationController;
    
    
    protected TasksModel model;
    
    
    protected AbstractGridItemsView view;
    
    
    protected SchedulerController parentController;
    
    
    public TasksController(SchedulerController parentController) {
        this.parentController = parentController;
    }
    
    
    
    
    public SchedulerController getParentController() {
        return parentController;
    }


    public Layout buildView(){
        this.model = new TasksModel((SchedulerModelImpl) this.parentController.getModel());
        this.taskNavigationController = new TasksNavigationController(this);
        this.view = new TasksView(this);
        return this.view.build();
    }
    
    public Layout rebuildView(){
        this.view = new TasksView(this);
        return this.view.build();
    }
    
    
        
    
    public TasksModel getModel() {
        return model;
    }


    
    
    
    

    /**
     * Updates the current task list depending the current job selection in the model 
     */
    public void updateTasks(boolean showUpdating) {
        Job selectedJob = this.model.getParentModel().getExecutionsModel().getJobsModel().getSelectedJob();
        
        if(showUpdating){
            this.model.notifyTasksChanging(selectedJob);
        }
        
        if (selectedJob == null) {
            model.setTasks(new ArrayList<Task>(), 0);
        } else {  
            final String jobId = "" + selectedJob.getId();

            AsyncCallback<String> callback = new AsyncCallback<String>() {

                public void onFailure(Throwable caught) {
                    String msg = JSONUtils.getJsonErrorMessage(caught);

                    model.taskUpdateError(msg);
                    LogModel.getInstance().logImportantMessage("Failed to update tasks for job " +
                            jobId + ": " + msg);
                }

                public void onSuccess(String result) {
                    try {
                        JSONPaginatedTasks tasks = SchedulerJSONUtils.parseJSONPaginatedTasks(result);
                        model.setTasksDirty(false);
                        model.setTasks(tasks.getTasks(), tasks.getTotalTasks());
                        // do not model.logMessage() : this is repeated by a timer
                    } catch (org.ow2.proactive_grid_cloud_portal.common.client.json.JSONException e) {
                        LogModel.getInstance().logCriticalMessage(e.getMessage());
                    }
                }
            };

            TasksNavigationModel navigationModel = this.model.getTasksNavigationModel();
            String tagFilter = navigationModel.getCurrentTagFilter();

            PaginationModel paginationModel = navigationModel.getPaginationModel();
            int offset = paginationModel.getOffset();
            int limit = paginationModel.getRange();
            String sessionId = LoginModel.getInstance().getSessionId();
            SchedulerServiceAsync scheduler = Scheduler.getSchedulerService();
            if(tagFilter.equals("")){
                this.taskUpdateRequest = scheduler.getTasks(sessionId, jobId, offset, limit, callback);
            }
            else{
                this.taskUpdateRequest = scheduler.getTasksByTag(sessionId, jobId, tagFilter, offset, limit, callback);
            }
        }
    }
    
    
    /**
     * Kill a task within a job 
     * @param jobId job id
     * @param taskName task name
     */
    public void killTask(final String taskName) {
        final Integer jobId = this.model.getParentModel().getExecutionsModel().getJobsModel().getSelectedJob().getId();
        String sessionId = LoginModel.getInstance().getSessionId();
        SchedulerServiceAsync scheduler = Scheduler.getSchedulerService();
        scheduler.killTask(sessionId, jobId, taskName, new AsyncCallback<Boolean>() {
            @Override
            public void onFailure(Throwable caught) {
                caught.printStackTrace();

                String msg = JSONUtils.getJsonErrorMessage(caught);
                LogModel.getInstance().logImportantMessage("Failed to kill task: " + msg);
            }

            @Override
            public void onSuccess(Boolean result) {
                LogModel.getInstance().logMessage("Successfully killed task " + taskName + " in job " + jobId);
            }
        });
    }

    /**
     * Restart a task within a job 
     * @param jobId job id
     * @param taskName task name
     */
    public void restartTask(final String taskName) {
        final Integer jobId = this.model.getParentModel().getExecutionsModel().getJobsModel().getSelectedJob().getId();
        String sessionId = LoginModel.getInstance().getSessionId();
        SchedulerServiceAsync scheduler = Scheduler.getSchedulerService();
        scheduler.restartTask(sessionId, jobId, taskName, new AsyncCallback<Boolean>() {
            @Override
            public void onFailure(Throwable caught) {

                caught.printStackTrace();
                String msg = JSONUtils.getJsonErrorMessage(caught);
                LogModel.getInstance().logImportantMessage("Failed to restart task: " + msg);
            }

            @Override
            public void onSuccess(Boolean result) {
                LogModel.getInstance().logMessage("Successfully restarted task " + taskName + " in job " + jobId);
            }
        });
    }

    /**
     * Preempt a task within a job 
     * @param jobId job id
     * @param taskName task name
     */
    public void preemptTask(final String taskName) {
        final Integer jobId = this.model.getParentModel().getExecutionsModel().getJobsModel().getSelectedJob().getId();
        String sessionId = LoginModel.getInstance().getSessionId();
        SchedulerServiceAsync scheduler = Scheduler.getSchedulerService();
        scheduler.preemptTask(sessionId, jobId, taskName, new AsyncCallback<Boolean>() {
            @Override
            public void onFailure(Throwable caught) {
                String msg = JSONUtils.getJsonErrorMessage(caught);
                LogModel.getInstance().logImportantMessage("Failed to preempt task: " + msg);
            }

            @Override
            public void onSuccess(Boolean result) {
                LogModel.getInstance().logMessage("Successfully preempted task " + taskName + " in job " + jobId);
            }
        });
    }
    
    
    public TasksNavigationController getTaskNavigationController() {
        return taskNavigationController;
    }

    public void setTaskNavigationController(
            TasksNavigationController taskNavigationController) {
        this.taskNavigationController = taskNavigationController;
    }
    
    
    public String computeNoVncPageUrl(String taskName){
        String jobId = String.valueOf(model.getParentModel().getExecutionsModel().getJobsModel().getSelectedJob().getId());
        String sessionId = LoginModel.getInstance().getSessionId();
        return NoVncUtils.createNoVncPageUrl(sessionId, jobId, taskName);
    }
    
    

    
    
    public void resetPendingTasksRequests(){
        if (this.taskUpdateRequest != null) {
            this.taskUpdateRequest.cancel();
            this.taskUpdateRequest = null;
        }
    }
    
    public void updatingTasks(){
        this.model.setTasksDirty(true);
    }
}
