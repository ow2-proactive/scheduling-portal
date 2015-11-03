package org.ow2.proactive_grid_cloud_portal.scheduler.client.controller;

import org.ow2.proactive_grid_cloud_portal.common.client.json.JSONUtils;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LogModel;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LoginModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Scheduler;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerController;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerModelImpl;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerServiceAsync;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.json.JSONPaginatedTasks;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.json.SchedulerJSONUtils;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.ExecutionsModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.PaginationModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.TasksCentricModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.TasksCentricNavigationModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.TasksCentricView;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.smartgwt.client.widgets.layout.Layout;

public class TasksCentricController extends TasksController{

    
    
    
    public TasksCentricController(SchedulerController parentController) {
        super(parentController);
    }
    
    
    @Override
    public Layout buildView() {
        SchedulerModelImpl schedulerModel = (SchedulerModelImpl) this.parentController.getModel();
        this.model = new TasksCentricModel(schedulerModel);
        this.taskNavigationController = new TasksCentricNavigationController(this);
        this.view = new TasksCentricView(this);
        return this.view.build();
    }

    
    public void tasksStateRevision(){
        TasksCentricNavigationModel navigationModel = (TasksCentricNavigationModel) this.model.getTasksNavigationModel();
        if(navigationModel.getTaskAutoRefreshOption()){
           this.updateTasks(false);
        }
    }
    
    
    /**
     * Updates the current task list depending the current job selection in the model 
     */
    public void updateTasks(boolean showUpdating) {
        if(showUpdating){
            this.model.notifyTasksChanging(false);
        }

        AsyncCallback<String> callback = new AsyncCallback<String>() {

            public void onFailure(Throwable caught) {
                String msg = JSONUtils.getJsonErrorMessage(caught);
                model.taskUpdateError(msg);
                LogModel.getInstance().logImportantMessage("Failed to update tasks for job : " + msg);
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

        TasksCentricNavigationModel navigationModel = (TasksCentricNavigationModel) this.model.getTasksNavigationModel();
        String tagFilter = navigationModel.getCurrentTagFilter();
        long fromDate = navigationModel.getFromDate();
        long toDate  = navigationModel.getToDate(); 
        
        PaginationModel paginationModel = navigationModel.getPaginationModel();
        int offset = paginationModel.getOffset();
        int limit = paginationModel.getRange();
        String sessionId = LoginModel.getInstance().getSessionId();
        SchedulerServiceAsync scheduler = Scheduler.getSchedulerService();
        
        ExecutionsModel executionsModel = this.model.getParentModel().getExecutionsModel();
        boolean myTasksOnly = executionsModel.isFetchMyExecutionsOnly();
        boolean pending = executionsModel.isFetchPendingExecutions();
        boolean running = executionsModel.isFetchRunningExecutions();
        boolean finished = executionsModel.isFetchFinishedExecutions();
        
        
        if(tagFilter.equals("")){
            this.taskUpdateRequest = scheduler.getTaskCentric(sessionId, fromDate, toDate, myTasksOnly, pending, 
                    running, finished, offset, limit, callback);
        }
        else{
            this.taskUpdateRequest = scheduler.getTaskCentricByTag(sessionId, tagFilter, fromDate, toDate, myTasksOnly, pending, 
                    running, finished, offset, limit, callback);
        }
    }
    
    
    
    public TasksPaginationController getPaginationController(){
        return this.taskNavigationController.getPaginationController();
    }
    
}
