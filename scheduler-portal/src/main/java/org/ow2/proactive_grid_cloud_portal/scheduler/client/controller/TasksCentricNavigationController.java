package org.ow2.proactive_grid_cloud_portal.scheduler.client.controller;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.TasksCentricNavigationModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.TasksCentricNavigationView;

import com.smartgwt.client.widgets.layout.Layout;

public class TasksCentricNavigationController extends TasksNavigationController{

    public TasksCentricNavigationController(TasksCentricController parentController) {
        super(parentController);
    }
    
    public Layout buildView(){
        this.view = new TasksCentricNavigationView(this);
        return this.view.build();
    }
    
    
    public void changeFromDate(long fromDate){
        TasksCentricNavigationModel navigationModel = (TasksCentricNavigationModel) this.model;
        if(fromDate != navigationModel.getFromDate()){
            navigationModel.setFromDate(fromDate);
            this.parentController.resetPendingTasksRequests();
            this.paginationController.firstPage();
        }
    }
    
    
    public void changeToDate(long toDate){
        TasksCentricNavigationModel navigationModel = (TasksCentricNavigationModel) this.model;
        if(toDate != navigationModel.getToDate()){
            navigationModel.setToDate(toDate);
            this.parentController.resetPendingTasksRequests();
            this.paginationController.firstPage();
        }
    }

}
