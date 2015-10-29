package org.ow2.proactive_grid_cloud_portal.scheduler.client.controller;

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

}
