package org.ow2.proactive_grid_cloud_portal.scheduler.client.controller;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerController;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerModelImpl;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.TasksCentricModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.FilteringTasksView;

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
        this.view = new FilteringTasksView(this);
        return this.view.build();
    }

    
}
