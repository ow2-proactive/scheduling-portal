package org.ow2.proactive_grid_cloud_portal.scheduler.client.model;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerModelImpl;

public class TasksCentricModel extends TasksModel{

    public TasksCentricModel(SchedulerModelImpl parentModel) {
        super(parentModel);
    }

    @Override
    protected void initNavigationModel() {
        this.tasksNavigationModel = new TasksCentricNavigationModel(this);
    }
    
}
