package org.ow2.proactive_grid_cloud_portal.scheduler.client.controller;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerController;
import org.ow2.proactive_grid_cloud_portal.scheduler.shared.SchedulerConfig;

public class TasksPaginationController extends PaginationController{

	public TasksPaginationController(SchedulerController schedulerController) {
		super(schedulerController, SchedulerConfig.TASKS_PAGE_SIZE);
		this.model = this.schedulerController.getModel().getTasksPaginationModel();
	}
	
	@Override
	public void fetch() {
		this.schedulerController.updateTasks();
	}
}
