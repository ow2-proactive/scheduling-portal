package org.ow2.proactive_grid_cloud_portal.scheduler.client.controller;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerController;
import org.ow2.proactive_grid_cloud_portal.scheduler.shared.SchedulerConfig;

public class JobsPaginationController extends PaginationController{

	public JobsPaginationController(SchedulerController schedulerController) {
		super(schedulerController, SchedulerConfig.JOBS_PAGE_SIZE);
		this.model = this.schedulerController.getModel().getJobsPaginationModel();
	}
	
	
	@Override
	public void fetch() {
		this.schedulerController.fetchJobs();
	}
}
