package org.ow2.proactive_grid_cloud_portal.scheduler.client.view;

import java.util.Map;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.Job;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.JobsUpdatedListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.JobsController;

public class JobsPaginationView extends PaginationView implements JobsUpdatedListener{

    public JobsPaginationView(JobsController controller){
        this.paginationController = controller.getPaginationController();
        this.paginationController.getModel().addPaginationListener(this);
        controller.getModel().addJobsUpdatedListener(this);
    }

    @Override
    public void jobsUpdated(Map<Integer, Job> jobs) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void jobsUpdating() {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void jobSubmitted(Job j) {
        // TODO Auto-generated method stub
        
    }
    
    
}
