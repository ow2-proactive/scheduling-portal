package org.ow2.proactive_grid_cloud_portal.scheduler.client;

public class Scheduler {

    protected static SchedulerServiceAsync schedulerService;
    
    public static void setSchedulerService(SchedulerServiceAsync service){
        schedulerService = service;
    }
    
    public static SchedulerServiceAsync getSchedulerService(){
        return schedulerService;
    }
}
