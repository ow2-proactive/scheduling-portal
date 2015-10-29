package org.ow2.proactive_grid_cloud_portal.scheduler.client.controller;

public enum ExecutionListMode {
    JOB_CENTRIC("Jobs-centric"),
    TASK_CENTRIC("Tasks-centric");
    
    public final String name;
    
    private ExecutionListMode(String name){
        this.name = name;
    }
}
