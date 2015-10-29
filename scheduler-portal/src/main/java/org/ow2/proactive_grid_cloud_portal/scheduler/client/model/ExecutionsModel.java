package org.ow2.proactive_grid_cloud_portal.scheduler.client.model;

import java.util.ArrayList;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.ExecutionDisplayModeListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerModelImpl;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.ExecutionListMode;

public class ExecutionsModel {

    protected ExecutionListMode mode;
    
    protected ArrayList<ExecutionDisplayModeListener> modeListeners;
    
    protected JobsModel jobsModel;
    
    protected TasksCentricModel tasksModel;
    
    protected SchedulerModelImpl parentModel;
    
    public ExecutionsModel(SchedulerModelImpl schedulerModel) {
        this.parentModel = schedulerModel;
        this.modeListeners = new ArrayList<ExecutionDisplayModeListener>();
    }

    public ExecutionListMode getMode() {
        return mode;
    }

    public void setMode(ExecutionListMode mode) {
        this.mode = mode;
        for(ExecutionDisplayModeListener listener: this.modeListeners){
            listener.modeSwitched(mode);
        }
    }
    
    public void addExecutionsDisplayModeListener(ExecutionDisplayModeListener listener){
        this.modeListeners.add(listener);
    }

    public JobsModel getJobsModel() {
        return jobsModel;
    }

    public void setJobsModel(JobsModel jobsModel) {
        this.jobsModel = jobsModel;
    }

    public TasksCentricModel getTasksModel() {
        return tasksModel;
    }

    public void setTasksModel(TasksCentricModel tasksModel) {
        this.tasksModel = tasksModel;
    }

    public SchedulerModelImpl getParentModel() {
        return parentModel;
    }
}
