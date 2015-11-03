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
    
    
    /**
     * True if the portal fetches my executions only.
     */
    protected boolean fetchMyExecutionsOnly = false;
    
    /**
     * True if the portal fetches the pending executions. 
     */
    protected boolean fetchPending = true;
    
    /**
     * True if the portal fetches the running executions. 
     */
    protected boolean fetchRunning = true;
    
    /**
     * True if the portal fetches the finished executions. 
     */
    protected boolean fetchFinished = true;
    
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
    
    /**
     * @return true if the model should only store the executions of the current user
     */
    public boolean isFetchMyExecutionsOnly() {
        return this.fetchMyExecutionsOnly;
    }

    /**
     * Sets if the portal should fetch my jobs only.
     * @param b true if the portal should fetch my jobs only.
     */
    public void fetchMyExecutionsOnly(boolean b) {
        this.fetchMyExecutionsOnly = b;
    }

    /**
     * @return true if the model should store pending jobs
     */
    public boolean isFetchPendingExecutions() {
        return this.fetchPending;
    }

    /**
     * Sets if the portal should fetch pending jobs.
     * @param b true if the portal should fetch pending jobs.
     */
    public void fetchPending(boolean f) {
        this.fetchPending = f;
    }

    /**
     * @return true if the model should store running jobs
     */
    public boolean isFetchRunningExecutions() {
        return this.fetchRunning;
    }

    /**
     * Sets if the portal should fetch running jobs.
     * @param b true if the portal should fetch running jobs.
     */
    public void fetchRunning(boolean f) {
        this.fetchRunning = f;
    }

    /**
     * @return true if the model should store finished jobs
     */
    public boolean isFetchFinishedExecutions() {
        return this.fetchFinished;
    }

    /**
     * Sets if the portal should fetch finished jobs.
     * @param b true if the portal should fetch finished jobs.
     */
    public void fetchFinished(boolean f) {
        this.fetchFinished = f;
    }
}
