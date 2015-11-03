package org.ow2.proactive_grid_cloud_portal.scheduler.client.model;

public class TasksCentricNavigationModel extends TasksNavigationModel{
    
    protected long fromDate;
    
    protected long toDate;
    
    public TasksCentricNavigationModel(TasksModel parentModel) {
        super(parentModel);
    }

    public long getFromDate() {
        return fromDate;
    }

    public void setFromDate(long fromDate) {
        this.fromDate = fromDate;
    }

    public long getToDate() {
        return toDate;
    }

    public void setToDate(long toDate) {
        this.toDate = toDate;
    }
    

}
