package org.ow2.proactive_grid_cloud_portal.scheduler.client.model;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerModelImpl;

public class ResultModel{
   
    protected SchedulerModelImpl parentModel;
    
    public ResultModel(SchedulerModelImpl parentModel){
        this.parentModel = parentModel;
        this.parentModel.setResultModel(this);
    }
    
}
