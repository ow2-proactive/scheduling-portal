package org.ow2.proactive_grid_cloud_portal.scheduler.client.model;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.DownloadOption;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerModelImpl;

public class ResultModel{

    protected DownloadOption downloadOption = DownloadOption.OPT_TEXT;
    
   
    protected SchedulerModelImpl parentModel;
    
    public ResultModel(SchedulerModelImpl parentModel){
        this.parentModel = parentModel;
        this.parentModel.setResultModel(this);
    }
    
    public DownloadOption getDownloadOption() {
        return downloadOption;
    }


    public void setDownloadOption(DownloadOption downloadOption) {
        this.downloadOption = downloadOption;
    }
    
}
