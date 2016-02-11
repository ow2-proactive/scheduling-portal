package org.ow2.proactive_grid_cloud_portal.scheduler.client.model;

import java.util.ArrayList;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.ServerLogsListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerModelImpl;

import com.smartgwt.client.util.StringUtil;

public class ServerLogsModel extends AbstractSelectedTargetModel{

    protected String logs;
    
    protected ArrayList<ServerLogsListener> logsListeners;
    
    
    public ServerLogsModel(SchedulerModelImpl parentModel) {
        super(parentModel);
        this.parentModel.setServerLogsModel(this);
        this.logsListeners = new ArrayList<ServerLogsListener>();
    }

    
    public void notifyLogsListeners(String jobId){
        for(ServerLogsListener listener: this.logsListeners){
            listener.logsUpdated(this.logs, jobId);
        }
    }
    
    
    public void setLogs(String logs, String jobId){
        this.logs = "<pre>" + StringUtil.asHTML(logs) + "</pre>";
        this.notifyLogsListeners(jobId);
    }
    
    
    public void addServerlogsListener(ServerLogsListener listener){
        this.logsListeners.add(listener);
    }
    
    
    public void resetLogs(String jobId){
        this.logs = null;
        this.notifyLogsListeners(jobId);
    }
}
