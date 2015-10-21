package org.ow2.proactive_grid_cloud_portal.common.client.model;

import java.util.ArrayList;
import java.util.Date;

import org.ow2.proactive_grid_cloud_portal.common.client.Listeners.LogListener;
import org.ow2.proactive_grid_cloud_portal.common.client.model.dispatcher.LogEventDispatcher;

import com.google.gwt.i18n.client.DateTimeFormat;
import com.google.gwt.i18n.client.DateTimeFormat.PredefinedFormat;

public class LogModel implements LogEventDispatcher{

    private ArrayList<LogListener> logListeners = null;
    
    protected LogModel(){
        this.logListeners = new ArrayList<LogListener>();
    }
    
    
    private static LogModel instance = null;
    
    public static LogModel getInstance(){
        if(instance == null){
            instance = new LogModel();
        }
        return instance;
    }
    
    public void addLogListener(LogListener listener) {
        this.logListeners.add(listener);
    }
    
    
    
    public void logMessage(String message) {
        for (LogListener list : this.logListeners) {
            list.logMessage(getLogStamp() + message);
        }
    }
    
    public void logImportantMessage(String error) {
        for (LogListener list : this.logListeners) {
            list.logImportantMessage(getLogStamp() + "<span style='color:#8f7601;'>" + error + "</span>");
        }
    }

    
    public void logCriticalMessage(String error) {
        for (LogListener list : this.logListeners) {
            list.logCriticalMessage(getLogStamp() + "<span style='color:red;'>" + error + "</span>");
        }
    }

    private String getLogStamp() {
        String date = DateTimeFormat.getFormat(PredefinedFormat.TIME_LONG).format(new Date());
        return "<span style='color:gray'>" + date + "</span> ";
    }
}
