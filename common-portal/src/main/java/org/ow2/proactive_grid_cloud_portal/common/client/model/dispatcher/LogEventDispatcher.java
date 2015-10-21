package org.ow2.proactive_grid_cloud_portal.common.client.model.dispatcher;

import org.ow2.proactive_grid_cloud_portal.common.client.Listeners.LogListener;

public interface LogEventDispatcher {

    /**
     * Log Listeners will be notified when an event is logged
     * 
     * @param listener a new listener to register
     */
    public void addLogListener(LogListener listener);
}
