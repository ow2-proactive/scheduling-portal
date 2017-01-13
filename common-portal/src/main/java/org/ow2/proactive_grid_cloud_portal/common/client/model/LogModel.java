/*
 * ProActive Parallel Suite(TM):
 * The Open Source library for parallel and distributed
 * Workflows & Scheduling, Orchestration, Cloud Automation
 * and Big Data Analysis on Enterprise Grids & Clouds.
 *
 * Copyright (c) 2007 - 2017 ActiveEon
 * Contact: contact@activeeon.com
 *
 * This library is free software: you can redistribute it and/or
 * modify it under the terms of the GNU Affero General Public License
 * as published by the Free Software Foundation: version 3 of
 * the License.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 *
 * If needed, contact us to obtain a release under GPL Version 2 or 3
 * or a different license than the AGPL.
 */
package org.ow2.proactive_grid_cloud_portal.common.client.model;

import java.util.ArrayList;
import java.util.Date;

import org.ow2.proactive_grid_cloud_portal.common.client.Listeners.LogListener;
import org.ow2.proactive_grid_cloud_portal.common.client.model.dispatcher.LogEventDispatcher;

import com.google.gwt.i18n.client.DateTimeFormat;
import com.google.gwt.i18n.client.DateTimeFormat.PredefinedFormat;


public class LogModel implements LogEventDispatcher {

    private ArrayList<LogListener> logListeners = null;

    protected LogModel() {
        this.logListeners = new ArrayList<LogListener>();
    }

    private static LogModel instance = null;

    public static LogModel getInstance() {
        if (instance == null) {
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
