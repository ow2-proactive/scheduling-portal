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
package org.ow2.proactive_grid_cloud_portal.scheduler.client.model;

import java.util.ArrayList;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.ServerLogsListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerModelImpl;

import com.smartgwt.client.util.StringUtil;


public class ServerLogsModel extends AbstractSelectedTargetModel {

    protected String logs;

    protected ArrayList<ServerLogsListener> logsListeners;

    public ServerLogsModel(SchedulerModelImpl parentModel) {
        super(parentModel);
        this.parentModel.setServerLogsModel(this);
        this.logsListeners = new ArrayList<ServerLogsListener>();
    }

    public void notifyLogsListeners(String jobId) {
        for (ServerLogsListener listener : this.logsListeners) {
            listener.logsUpdated(this.logs, jobId);
        }
    }

    public void setLogs(String logs, String jobId) {
        this.logs = "<pre>" + StringUtil.asHTML(logs) + "</pre>";
        this.notifyLogsListeners(jobId);
    }

    public void addServerlogsListener(ServerLogsListener listener) {
        this.logsListeners.add(listener);
    }

    public void resetLogs(String jobId) {
        this.logs = null;
        this.notifyLogsListeners(jobId);
    }
}
