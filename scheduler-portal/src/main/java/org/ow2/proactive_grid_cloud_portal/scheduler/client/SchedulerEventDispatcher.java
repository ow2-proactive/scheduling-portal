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
package org.ow2.proactive_grid_cloud_portal.scheduler.client;

import org.ow2.proactive_grid_cloud_portal.common.client.EventDispatcher;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.SchedulerStatusListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.StatisticsListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.UsersListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.VisualizationListener;


/**
 * Registers event listeners, notifies them when something happens
 *
 * @author mschnoor
 *
 */
public interface SchedulerEventDispatcher extends EventDispatcher {

    void addSchedulerStatusListener(SchedulerStatusListener listener);

    void addUsersListener(UsersListener listener);

    void addUsersWithJobsListener(UsersListener listener);

    void addStatisticsListener(StatisticsListener listener);

    void addVisualizationListener(VisualizationListener listener);

    void addUsageListener(SchedulerListeners.UsageListener listener);

    void setThirdPartyCredentialsListener(
            SchedulerListeners.ThirdPartyCredentialsListener thirdPartyCredentialsListener);

}
