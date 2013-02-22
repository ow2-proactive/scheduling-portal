/*
 * ################################################################
 *
 * ProActive Parallel Suite(TM): The Java(TM) library for
 *    Parallel, Distributed, Multi-Core Computing for
 *    Enterprise Grids & Clouds
 *
 * Copyright (C) 1997-2011 INRIA/University of
 *                 Nice-Sophia Antipolis/ActiveEon
 * Contact: proactive@ow2.org or contact@activeeon.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Affero General Public License
 * as published by the Free Software Foundation; version 3 of
 * the License.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
 * USA
 *
 * If needed, contact us to obtain a release under GPL Version 2 or 3
 * or a different license than the AGPL.
 *
 *  Initial developer(s):               The ProActive Team
 *                        http://proactive.inria.fr/team_members.htm
 *  Contributor(s):
 *
 * ################################################################
 * $$PROACTIVE_INITIAL_DEV$$
 */
package org.ow2.proactive_grid_cloud_portal.scheduler.client;

import org.ow2.proactive_grid_cloud_portal.common.client.EventDispatcher;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.JobOutputListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.JobSelectedListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.JobsUpdatedListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.RemoteHintListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.SchedulerStatusListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.StatisticsListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.TasksUpdatedListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.UsersListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.VisualizationListener;


/**
 * Registers event listeners, notifies them when something happens
 *
 *
 * @author mschnoor
 *
 */
public interface SchedulerEventDispatcher extends EventDispatcher {

    /**
     * register a new listener for JobsUpdated events
     *
     * @param listener
     */
    public abstract void addJobsUpdatedListener(JobsUpdatedListener listener);

    /**
     * register a new listener for JobSelected events
     *
     * @param listener
     */
    public abstract void addJobSelectedListener(JobSelectedListener listener);

    /**
     * register a new listener for TasksUpdated events
     * 
     * @param listener
     */
    public abstract void addTasksUpdatedListener(TasksUpdatedListener listener);

    /**
     * register a new listener for SchedulerState events
     * 
     * @param listener
     */
    public abstract void addSchedulerStatusListener(SchedulerStatusListener listener);

    /**
     * Register a new listener for job output events
     * 
     * @param listener
     */
    public abstract void addJobOutputListener(JobOutputListener listener);

    /**
     * register a new listener for scheduler users events
     * 
     * @param listener
     */
    public abstract void addUsersListener(UsersListener listener);

    /**
     * register a new listener for statistics events
     * 
     * @param listener
     */
    public abstract void addStatisticsListener(StatisticsListener listener);

    /**
     * register a new remote hint listener
     * 
     * @param listener
     */
    public abstract void addRemoteHintListener(RemoteHintListener listener);

    /**
     * register a new visualization listener
     * 
     * @param listener
     */
    public abstract void addVisualizationListener(VisualizationListener listener);

    public abstract void addUsageListener(SchedulerListeners.UsageListener listener);
}
