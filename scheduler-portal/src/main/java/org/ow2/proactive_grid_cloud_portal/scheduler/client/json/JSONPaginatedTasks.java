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

package org.ow2.proactive_grid_cloud_portal.scheduler.client.json;

import java.util.List;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.Task;

/**
 * A wrapper around a paginated list of tasks, and the total number of tasks.
 */
public class JSONPaginatedTasks{
    /**
     * The page of tasks.
     */
    private List<Task> tasks;
    
    /**
     * Total number of tasks without pagination.
     */
    private long totalTasks;
    
    /**
     * Builds a wrapper around a paginated list of tasks.
     * @param tasks
     * @param totalTasks
     */
    public JSONPaginatedTasks(List<Task> tasks, long totalTasks) {
        super();
        this.tasks = tasks;
        this.totalTasks = totalTasks;
    }
    
    
    /**
     * Gets the page of tasks.
     * @return the page of tasks.
     */
    public List<Task> getTasks() {
        return tasks;
    }
    
    /**
     * Gets the total number of tasks without pagination.
     * @return the total number of tasks without pagination.
     */
    public long getTotalTasks() {
        return totalTasks;
    }
}