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
package org.ow2.proactive_grid_cloud_portal.scheduler.client.json;

import java.util.List;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.Task;


/**
 * A wrapper around a paginated list of tasks, and the total number of tasks.
 */
public class JSONPaginatedTasks {
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
