/*
 *  *
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
 *  * $$PROACTIVE_INITIAL_DEV$$
 */
package org.ow2.proactive_grid_cloud_portal.scheduler.client;

import com.google.gwt.user.client.rpc.IsSerializable;

public class TaskUsage implements IsSerializable {
    private String taskId;
    private String taskName;
    private long taskStartTime = -1;
    private long taskFinishedTime = -1;
    private long taskExecutionDuration;
    private long taskNodeNumber;

    public TaskUsage(String taskId, String taskName,
                     long taskStartTime, long taskFinishedTime,
                     long taskExecutionDuration, long taskNodeNumber) {
        this.taskId = taskId;
        this.taskName = taskName;
        this.taskStartTime = taskStartTime;
        this.taskFinishedTime = taskFinishedTime;
        this.taskExecutionDuration = taskExecutionDuration;
        this.taskNodeNumber = taskNodeNumber;
    }

    public TaskUsage() {
    }

    public String getTaskId() {
        return taskId;
    }

    public String getTaskName() {
        return taskName;
    }

    public long getTaskStartTime() {
        return taskStartTime;
    }

    public long getTaskFinishedTime() {
        return taskFinishedTime;
    }

    public long getTaskExecutionDuration() {
        return taskExecutionDuration;
    }

    public long getTaskNodeNumber() {
        return taskNodeNumber;
    }

    public String toCsv(String separator) {
        StringBuilder sb = new StringBuilder();
        sb.append(taskId).append(separator);
        sb.append(taskName).append(separator);
        sb.append(taskNodeNumber).append(separator);
        sb.append(taskStartTime).append(separator);
        sb.append(taskFinishedTime).append(separator);
        sb.append(taskExecutionDuration);
        return sb.toString();
    }

    public static String toCsvHeader(String separator) {
        StringBuilder sb = new StringBuilder();
        sb.append("Task Id").append(separator);
        sb.append("Task Name").append(separator);
        sb.append("Task Node Number").append(separator);
        sb.append("Task Start Time").append(separator);
        sb.append("Task Finished Time").append(separator);
        sb.append("Task Duration");
        return sb.toString();
    }
}
