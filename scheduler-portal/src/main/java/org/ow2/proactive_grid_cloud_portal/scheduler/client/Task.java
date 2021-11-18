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

import java.io.Serializable;
import java.util.Date;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.json.SchedulerJSONUtils;

import com.google.gwt.json.client.JSONObject;
import com.google.gwt.json.client.JSONString;
import com.google.gwt.json.client.JSONValue;


/**
 * A representation for the business object corresponding to a Task.
 * @author ahagea
 *
 */
@SuppressWarnings("serial")
public class Task implements Serializable, Comparable<Task> {

    private long id;

    private String name;

    private String hostName;

    private TaskStatus status;

    private long startTime;

    private long inErrorTime;

    private long scheduledTime;

    private long finishTime;

    private long executionDuration;

    private long startAtTime;

    private String description;

    private String tag;

    private int maxNumberOfExec;

    private int numberOfExecLeft;

    private int maxNumberOfExecOnFailure;

    private int numberOfExecOnFailureLeft;

    private boolean visualizationActivated = false;

    private String visualizationConnectionString;

    private int nodeCount;

    private long jobId = 0;

    private String jobName = "a job";

    /**
     * The constructor that has no arguments required by the Serializable interface
     */
    public Task() {
    }

    /**
     * The constructor of the class that is used for creating new instances of the class.
     * @param id the task id
     * @param name the task name
     * @param status the task status
     * @param hostName the last execution HostName of the task
     * @param startTime the start time of the task
     * @param finishedTime the finished time of the task
     * @param executionDuration the duration of the task execution in milliseconds
     * @param description task description.
     * @param nodeCount number of nodes used by the task
     * @param maxNumberOfExec maximum number of executions
     * @param numberOfExecLeft number of executions left
     * @param maxNumberOfExecOnFailure maximum number of executions on failure
     * @param numberOfExecOnFailureLeft maximum number of executions on failure left
     * @param visualizationActivated is remote visualization activated
     * @param visualizationConnectionString visualization connection string
     */
    public Task(long id, String name, TaskStatus status, String hostName, long startTime, long inErrorTime,
            long finishedTime, long scheduledTime, long executionDuration, String description, int nodeCount,
            int maxNumberOfExec, int numberOfExecLeft, int maxNumberOfExecOnFailure, int numberOfExecOnFailureLeft,
            boolean visualizationActivated, String visualizationConnectionString) {

        this.id = id;
        this.name = name;
        this.status = status;
        this.hostName = hostName;
        this.startTime = startTime;
        this.inErrorTime = inErrorTime;
        this.scheduledTime = scheduledTime;
        this.finishTime = finishedTime;
        this.executionDuration = executionDuration;
        this.startAtTime = -1L;
        this.description = description;
        this.nodeCount = nodeCount;
        this.maxNumberOfExec = maxNumberOfExec;
        this.numberOfExecLeft = numberOfExecLeft;
        this.maxNumberOfExecOnFailure = maxNumberOfExecOnFailure;
        this.numberOfExecOnFailureLeft = numberOfExecOnFailureLeft;
        this.visualizationActivated = visualizationActivated;
        this.visualizationConnectionString = visualizationConnectionString;
    }

    /**
     * Setter of the task id.
     * @param id the id to set
     */
    public void setId(long id) {
        this.id = id;
    }

    /**
     * Getter of the task id.
     * @return the id
     */
    public Long getId() {
        return id;
    }

    /**
     * Setter of the task name.
     * @param name the name to set
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * Getter of the task name.
     * @return the name
     */
    public String getName() {
        return name;
    }

    /**
     * Setter of the host name of the task.
     * @param hostName the hostName to set
     */
    public void setHostName(String hostName) {
        this.hostName = hostName;
    }

    /**
     * Getter of the hostname of the task.
     * @return the hostName
     */
    public String getHostName() {
        return hostName;
    }

    /**
     * Setter of the status of the task.
     * @param status the status to set
     */
    public void setStatus(TaskStatus status) {
        this.status = status;
    }

    /**
     * Getter of the status of the task.
     * @return the status
     */
    public TaskStatus getStatus() {
        return status;
    }

    /**
     * Setter of the start time of the task.
     * @param startTime the startTime to set
     */
    public void setStartTime(long startTime) {
        this.startTime = startTime;
    }

    /**
     * Getter of the start time of the task.
     * @return the startTime
     */
    public long getStartTime() {
        return startTime;
    }

    /**
     * Setter of the finished time of the task.
     * @param finishedTime the finishTime to set
     */
    public void setFinishTime(long finishedTime) {
        this.finishTime = finishedTime;
    }

    /**
     * Getter of the finished time of the task.
     * @return the finishTime
     */
    public long getFinishTime() {
        return finishTime;
    }

    /**
     * Setter of the execution time of the task.
     * @param executionDuration the executionTime to set
     */
    public void setExecutionTime(long executionDuration) {
        this.executionDuration = executionDuration;
    }

    /**
     * Getter of the execution time of the task.
     * @return the executionTime
     */
    public long getExecutionTime() {
        return executionDuration;
    }

    /**
     * Setter of the start_at of the task.
     * @param startAtTime the next starting time to set
     */
    public void setStartAtTime(long startAtTime) {
        this.startAtTime = startAtTime;
    }

    /**
     * Getter of the start_at time of the task.
     * @return the next starting time
     */
    public long getStartAtTime() {
        return startAtTime;
    }

    public long getInErrorTime() {
        return inErrorTime;
    }

    public void setInErrorTime(long inErrorTime) {
        this.inErrorTime = inErrorTime;
    }

    public long getScheduledTime() {
        return scheduledTime;
    }

    public void setScheduledTime(long scheduledTime) {
        this.scheduledTime = scheduledTime;
    }

    public int getMaxNumberOfExec() {
        return maxNumberOfExec;
    }

    public int getNumberOfExecLeft() {
        return numberOfExecLeft;
    }

    public int getMaxNumberOfExecOnFailure() {
        return maxNumberOfExecOnFailure;
    }

    public int getNumberOfExecOnFailureLeft() {
        return numberOfExecOnFailureLeft;
    }

    public boolean isVisualizationActivated() {
        return visualizationActivated;
    }

    public void setVisualizationActivated(boolean visualizationActivated) {
        this.visualizationActivated = visualizationActivated;
    }

    public String getVisualizationConnectionString() {
        return visualizationConnectionString;
    }

    public void setVisualizationConnectionString(String visualizationConnectionString) {
        this.visualizationConnectionString = visualizationConnectionString;
    }

    public int getNodeCount() {
        return this.nodeCount;
    }

    /**
     * Setter for the task description.
     * @param description the description to set
     */
    public void setDescription(String description) {
        this.description = description;
    }

    /**
     * Getter of the task description.
     * @return the description
     */
    public String getDescription() {
        return description;
    }

    /**
     * Getter of the task tag.
     * @return the tag of the task.
     */
    public String getTag() {
        return tag;
    }

    /**
     * Setter of the task tag.
     * @param tag the tag of the task.
     */
    public void setTag(String tag) {
        this.tag = tag;
    }

    public String getIdName() {
        return "id";
    }

    public long getJobId() {
        return jobId;
    }

    public void setJobId(long jobId) {
        this.jobId = jobId;
    }

    public String getJobName() {
        return jobName;
    }

    public void setJobName(String jobName) {
        this.jobName = jobName;
    }

    public String toString() {
        return "[ id=" + id + "; " + "name=" + name + "; " + "status=" + status + "; " + "hostName=" + hostName + "; " +
               "startTime=" + new Date(startTime) + "; " + "finishTime=" + new Date(finishTime) + "; " +
               "executionDuration=" + executionDuration + "; " + "description=" + description + "]";
    }

    public boolean equals(Object o) {
        return o instanceof Task && this.id == ((Task) o).getId() && this.jobId == ((Task) o).getJobId();
    }

    @Override
    public int hashCode() {
        return (int) (id ^ (id >>> 32));
    }

    public int compareTo(Task task) {
        return ((Long) this.id).compareTo(task.getId());
    }

    /**
     * @param jsonTask the JSON representation of a Task
     * @return the POJO equivalent
     */
    public static Task parseJson(JSONObject jsonTask) {
        String name = SchedulerJSONUtils.getStringOrDefault(jsonTask.get("name"));
        JSONObject taskInfo = jsonTask.get("taskInfo").isObject();
        String hostName = "";
        if (taskInfo.containsKey("executionHostName")) {
            JSONString host = taskInfo.get("executionHostName").isString();
            if (host != null) {
                hostName = host.stringValue();
            }
        }
        long id = (long) taskInfo.get("taskId").isObject().get("id").isNumber().doubleValue();
        String status = taskInfo.get("taskStatus").isString().stringValue();
        TaskStatus taskStatus = TaskStatus.valueOf(status);
        long startTime = (long) taskInfo.get("startTime").isNumber().doubleValue();
        long inErrorTime = (long) taskInfo.get("inErrorTime").isNumber().doubleValue();
        long finishedTime = (long) taskInfo.get("finishedTime").isNumber().doubleValue();
        long executionDuration = (long) taskInfo.get("executionDuration").isNumber().doubleValue();

        JSONObject jobIdInfo = taskInfo.get("jobId").isObject();

        long jobId = (long) jobIdInfo.get("id").isNumber().doubleValue();
        String jobName = SchedulerJSONUtils.getStringOrDefault(jobIdInfo.get("readableName"));

        String description = "";
        if (jsonTask.containsKey("description")) {
            JSONString desc = jsonTask.get("description").isString();
            if (desc != null)
                description = desc.stringValue();
        }
        String tag = "";
        JSONValue tagValue = jsonTask.get("tag");
        if (tagValue != null && tagValue instanceof JSONString) {
            tag = ((JSONString) tagValue).stringValue();
        }
        int maxExec = (int) jsonTask.get("maxNumberOfExecution").isNumber().doubleValue();
        int execLeft = (int) taskInfo.get("numberOfExecutionLeft").isNumber().doubleValue();
        int execOnFailureLeft = (int) taskInfo.get("numberOfExecutionOnFailureLeft").isNumber().doubleValue();
        int maxExecOnFailure = (int) jsonTask.get("maxNumberOfExecutionOnFailure").isNumber().doubleValue();
        boolean visualizationActivated = false;
        String visualizationConnectionString = "";
        if (taskInfo.containsKey("visualizationActivated") &&
            taskInfo.get("visualizationActivated").isBoolean() != null) {
            visualizationActivated = (boolean) taskInfo.get("visualizationActivated").isBoolean().booleanValue();
        }
        if (taskInfo.containsKey("visualizationConnectionString") &&
            taskInfo.get("visualizationConnectionString").isString() != null) {
            visualizationConnectionString = (String) taskInfo.get("visualizationConnectionString")
                                                             .isString()
                                                             .stringValue();
        }

        int nodes = 1;
        if (jsonTask.containsKey("parallelEnvironment")) {
            JSONObject parEnv = jsonTask.get("parallelEnvironment").isObject();
            if (parEnv != null && parEnv.containsKey("nodesNumber")) {
                nodes = (int) parEnv.get("nodesNumber").isNumber().doubleValue();
            }
        }
        long scheduledTime = (long) taskInfo.get("scheduledTime").isNumber().doubleValue();

        Task result = new Task(id,
                               name,
                               taskStatus,
                               hostName,
                               startTime,
                               inErrorTime,
                               finishedTime,
                               scheduledTime,
                               executionDuration,
                               description,
                               nodes,
                               maxExec,
                               execLeft,
                               maxExecOnFailure,
                               execOnFailureLeft,
                               visualizationActivated,
                               visualizationConnectionString);
        result.setTag(tag);
        result.setJobId(jobId);
        result.setJobName(jobName);

        result.setStartAtTime(scheduledTime);
        return result;
    }

}
