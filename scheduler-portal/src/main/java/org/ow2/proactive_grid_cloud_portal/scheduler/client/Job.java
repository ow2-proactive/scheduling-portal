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
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.json.SchedulerJSONUtils;

import com.google.common.collect.ImmutableMap;
import com.google.gwt.json.client.JSONObject;


/**
 * Representation of the business object that represents a job.
 *
 * @author ahagea
 */
@SuppressWarnings("serial")
public class Job implements Serializable, Comparable<Job> {

    private int id;

    private String name;

    private String projectName;

    private JobStatus status;

    private JobPriority priority;

    private String user;

    private String description;

    private int pendingTasks;

    private int runningTasks;

    private int finishedTasks;

    private int totalTasks;

    private int failedTasks;

    private int faultyTasks;

    private int inErrorTasks;

    private long submitTime;

    private long startTime;

    private long inErrorTime;

    private long finishTime;

    private final ImmutableMap<String, String> genericInformation;

    private final Map<String, String> variables;

    private final Map<String, Map<String, String>> detailedVariables;

    private final ImmutableMap<String, String> resultMap;

    /**
     * The constructor that has no arguments required by the Serializable interface
     */
    public Job() {
        this.genericInformation = ImmutableMap.of();
        this.variables = ImmutableMap.of();
        this.detailedVariables = ImmutableMap.of();
        this.resultMap = ImmutableMap.of();
    }

    /**
     * Creates a new instance of Job
     *
     * @param id Job id
     */
    public Job(int id) {
        this();
        this.id = id;
    }

    /**
     * Creates a new instance of the Job class.
     * @param id the job ID
     * @param name the job name
     * @param name the project
     * @param status the job status
     * @param priority the job priority
     * @param user the username of the user that submitted the job
     * @param genericInformation the job generic information
     * @param pending number of pending tasks
     * @param running number of running tasks
     * @param finished number of finished tasks
     * @param total total number of tasks
     * @param failed number of failed tasks
     * @param faulty number of faulty tasks
     * @param inError number of in error tasks
     * @param submitTime submission time
     * @param startTime start time
     * @param inErrorTime in error time
     * @param finishTime finish time
     * @param description job description
     */
    public Job(int id, String name, String projectName, JobStatus status, JobPriority priority, String user,
            Map<String, String> genericInformation, Map<String, String> variables,
            Map<String, Map<String, String>> detailedVariables, Map<String, String> resultMap, int pending, int running,
            int finished, int total, int failed, int faulty, int inError, long submitTime, long startTime,
            long inErrorTime, long finishTime, String description) {
        this.id = id;
        this.name = name;
        this.projectName = projectName;
        this.setStatus(status);
        this.setPriority(priority);
        this.setUser(user);
        this.pendingTasks = pending;
        this.runningTasks = running;
        this.finishedTasks = finished;
        this.totalTasks = total;

        this.failedTasks = failed;
        this.faultyTasks = faulty;
        this.inErrorTasks = inError;

        this.submitTime = submitTime;
        this.startTime = startTime;
        this.inErrorTime = inErrorTime;
        this.finishTime = finishTime;
        this.genericInformation = ImmutableMap.copyOf(genericInformation);
        this.variables = variables;
        this.detailedVariables = detailedVariables;
        this.variables.replaceAll((key, value) -> {
            return value.toString().matches("ENC((.*))") ? "*******" : value;
        });
        this.resultMap = ImmutableMap.copyOf(resultMap);
        this.description = description;
    }

    /**
     * Setter of the job ID.
     * @param id the new ID that will be set.
     */
    public void setID(int id) {
        this.id = id;
    }

    /**
     * Getter of the job description.
     * @return the description
     */
    public String getDescription() {
        return description;
    }

    /**
     * Getter of the job ID.
     * @return the ID of the job.
     */
    public Integer getId() {
        return id;
    }

    /**
     * Setter of the job name.
     * @param name the name of the job.
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * Getter for the name of the job.
     * @return the username.
     */
    public String getName() {
        return name;
    }

    /**
     * Setter of the job projectName.
     * @param projectName the project of the job.
     */
    public void setProjectName(String projectName) {
        this.projectName = projectName;
    }

    /**
     * Getter for the projectName of the job.
     * @return the username.
     */
    public String getProjectName() {
        return projectName;
    }

    /**
     * Setter for the status of the job.
     * @param status The status of the job.
     */
    public void setStatus(JobStatus status) {
        this.status = status;
    }

    /**
     * Getter of the job status.
     * @return the status of the job.
     */
    public JobStatus getStatus() {
        return status;
    }

    /**
     * Setter of the priority of the job.
     * @param priority the priority of the job that will be set.
     */
    public void setPriority(JobPriority priority) {
        this.priority = priority;
    }

    /**
     * Getter of the job priority.
     * @return the priority of the job.
     */
    public JobPriority getPriority() {
        return priority;
    }

    /**
     * Setter of the user that submitted the job.
     * @param user the user that submitted the job.
     */
    public void setUser(String user) {
        this.user = user;
    }

    /**
     * Getter of user that submitted the job.
     * @return the username.
     */
    public String getUser() {
        return user;
    }

    /**
     * @return number of currently running tasks in this job
     */
    public int getRunningTasks() {
        return this.runningTasks;
    }

    /**
     * @return number of currently pending tasks in this job
     */
    public int getPendingTasks() {
        return this.pendingTasks;
    }

    /**
     * @return number of currently finished tasks in this job
     */
    public int getFinishedTasks() {
        return this.finishedTasks;
    }

    public int getFailedTasks() {
        return failedTasks;
    }

    public int getFaultyTasks() {
        return faultyTasks;
    }

    public int getInErrorTasks() {
        return inErrorTasks;
    }

    /**
     * @return time at which the job was submitted
     */
    public long getSubmitTime() {
        return submitTime;
    }

    /**
     * @return time at which the job was started
     */
    public long getStartTime() {
        return startTime;
    }

    /**
     * @return time at which the job was started
     */
    public long getInErrorTime() {
        return inErrorTime;
    }

    /**
     * @return time at which the job has finished
     */
    public long getFinishTime() {
        return finishTime;
    }

    /**
     * @return total number of tasks in this job
     */
    public int getTotalTasks() {
        return this.totalTasks;
    }

    @Override
    public boolean equals(Object o) {
        if (!(o instanceof Job))
            return false;

        return this.id == ((Job) o).getId();
    }

    @Override
    public int hashCode() {
        return this.id;
    }

    /**
     * @param job
     * @return Return true if and only if all the job's field
     * are equal to those of <code>this</code>
     */
    public boolean isEqual(Job job) {
        return this.id == job.getId() && this.name.equals(job.getName()) && this.priority.equals(job.getPriority()) &&
               this.status.equals(job.getStatus()) && this.user.equals(job.getUser()) &&
               pendingTasks == job.pendingTasks && runningTasks == job.runningTasks &&
               finishedTasks == job.finishedTasks && failedTasks == job.failedTasks && faultyTasks == job.faultyTasks &&
               inErrorTasks == job.inErrorTasks && finishTime == job.finishTime && inErrorTime == job.inErrorTime;
    }

    public int compareTo(Job job) {
        return ((Integer) this.id).compareTo(job.getId());
    }

    @Override
    public String toString() {
        return "[ id=" + id + "; " + "name=" + name + "; " + "status=" + status + "; " + "priority=" + priority + "; " +
               "user=" + user + "]";
    }

    /**
     * @return true if and only if the job has been executed.
     * In other words, returns true if and only if its status is
     * <code>FINISHED</code>, <code>CANCELED</code>, <code>FAILED</code>
     * or <code>KILLED</code>.
     */
    public boolean isExecuted() {
        return this.status == JobStatus.CANCELED || this.status == JobStatus.FINISHED ||
               this.status == JobStatus.FAILED || this.status == JobStatus.KILLED;
    }

    /**
     * @param jsonJob the JSON representation of a job
     * @return a POJO equivalent
     */
    public static Job parseJson(JSONObject jsonJob) {
        JSONObject jsonInfo = jsonJob.get("node").isObject();
        return parseJSONInfo(jsonInfo);
    }

    public static Job parseJSONInfo(JSONObject jsonJobInfo) {
        String user = SchedulerJSONUtils.getStringOrDefault(jsonJobInfo.get("owner"));
        String priority = SchedulerJSONUtils.getStringOrDefault(jsonJobInfo.get("priority"));
        String status = SchedulerJSONUtils.getStringOrDefault(jsonJobInfo.get("status"));
        int pending = (int) jsonJobInfo.get("numberOfPendingTasks").isNumber().doubleValue();
        int running = (int) jsonJobInfo.get("numberOfRunningTasks").isNumber().doubleValue();
        int finished = (int) jsonJobInfo.get("numberOfFinishedTasks").isNumber().doubleValue();
        int total = (int) jsonJobInfo.get("totalNumberOfTasks").isNumber().doubleValue();
        int failed = (int) jsonJobInfo.get("numberOfFailedTasks").isNumber().doubleValue();
        int faulty = (int) jsonJobInfo.get("numberOfFaultyTasks").isNumber().doubleValue();
        int inError = (int) jsonJobInfo.get("numberOfInErrorTasks").isNumber().doubleValue();
        long submittedTime = (long) jsonJobInfo.get("submittedTime").isNumber().doubleValue();
        long startTime = (long) jsonJobInfo.get("startTime").isNumber().doubleValue();
        long inErrorTime = (long) jsonJobInfo.get("inErrorTime").isNumber().doubleValue();
        long finishedTime = (long) jsonJobInfo.get("finishedTime").isNumber().doubleValue();
        String description = SchedulerJSONUtils.getStringOrDefault(jsonJobInfo.get("description"));

        Map<String, String> genericInformation = SchedulerJSONUtils.extractMap(jsonJobInfo.get("genericInformation"));
        Map<String, String> variables = SchedulerJSONUtils.extractMap(jsonJobInfo.get("variables"));
        Map<String, String> resultMap = SchedulerJSONUtils.extractMap(jsonJobInfo.get("resultMap"));

        String name = SchedulerJSONUtils.getStringOrDefault(jsonJobInfo.get("name"));
        String projectName = SchedulerJSONUtils.getStringOrDefault(jsonJobInfo.get("projectName"));
        int id = Integer.valueOf(jsonJobInfo.get("id").isString().stringValue());

        return new Job(id,
                       name,
                       projectName,
                       JobStatus.valueOf(status),
                       JobPriority.findPriority(priority),
                       user,
                       genericInformation,
                       variables,
                       new HashMap<>(),
                       resultMap,
                       pending,
                       running,
                       finished,
                       total,
                       failed,
                       faulty,
                       inError,
                       submittedTime,
                       startTime,
                       inErrorTime,
                       finishedTime,
                       description);
    }

    public static Map<String, Map<String, String>> parseJSONDetailedVariables(JSONObject jsonJobInfo) {
        Map<String, Map<String, String>> detailedVariablesMap = new LinkedHashMap<>();
        JSONObject detailedVariablesObject = jsonJobInfo.get("detailedVariables").isObject();
        JSONObject variablesObject = jsonJobInfo.get("variables").isObject();
        Set<String> variables = variablesObject.keySet();
        for (String variable : variables) {
            Map<String, String> variableMap = new HashMap<>();
            JSONObject nameObject = detailedVariablesObject.get(variable).isObject();
            variableMap.put("name", SchedulerJSONUtils.getStringOrDefault(nameObject.get("name")));
            variableMap.put("value", SchedulerJSONUtils.getStringOrDefault(nameObject.get("value")));
            variableMap.put("model", SchedulerJSONUtils.getStringOrDefault(nameObject.get("model")));
            variableMap.put("description", SchedulerJSONUtils.getStringOrDefault(nameObject.get("description")));
            variableMap.put("group", SchedulerJSONUtils.getStringOrDefault(nameObject.get("group")));
            variableMap.put("advanced", nameObject.get("advanced").isBoolean().toString());
            variableMap.put("hidden", nameObject.get("hidden").isBoolean().toString());
            detailedVariablesMap.put(variable, variableMap);
        }
        return detailedVariablesMap;
    }

    /**
     * Format a duration in milliseconds to a human readable format.
     *
     * @param durationAsString a long that depicts the duration as String.
     * @return the same duration in a human readable format.
     */
    public static String formatDuration(String durationAsString) {
        return formatDuration(Long.parseLong(durationAsString));
    }

    /**
     * Format a duration in milliseconds to a human readable format.
     *
     * @param millis a duration in milliseconds.
     * @return the same duration in a human readable format.
     */
    public static String formatDuration(long millis) {
        if (millis < 0) {
            return "";
        }

        long secs = millis / 1000;
        long mins = secs / 60;
        long h = mins / 60;

        millis = millis % 1000;
        secs = secs % 60;
        mins = mins % 60;

        String result = millis + "ms";

        if (secs > 0) {
            result = secs + "s " + result;
        }

        if (mins > 0) {
            result = mins + "m " + result;
        }

        if (h > 0) {
            result = h + "h " + result;
        }

        return result;
    }

    public void setRunningTasks(int running) {
        this.runningTasks = running;
    }

    public void setFinishedTasks(int finished) {
        this.finishedTasks = finished;
    }

    public void setPendingTasks(int pending) {
        this.pendingTasks = pending;
    }

    public void setDetailsVariables(Map<String, Map<String, String>> detailedVariables) {
        this.detailedVariables.clear();
        this.detailedVariables.putAll(detailedVariables);
    }

    public void setTotalTasks(int total) {
        this.totalTasks = total;
    }

    public Map<String, String> getGenericInformation() {
        return genericInformation;
    }

    public Map<String, String> getVariables() {
        return variables;
    }

    public Map<String, Map<String, String>> getDetailedVariables() {
        return detailedVariables;
    }

    public Map<String, String> getResultMap() {
        return resultMap;
    }
}
