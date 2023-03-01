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

import static org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.JobsController.PREFIX_SIGNAL_READY;

import java.io.Serializable;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.json.SchedulerJSONUtils;

import com.google.common.collect.ImmutableMap;
import com.google.gwt.json.client.JSONObject;
import com.google.gwt.json.client.JSONValue;


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

    private String bucketName;

    private String submissionMode;

    private JobStatus status;

    private JobPriority priority;

    private String user;

    private String tenant;

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

    private long cumulatedCoreTime;

    private long parentId;

    private int childrenCount;

    private int numberOfNodes;

    private int numberOfNodesInParallel;

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

    public Job(JobBuilder builder) {
        this.id = builder.getId();
        this.name = builder.getName();
        this.projectName = builder.getProjectName();
        this.bucketName = builder.getBucketName();
        this.submissionMode = builder.getSubmissionMode();
        this.setStatus(builder.getStatus());
        this.setPriority(builder.getPriority());
        this.setUser(builder.getUser());
        this.setTenant(builder.getTenant());
        this.pendingTasks = builder.getPendingTasks();
        this.runningTasks = builder.getRunningTasks();
        this.finishedTasks = builder.getFinishedTasks();
        this.totalTasks = builder.getTotalTasks();

        this.failedTasks = builder.getFailedTasks();
        this.faultyTasks = builder.getFaultyTasks();
        this.inErrorTasks = builder.getInErrorTasks();

        this.submitTime = builder.getSubmitTime();
        this.startTime = builder.getStartTime();
        this.inErrorTime = builder.getInErrorTime();
        this.finishTime = builder.getFinishTime();
        if (builder.getGenericInformation() != null) {
            this.genericInformation = ImmutableMap.copyOf(builder.getGenericInformation());
        } else {
            this.genericInformation = ImmutableMap.copyOf(new HashMap<>());
        }
        this.variables = builder.getVariables();
        if (this.variables != null) {
            this.variables.replaceAll((key, value) -> value.matches("ENC((.*))") ? "*******" : value);
        }
        this.detailedVariables = builder.getDetailedVariables();
        if (builder.getResultMap() != null) {
            this.resultMap = ImmutableMap.copyOf(builder.getResultMap());
        } else {
            this.resultMap = ImmutableMap.copyOf(new HashMap<>());
        }
        this.description = builder.getDescription();
        this.cumulatedCoreTime = builder.getCumulatedCoreTime();
        this.parentId = builder.getParentId();
        this.childrenCount = builder.getChildrenCount();
        this.numberOfNodes = builder.getNumberOfNodes();
        this.numberOfNodesInParallel = builder.getNumberOfNodesInParallel();
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
     * @return the projectName.
     */
    public String getProjectName() {
        return projectName;
    }

    /**
     * Getter for the bucketName of the job.
     * @return the bucketName.
     */
    public String getBucketName() {
        return bucketName;
    }

    /**
     * Setter of the job bucketName.
     * @param bucketName the bucket name of the job.
     */
    public void setBucketName(String bucketName) {
        this.bucketName = bucketName;
    }

    /**
     * Getter for the submission mode of the job.
     * @return the submission mode.
     */
    public String getSubmissionMode() {
        return submissionMode;
    }

    /**
     * Setter of the job submission mode.
     * @param submissionMode the submission mode of the job.
     */
    public void setSubmissionMode(String submissionMode) {
        this.submissionMode = submissionMode;
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

    public String getTenant() {
        return tenant;
    }

    public void setTenant(String tenant) {
        this.tenant = tenant;
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

    /**
     * @return cumulated core time for this job
     */
    public long getCumulatedCoreTime() {
        return cumulatedCoreTime;
    }

    public void setCumulatedCoreTime(long cumulatedCoreTime) {
        this.cumulatedCoreTime = cumulatedCoreTime;
    }

    /**
     * @return parent id of this job
     */
    public long getParentId() {
        return parentId;
    }

    public void setParentId(long parentId) {
        this.parentId = parentId;
    }

    /**
     * @return children count for this job
     */
    public int getChildrenCount() {
        return childrenCount;
    }

    public void setChildrenCount(int childrenCount) {
        this.childrenCount = childrenCount;
    }

    /**
     * @return number of nodes used by this job
     */
    public int getNumberOfNodes() {
        return numberOfNodes;
    }

    public void setNumberOfNodes(int numberOfNodes) {
        this.numberOfNodes = numberOfNodes;
    }

    /**
     * @return number of nodes in parallel used by this job
     */
    public int getNumberOfNodesInParallel() {
        return numberOfNodesInParallel;
    }

    public void setNumberOfNodesInParallel(int numberOfNodesInParallel) {
        this.numberOfNodesInParallel = numberOfNodesInParallel;
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
        return this.id == job.getId() && this.name.equals(job.getName()) &&
               this.projectName.equals(job.getProjectName()) && this.bucketName.equals(job.getBucketName()) &&
               this.priority.equals(job.getPriority()) && this.status.equals(job.getStatus()) &&
               this.user.equals(job.getUser()) && pendingTasks == job.pendingTasks &&
               runningTasks == job.runningTasks && finishedTasks == job.finishedTasks &&
               failedTasks == job.failedTasks && faultyTasks == job.faultyTasks && inErrorTasks == job.inErrorTasks &&
               finishTime == job.finishTime && inErrorTime == job.inErrorTime &&
               cumulatedCoreTime == job.cumulatedCoreTime && parentId == job.parentId &&
               childrenCount == job.childrenCount && numberOfNodes == job.numberOfNodes &&
               numberOfNodesInParallel == job.numberOfNodesInParallel &&
               submissionMode != null ? submissionMode.equals(job.submissionMode) : job.submissionMode == null;
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
        String tenant = SchedulerJSONUtils.getStringOrDefault(jsonJobInfo.get("tenant"));
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
        long cumulatedCoreTime = jsonJobInfo.get("cumulatedCoreTime") == null ? 0
                                                                              : (long) jsonJobInfo.get("cumulatedCoreTime")
                                                                                                  .isNumber()
                                                                                                  .doubleValue();
        JSONValue parentIdValue = jsonJobInfo.get("parentId");
        long parentId = parentIdValue == null ? 0
                                              : parentIdValue.isNumber() == null ? 0
                                                                                 : (long) parentIdValue.isNumber()
                                                                                                       .doubleValue();
        int childrenCount = (int) (jsonJobInfo.get("childrenCount") == null ? 0 : jsonJobInfo.get("childrenCount")
                                                                                             .isNumber()
                                                                                             .doubleValue());
        int numberOfNodes = (int) (jsonJobInfo.get("numberOfNodes") == null ? 0 : jsonJobInfo.get("numberOfNodes")
                                                                                             .isNumber()
                                                                                             .doubleValue());
        int numberOfNodesInParallel = (int) (jsonJobInfo.get("numberOfNodesInParallel") == null ? 0
                                                                                                : jsonJobInfo.get("numberOfNodesInParallel")
                                                                                                             .isNumber()
                                                                                                             .doubleValue());
        String description = SchedulerJSONUtils.getStringOrDefault(jsonJobInfo.get("description"));

        Map<String, String> genericInformation = SchedulerJSONUtils.extractMap(jsonJobInfo.get("genericInformation"));
        Map<String, String> variables = SchedulerJSONUtils.extractMap(jsonJobInfo.get("variables"));
        Map<String, String> resultMap = SchedulerJSONUtils.extractMap(jsonJobInfo.get("resultMap"));

        String name = SchedulerJSONUtils.getStringOrDefault(jsonJobInfo.get("name"));
        String projectName = SchedulerJSONUtils.getStringOrDefault(jsonJobInfo.get("projectName"));
        String bucketName = SchedulerJSONUtils.getStringOrDefault(jsonJobInfo.get("bucketName"));
        String submissionMode = SchedulerJSONUtils.getStringOrDefault(jsonJobInfo.get("submissionMode"));
        int id = Integer.valueOf(jsonJobInfo.get("id").isString().stringValue());

        return new JobBuilder().id(id)
                               .name(name)
                               .projectName(projectName)
                               .bucketName(bucketName)
                               .status(JobStatus.valueOf(status))
                               .priority(JobPriority.findPriority(priority))
                               .user(user)
                               .tenant(tenant)
                               .genericInformation(genericInformation)
                               .variables(variables)
                               .detailedVariables(new HashMap<>())
                               .resultMap(resultMap)
                               .pendingTasks(pending)
                               .runningTasks(running)
                               .finishedTasks(finished)
                               .totalTasks(total)
                               .failedTasks(failed)
                               .faultyTasks(faulty)
                               .inErrorTasks(inError)
                               .submitTime(submittedTime)
                               .startTime(startTime)
                               .inErrorTime(inErrorTime)
                               .finishTime(finishedTime)
                               .description(description)
                               .cumulatedCoreTime(cumulatedCoreTime)
                               .parentId(parentId)
                               .childrenCount(childrenCount)
                               .numberOfNodes(numberOfNodes)
                               .numberOfNodesInParallel(numberOfNodesInParallel)
                               .submissionMode(submissionMode)
                               .build();

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

    public static Map<String, Map<String, Map<String, String>>> parseJSONDetailedSignals(JSONObject jsonJobInfo) {
        Map<String, Map<String, Map<String, String>>> detailedVariablesMap = new LinkedHashMap<>();
        JSONObject detailedSignalsObject = jsonJobInfo.get("detailedSignals").isObject();
        Set<String> signals = SchedulerJSONUtils.extractSet(jsonJobInfo.get("signals"));

        signals.forEach(signal -> {
            Map<String, Map<String, String>> signalMap = new HashMap<>();
            JSONValue jsonValue = detailedSignalsObject.get(signal.substring(PREFIX_SIGNAL_READY.length()));
            if (jsonValue != null) {
                JSONObject signalObject = jsonValue.isObject();
                signalObject.keySet().forEach(variable -> {
                    Map<String, String> variableMap = new HashMap<>();
                    JSONObject variableObject = signalObject.get(variable).isObject();
                    variableMap.put("name", SchedulerJSONUtils.getStringOrDefault(variableObject.get("name")));
                    variableMap.put("value", SchedulerJSONUtils.getStringOrDefault(variableObject.get("value")));
                    variableMap.put("model", SchedulerJSONUtils.getStringOrDefault(variableObject.get("model")));
                    variableMap.put("description",
                                    SchedulerJSONUtils.getStringOrDefault(variableObject.get("description")));
                    variableMap.put("group", SchedulerJSONUtils.getStringOrDefault(variableObject.get("group")));
                    variableMap.put("advanced", variableObject.get("advanced").isBoolean().toString());
                    variableMap.put("hidden", variableObject.get("hidden").isBoolean().toString());
                    signalMap.put(variable, variableMap);
                });
                detailedVariablesMap.put(signal, signalMap);
            }
        });
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
