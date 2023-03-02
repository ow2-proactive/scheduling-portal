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

import java.util.Map;

import com.google.common.collect.ImmutableMap;


public class JobBuilder {

    private int id;

    private String name;

    private String projectName;

    private String bucketName;

    private JobStatus status;

    private JobPriority priority;

    private String user;

    private String tenant;

    private String description;

    private String submissionMode;

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

    private Map<String, String> genericInformation;

    private Map<String, String> variables;

    private Map<String, Map<String, String>> detailedVariables;

    private Map<String, String> resultMap;

    public int getId() {
        return id;
    }

    public String getName() {
        return name;
    }

    public String getProjectName() {
        return projectName;
    }

    public String getBucketName() {
        return bucketName;
    }

    public JobStatus getStatus() {
        return status;
    }

    public JobPriority getPriority() {
        return priority;
    }

    public String getUser() {
        return user;
    }

    public String getTenant() {
        return tenant;
    }

    public String getDescription() {
        return description;
    }

    public int getPendingTasks() {
        return pendingTasks;
    }

    public int getRunningTasks() {
        return runningTasks;
    }

    public int getFinishedTasks() {
        return finishedTasks;
    }

    public int getTotalTasks() {
        return totalTasks;
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

    public long getSubmitTime() {
        return submitTime;
    }

    public long getStartTime() {
        return startTime;
    }

    public long getInErrorTime() {
        return inErrorTime;
    }

    public long getFinishTime() {
        return finishTime;
    }

    public long getCumulatedCoreTime() {
        return cumulatedCoreTime;
    }

    public long getParentId() {
        return parentId;
    }

    public int getChildrenCount() {
        return childrenCount;
    }

    public int getNumberOfNodes() {
        return numberOfNodes;
    }

    public int getNumberOfNodesInParallel() {
        return numberOfNodesInParallel;
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

    public String getSubmissionMode() {
        return submissionMode;
    }

    public JobBuilder id(int id) {
        this.id = id;
        return this;
    }

    public JobBuilder name(String name) {
        this.name = name;
        return this;
    }

    public JobBuilder projectName(String projectName) {
        this.projectName = projectName;
        return this;
    }

    public JobBuilder bucketName(String bucketName) {
        this.bucketName = bucketName;
        return this;
    }

    public JobBuilder status(JobStatus status) {
        this.status = status;
        return this;
    }

    public JobBuilder priority(JobPriority priority) {
        this.priority = priority;
        return this;
    }

    public JobBuilder user(String user) {
        this.user = user;
        return this;
    }

    public JobBuilder tenant(String tenant) {
        this.tenant = tenant;
        return this;
    }

    public JobBuilder description(String description) {
        this.description = description;
        return this;
    }

    public JobBuilder pendingTasks(int pendingTasks) {
        this.pendingTasks = pendingTasks;
        return this;
    }

    public JobBuilder runningTasks(int runningTasks) {
        this.runningTasks = runningTasks;
        return this;
    }

    public JobBuilder finishedTasks(int finishedTasks) {
        this.finishedTasks = finishedTasks;
        return this;
    }

    public JobBuilder totalTasks(int totalTasks) {
        this.totalTasks = totalTasks;
        return this;
    }

    public JobBuilder failedTasks(int failedTasks) {
        this.failedTasks = failedTasks;
        return this;
    }

    public JobBuilder faultyTasks(int faultyTasks) {
        this.faultyTasks = faultyTasks;
        return this;
    }

    public JobBuilder inErrorTasks(int inErrorTasks) {
        this.inErrorTasks = inErrorTasks;
        return this;
    }

    public JobBuilder submitTime(long submitTime) {
        this.submitTime = submitTime;
        return this;
    }

    public JobBuilder startTime(long startTime) {
        this.startTime = startTime;
        return this;
    }

    public JobBuilder inErrorTime(long inErrorTime) {
        this.inErrorTime = inErrorTime;
        return this;
    }

    public JobBuilder finishTime(long finishTime) {
        this.finishTime = finishTime;
        return this;
    }

    public JobBuilder cumulatedCoreTime(long cumulatedCoreTime) {
        this.cumulatedCoreTime = cumulatedCoreTime;
        return this;
    }

    public JobBuilder parentId(long parentId) {
        this.parentId = parentId;
        return this;
    }

    public JobBuilder childrenCount(int childrenCount) {
        this.childrenCount = childrenCount;
        return this;
    }

    public JobBuilder numberOfNodes(int numberOfNodes) {
        this.numberOfNodes = numberOfNodes;
        return this;
    }

    public JobBuilder numberOfNodesInParallel(int numberOfNodesInParallel) {
        this.numberOfNodesInParallel = numberOfNodesInParallel;
        return this;
    }

    public JobBuilder genericInformation(Map<String, String> genericInformation) {
        this.genericInformation = ImmutableMap.copyOf(genericInformation);
        return this;
    }

    public JobBuilder variables(Map<String, String> variables) {
        this.variables = variables;
        return this;
    }

    public JobBuilder detailedVariables(Map<String, Map<String, String>> detailedVariables) {
        this.detailedVariables = detailedVariables;
        return this;
    }

    public JobBuilder resultMap(Map<String, String> resultMap) {
        this.resultMap = ImmutableMap.copyOf(resultMap);
        return this;
    }

    public JobBuilder submissionMode(String submissionMode) {
        this.submissionMode = submissionMode;
        return this;
    }

    public Job build() {
        return new Job(this);
    }
}
