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

import java.io.Serializable;

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
    private JobStatus status;
    private JobPriority priority;
    private String user;
    private int pendingTasks;
    private int runningTasks;
    private int finishedTasks;
    private int totalTasks;
    private long submitTime;
    private long startTime;
    private long finishTime;

    /**
     * The constructor that has no arguments required by the Serializable interface
     */
    public Job() {
    }

    /**
     * Creates a new instance of Job
     *
     * @param id Job id
     */
    public Job(int id) {
        this.id = id;
    }

    /**
     * Creates a new instance of the Job class.
     * @param id the job ID
     * @param name the job name
     * @param status the job status
     * @param priority the job priority
     * @param user the username of the user that submitted the job
     */
    public Job(int id, String name, JobStatus status, JobPriority priority, String user, int pending,
            int running, int finished, int total, long submitTime, long startTime, long finishTime) {
        this.id = id;
        this.name = name;
        this.setStatus(status);
        this.setPriority(priority);
        this.setUser(user);
        this.pendingTasks = pending;
        this.runningTasks = running;
        this.finishedTasks = finished;
        this.totalTasks = total;
        this.submitTime = submitTime;
        this.startTime = startTime;
        this.finishTime = finishTime;
    }

    /**
     * Setter of the job ID.
     * @param id the new ID that will be set.
     */
    public void setID(int id) {
        this.id = id;
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
        return this.id == job.getId() && this.name.equals(job.getName()) &&
                this.priority.equals(job.getPriority()) && this.status.equals(job.getStatus()) &&
                this.user.equals(job.getUser()) && pendingTasks == job.pendingTasks &&
                runningTasks == job.runningTasks && finishedTasks == job.finishedTasks &&
                finishTime == job.finishTime;
    }

    public int compareTo(Job job) {
        return ((Integer) this.id).compareTo(job.getId());
    }

    @Override
    public String toString() {
        return "[ id=" + id + "; " + "name=" + name + "; " + "status=" + status + "; " + "priority=" +
                priority + "; " + "user=" + user + "]";
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
        int id = Integer.parseInt(jsonJob.get("jobid").isString().stringValue());
        String user = jsonJob.get("jobOwner").isString().stringValue();
        JSONObject jsonInfo = jsonJob.get("jobInfo").isObject(); // TODO to update jobInfo
        String priority = jsonInfo.get("priority").isString().stringValue();
        String status = jsonInfo.get("status").isString().stringValue();
        int pending = (int) jsonInfo.get("numberOfPendingTasks").isNumber().doubleValue();
        int running = (int) jsonInfo.get("numberOfRunningTasks").isNumber().doubleValue();
        int finished = (int) jsonInfo.get("numberOfFinishedTasks").isNumber().doubleValue();
        int total = (int) jsonInfo.get("totalNumberOfTasks").isNumber().doubleValue();
        long submittedTime = (long) jsonInfo.get("submittedTime").isNumber().doubleValue();
        long startTime = (long) jsonInfo.get("startTime").isNumber().doubleValue();
        long finishedTime = (long) jsonInfo.get("finishedTime").isNumber().doubleValue();
        JSONObject jsonInfoId = jsonInfo.get("jobId").isObject();
        String name = jsonInfoId.get("readableName").isString().stringValue();

        return new Job(id, name, JobStatus.valueOf(status), JobPriority.findPriority(priority), user,
                pending, running, finished, total, submittedTime, startTime, finishedTime);
    }

    /**
     * @param millis a duration in milliseconds
     * @return the same duration in a human readable format
     */
    public static String formatDuration(long millis) {
        if (millis < 0)
            return "";

        long secs = millis / 1000;
        long mins = secs / 60;
        long h = mins / 60;
        millis = millis % 1000;
        secs = secs % 60;
        mins = mins % 60;

        String ret = millis + "ms";
        if (secs > 0)
            ret = secs + "s " + ret;
        if (mins > 0)
            ret = mins + "m " + ret;
        if (h > 0)
            ret = h + "h " + ret;

        return ret;
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

    public void setTotalTasks(int total) {
        this.totalTasks = total;
    }
}
