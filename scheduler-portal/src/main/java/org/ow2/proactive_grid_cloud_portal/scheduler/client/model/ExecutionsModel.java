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

package org.ow2.proactive_grid_cloud_portal.scheduler.client.model;

import java.util.ArrayList;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.ExecutionDisplayModeListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerModelImpl;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.ExecutionListMode;

public class ExecutionsModel {

    protected ExecutionListMode mode = ExecutionListMode.JOB_CENTRIC;

    protected ArrayList<ExecutionDisplayModeListener> modeListeners;

    protected JobsModel jobsModel;

    protected TasksCentricModel tasksModel;

    protected SchedulerModelImpl parentModel;


    /**
     * True if the portal fetches my executions only.
     */
    protected boolean fetchMyExecutionsOnly = false;

    /**
     * True if the portal fetches the pending executions. 
     */
    protected boolean fetchPending = true;

    /**
     * True if the portal fetches the running executions. 
     */
    protected boolean fetchRunning = true;

    /**
     * True if the portal fetches the finished executions. 
     */
    protected boolean fetchFinished = true;

    public ExecutionsModel(SchedulerModelImpl schedulerModel) {
        this.parentModel = schedulerModel;
        this.modeListeners = new ArrayList<ExecutionDisplayModeListener>();
    }

    public ExecutionListMode getMode() {
        return mode;
    }

    public void setMode(ExecutionListMode mode) {
        this.mode = mode;
        for(ExecutionDisplayModeListener listener: this.modeListeners){
            listener.modeSwitched(mode);
        }
    }

    public void addExecutionsDisplayModeListener(ExecutionDisplayModeListener listener){
        this.modeListeners.add(listener);
    }

    public JobsModel getJobsModel() {
        return jobsModel;
    }

    public void setJobsModel(JobsModel jobsModel) {
        this.jobsModel = jobsModel;
    }

    public TasksCentricModel getTasksModel() {
        return tasksModel;
    }

    public void setTasksModel(TasksCentricModel tasksModel) {
        this.tasksModel = tasksModel;
    }

    public SchedulerModelImpl getParentModel() {
        return parentModel;
    }

    /**
     * @return true if the model should only store the executions of the current user
     */
    public boolean isFetchMyExecutionsOnly() {
        return this.fetchMyExecutionsOnly;
    }

    /**
     * Sets if the portal should fetch my jobs only.
     * @param b true if the portal should fetch my jobs only.
     */
    public void fetchMyExecutionsOnly(boolean b) {
        this.fetchMyExecutionsOnly = b;
    }

    /**
     * @return true if the model should store pending jobs
     */
    public boolean isFetchPendingExecutions() {
        return this.fetchPending;
    }

    /**
     * Sets if the portal should fetch pending jobs.
     * @param b true if the portal should fetch pending jobs.
     */
    public void fetchPending(boolean f) {
        this.fetchPending = f;
    }

    /**
     * @return true if the model should store running jobs
     */
    public boolean isFetchRunningExecutions() {
        return this.fetchRunning;
    }

    /**
     * Sets if the portal should fetch running jobs.
     * @param b true if the portal should fetch running jobs.
     */
    public void fetchRunning(boolean f) {
        this.fetchRunning = f;
    }

    /**
     * @return true if the model should store finished jobs
     */
    public boolean isFetchFinishedExecutions() {
        return this.fetchFinished;
    }

    /**
     * Sets if the portal should fetch finished jobs.
     * @param b true if the portal should fetch finished jobs.
     */
    public void fetchFinished(boolean f) {
        this.fetchFinished = f;
    }
}
