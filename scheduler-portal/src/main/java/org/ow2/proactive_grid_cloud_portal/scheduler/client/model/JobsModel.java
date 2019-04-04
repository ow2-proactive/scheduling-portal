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
package org.ow2.proactive_grid_cloud_portal.scheduler.client.model;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.Job;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.JobSelectedListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.JobsUpdatedListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.shared.filter.FilterModel;


/**
 * Model for the jobs logic.
 * @author the activeeon team.
 *
 */
public class JobsModel {

    /**
     * The jobs that are fetched from the server.
     */
    private Map<Integer, Job> jobs = null;

    /**
     * The current selected job.
     */
    private Job selectedJob = null;

    /**
     * The current jobs selected on the grid.
     */
    private List<Integer> selectedJobsIds = null;

    /**
     * Listener for the updates of the jobs list.
     */
    private ArrayList<JobsUpdatedListener> jobsUpdatedListeners = null;

    /**
     * Listener for the job selection changes.
     */
    private ArrayList<JobSelectedListener> jobSelectedListeners = null;

    /**
     * The parent model.
     */
    private ExecutionsModel parentModel;

    /**
     * The model for the job pagination logic.
     */
    private JobsPaginationModel paginationModel;

    /**
     * The filters
     */
    private FilterModel filterModel;

    /**
     * Builds a jobs model from the scheduler parent model.
     * @param parentModel the scheduler parent model.
     */
    public JobsModel(ExecutionsModel parentModel) {
        this.parentModel = parentModel;
        this.jobsUpdatedListeners = new ArrayList<JobsUpdatedListener>();
        this.jobSelectedListeners = new ArrayList<JobSelectedListener>();
        filterModel = new FilterModel();
    }

    /**
     * @return local view of all the jobs known by the scheduler server
     */
    public Map<Integer, Job> getJobs() {
        return this.jobs;
    }

    /**
     * Empties the jobs list.
     */
    public void emptyJobs() {
        setJobs(null);
    }

    public FilterModel getFilterModel() {
        return filterModel;
    }

    public void setFilterModel(FilterModel filterModel) {
        this.filterModel = filterModel;
    }

    /**
     * Modifies the local joblist
     * triggers {@link JobsUpdatedListener#jobsUpdated(java.util.Map)}},
     * or {@link JobsUpdatedListener#jobsUpdating()} if <code>jobs</code> was null
     * 
     * @param jobs a jobset, or null
     */
    public void setJobs(Map<Integer, Job> jobs) {
        this.jobs = jobs;
        boolean empty = false;

        if (jobs == null) {
            empty = true;
            this.jobs = new LinkedHashMap<Integer, Job>();
        }

        for (JobsUpdatedListener listener : this.jobsUpdatedListeners) {
            listener.jobsUpdated(this.jobs);
            if (empty)
                listener.jobsUpdating();
        }

        if (this.selectedJob != null) {
            Job oldSel = this.selectedJob;
            this.selectedJob = jobs.get(oldSel.getId());
            if (this.selectedJob == null) {
                for (JobSelectedListener listener : this.jobSelectedListeners) {
                    listener.jobUnselected();
                }
            } else {
                if (this.selectedJob != null && !this.selectedJob.isEqual(oldSel)) {
                    for (JobSelectedListener listener : this.jobSelectedListeners) {
                        listener.selectedJobUpdated(this.selectedJob);
                    }
                }
            }
        }
    }

    /**
     * Notifies that a job has been submitted.
     * @param j
     */
    public void jobSubmitted(Job j) {
        for (JobsUpdatedListener listener : this.jobsUpdatedListeners) {
            listener.jobSubmitted(j);
        }
    }

    /**
     * Notifies that jobs are updating.
     */
    public void jobsUpdating() {
        for (JobsUpdatedListener listener : this.jobsUpdatedListeners) {
            listener.jobsUpdating();
        }
    }

    /**
     * Gets the jobs pagination model. 
     * @return the jobs pagination model.
     */
    public JobsPaginationModel getPaginationModel() {
        return paginationModel;
    }

    /**
     * Sets the jobs pagination model.
     * @param jobsPaginationModel the jobs pagination model.
     */
    public void setPaginationModel(JobsPaginationModel jobsPaginationModel) {
        this.paginationModel = jobsPaginationModel;
    }

    /**
     * Modifies the Job selection,
     * triggers a JobSelected event
     *
     */
    public void selectJob(Job job) {
        boolean selChanged = this.selectedJob == null || !this.selectedJob.equals(job);
        this.selectedJob = job;

        if (selChanged) {
            // notify job selection listeners
            for (JobSelectedListener listener : this.jobSelectedListeners) {
                if (job == null)
                    listener.jobUnselected();
                else
                    listener.jobSelected(job);
            }

            this.parentModel.getParentModel().getTasksModel().selectTask(null);
        }
    }

    /**
     * @return the currently selected job
     */
    public Job getSelectedJob() {
        return this.selectedJob;
    }

    /**
     * @param jobId the Id of a Job
     * @return the corresponding job if known by the Model, or null
     */
    public Job getJob(int jobId) {
        for (Job j : this.jobs.values()) {
            if (j.getId() == jobId) {
                return j;
            }
        }
        return null;
    }

    /**
     * Add a listener for jobs list updates
     * @param listener a listener for jobs list updates.
     */
    public void addJobsUpdatedListener(JobsUpdatedListener listener) {
        this.jobsUpdatedListeners.add(listener);
    }

    /**
     * Add a listener for job selection changes.
     * @param listener a listener for job selection changes.
     */
    public void addJobSelectedListener(JobSelectedListener listener) {
        this.jobSelectedListeners.add(listener);
    }

    public List<Integer> getSelectedJobsIds() {
        return selectedJobsIds;
    }

    public void setSelectedJobsIds(List<Integer> selectedJobsIds) {
        this.selectedJobsIds = selectedJobsIds;
    }
}
