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

package org.ow2.proactive_grid_cloud_portal.scheduler.client.controller;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.ow2.proactive_grid_cloud_portal.common.client.json.JSONUtils;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LogModel;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LoginModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Job;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.JobPriority;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.JobStatus;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Scheduler;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerServiceAsync;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.json.JSONPaginatedJobs;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.json.SchedulerJSONUtils;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.ExecutionsModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.JobsModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.JobsView;

import com.google.gwt.http.client.Response;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.smartgwt.client.widgets.layout.Layout;

/**
 * Controller for the jobs logic.
 * @author the activeeon team
 *
 */
public class JobsController {

    /**
     * Model for the jobs logic.
     */
    protected JobsModel model;

    /**
     * Controller for the jobs pagination logic.
     */
    protected JobsPaginationController paginationController;

    /**
     * The parent controller of this controller.
     */
    protected ExecutionsController parentController;

    /**
     * The view controlled by this controller.
     */
    protected JobsView view;

    /**
     * Builds a jobs controller from a parent scheduler controller.
     * @param parentController the parent controller.
     */
    public JobsController(ExecutionsController parentController) {
        this.parentController = parentController;
    }


    /**
     * Gets the models for this controller.
     * @return the model for this controller.
     */
    public JobsModel getModel() {
        return model;
    }

    /**
     * Sets the model for this controller.
     * @param model the model for this controller.
     */
    public void setModel(JobsModel model) {
        this.model = model;
    }


    /**
     * Builds the view controlled by this controller.
     * @return a layout that displays the view.
     */
    public Layout buildView(){
        ExecutionsModel executionsModel = this.parentController.getModel();
        this.model = new JobsModel(executionsModel);
        executionsModel.setJobsModel(this.model);

        this.paginationController = new JobsPaginationController(this);
        this.view = new JobsView(this);
        return this.view.build();
    }



    /**
     * Select another job.
     *
     * @param jobId of the new job selection. you can use null to cancel the current selection
     */
    public void selectJob(Job job) {
        Job selectedJob = model.getSelectedJob();
        // cancel async requests relative to the old selection
        if (selectedJob != null && !selectedJob.equals(job)) {
            this.parentController.getParentController().getOutputController().cancelOutputRequests();
            this.parentController.getParentController().resetPendingTasksRequests();
        }

        this.model.selectJob(job);
        
        if(job != null){
            this.parentController.getTasksController().updatingTasks();
            this.parentController.getParentController().visuFetch(job.getId().toString());
        }
    }


    /**
     * Pauses the given job, depending its current state
     * 
     * @param jobId id of the job to pause/resume
     */
    public void pauseJobs(List<String> jobId) {
        final List<Integer> l = new ArrayList<Integer>(jobId.size());
        for (String id : jobId) {
            l.add(Integer.parseInt(id));
        }

        SchedulerServiceAsync scheduler = Scheduler.getSchedulerService();
        scheduler.pauseJobs(LoginModel.getInstance().getSessionId(), l, new AsyncCallback<Integer>() {
            public void onSuccess(Integer result) {
                LogModel.getInstance().logMessage("Successfully paused " + result + "/" + l.size() + " jobs");
            }

            public void onFailure(Throwable caught) {
                String message = JSONUtils.getJsonErrorMessage(caught);
                LogModel.getInstance().logImportantMessage("Failed to pause jobs : " + message);
            }
        });
    }

    /**
     * Pauses or resumes the given job, depending its current state
     * 
     * @param jobId id of the job to pause/resume
     */
    public void resumeJobs(List<String> jobId) {
        final List<Integer> l = new ArrayList<Integer>(jobId.size());
        for (String id : jobId) {
            l.add(Integer.parseInt(id));
        }

        SchedulerServiceAsync scheduler = Scheduler.getSchedulerService();
        scheduler.resumeJobs(LoginModel.getInstance().getSessionId(), l, new AsyncCallback<Integer>() {
            public void onSuccess(Integer result) {
                LogModel.getInstance().logMessage("Successfully resumed " + result + "/" + l.size() + " jobs");
            }

            public void onFailure(Throwable caught) {
                String message = JSONUtils.getJsonErrorMessage(caught);
                LogModel.getInstance().logImportantMessage("Failed to resume jobs : " + message);
            }
        });
    }

    /**
     * Removes a job
     * 
     * @param jobId id of the job
     */
    public void removeJob(List<String> jobId) {
        final List<Integer> l = new ArrayList<Integer>(jobId.size());
        for (String id : jobId) {
            l.add(Integer.parseInt(id));
        }

        SchedulerServiceAsync scheduler = Scheduler.getSchedulerService();
        scheduler.removeJobs(LoginModel.getInstance().getSessionId(), l, new AsyncCallback<Integer>() {
            public void onSuccess(Integer result) {
                LogModel.getInstance().logMessage("Successfully removed " + result + "/" + l.size() + " jobs");
            }

            public void onFailure(Throwable caught) {
                String message = JSONUtils.getJsonErrorMessage(caught);
                LogModel.getInstance().logImportantMessage("Failed to remove jobs : " + message);
            }
        });
    }

    /**
     * Kills a job
     * 
     * @param jobId id of the job
     */
    public void killJob(List<String> jobId) {
        final List<Integer> l = new ArrayList<Integer>(jobId.size());
        for (String id : jobId) {
            l.add(Integer.parseInt(id));
        }

        SchedulerServiceAsync scheduler = Scheduler.getSchedulerService();
        scheduler.killJobs(LoginModel.getInstance().getSessionId(), l, new AsyncCallback<Integer>() {
            public void onSuccess(Integer result) {
                LogModel.getInstance().logMessage("Successfully killed " + result + "/" + l.size() + " jobs");
            }

            public void onFailure(Throwable caught) {
                String message = JSONUtils.getJsonErrorMessage(caught);
                LogModel.getInstance().logImportantMessage("Failed to kill jobs : " + message);
            }
        });
    }



    /**
     * Apply the specified priority to the given job
     * 
     * @param jobId id of the job
     * @param priority new priority
     */
    public void setJobPriority(List<String> jobId, final JobPriority priority) {
        final List<Integer> l = new ArrayList<Integer>(jobId.size());
        for (String id : jobId) {
            l.add(Integer.parseInt(id));
        }

        SchedulerServiceAsync scheduler = Scheduler.getSchedulerService();
        scheduler.setPriorityByName(LoginModel.getInstance().getSessionId(), l, priority.name(),
                new AsyncCallback<Void>() {
            public void onSuccess(Void result) {
                LogModel.getInstance().logMessage("Successfully set priority to " + priority.name() + " for " +
                        l.size() + " jobs");
            }

            public void onFailure(Throwable caught) {
                String message = JSONUtils.getJsonErrorMessage(caught);
                LogModel.getInstance().logImportantMessage("Failed to set priority to " + priority.name() + " : " +
                        message);
            }
        });
    }


    /**
     * Gets the jobs pagination controller.
     * @return the jobs pagination controller.
     */
    public JobsPaginationController getPaginationController() {
        return paginationController;
    }

    /**
     * Sets the jobs pagination controller.
     * @param jobsPaginationController the jobs pagination controller.
     */
    public void setPaginationController(
            JobsPaginationController jobsPaginationController) {
        this.paginationController = jobsPaginationController;
    }


    /**
     * Add a fake submitted job to the list
     * the name is not important, it will be updated
     * 
     * @param jobId id of the job
     * @param name name of the job
     */
    public void addSubmittingJob(int jobId, String name) {
        Job j = new Job(jobId, name, JobStatus.PENDING, JobPriority.NORMAL, LoginModel.getInstance().getLogin(), 0, 0, 0, 0, -1,
                -1, -1);
        this.model.getJobs().put(jobId, j);
        this.model.jobSubmitted(j);
    }



    /**
     * Fetch the complete JobBag from the server,
     * update the local scheduler revision number,
     * update the model and views
     */
    public void fetchJobs(boolean showUpdating) {
        if(showUpdating){
            model.jobsUpdating();
        }

        final long t1 = System.currentTimeMillis();

        int offset = paginationController.getModel().getOffset();
        int limit = paginationController.getModel().getPageSize();

        ExecutionsModel executionModel = this.parentController.getModel();
        boolean fetchMyJobs = executionModel.isFetchMyExecutionsOnly();
        boolean fetchPending = executionModel.isFetchPendingExecutions();
        boolean fetchRunning = executionModel.isFetchRunningExecutions();
        boolean fetchFinished = executionModel.isFetchFinishedExecutions();

        SchedulerServiceAsync scheduler = Scheduler.getSchedulerService();
        scheduler.revisionAndjobsinfo(LoginModel.getInstance().getSessionId(), offset, limit, fetchMyJobs,
                fetchPending, fetchRunning, fetchFinished,
                new AsyncCallback<String>() {

            public void onFailure(Throwable caught) {
                if (!LoginModel.getInstance().isLoggedIn()) {
                    // might have been disconnected in between
                    return;
                }
                int httpErrorCodeFromException = JSONUtils.getJsonErrorCode(caught);
                if (httpErrorCodeFromException == Response.SC_UNAUTHORIZED) {
                    parentController.getParentController().teardown("You have been disconnected from the server.");
                } else if (httpErrorCodeFromException == Response.SC_FORBIDDEN) {
                    LogModel.getInstance().logImportantMessage(
                            "Failed to fetch jobs because of permission (automatic refresh will be disabled)"
                                    + JSONUtils.getJsonErrorMessage(caught));
                    parentController.getParentController().stopTimer();
                    // display empty message in jobs view
                    model.emptyJobs();
                } else {
                    LogModel.getInstance().logCriticalMessage("Error while fetching jobs:\n" + JSONUtils.getJsonErrorMessage(caught));
                }
            }

            public void onSuccess(String result) {
                JSONPaginatedJobs resultJobs;
                try {
                    resultJobs = SchedulerJSONUtils.parseJSONPaginatedJobs(result);
                    Map<Integer, Job> jobs = resultJobs.getJobs();
                    long revision = resultJobs.getRevision();
                    long totalJobs = resultJobs.getTotal();
                    model.setJobs(jobs, revision, totalJobs);

                    int jn = jobs.size();
                    if (jn > 0) {
                        long t = (System.currentTimeMillis() - t1);
                        LogModel.getInstance().logMessage("<span style='color:gray;'>Fetched " + jn + " jobs in " + t +
                                " ms</span>");
                    }
                } catch (org.ow2.proactive_grid_cloud_portal.common.client.json.JSONException e) {
                    LogModel.getInstance().logCriticalMessage(e.getMessage());
                }
            }
        });
    }


    /**
     * Fetch jobs state revision. If revision is more recent, fetch jobs.
     */
    public void jobsStateRevision(){
        SchedulerServiceAsync scheduler = Scheduler.getSchedulerService();
        scheduler.schedulerStateRevision(LoginModel.getInstance().getSessionId(), new AsyncCallback<Long>() {

            public void onFailure(Throwable caught) {
                if (!LoginModel.getInstance().isLoggedIn()) {
                    // might have been disconnected in between
                    return;
                }
                if (JSONUtils.getJsonErrorCode(caught) == Response.SC_UNAUTHORIZED) {
                    parentController.getParentController().teardown("You have been disconnected from the server.");
                }
                LogModel.getInstance().logCriticalMessage("Failed to get Scheduler Revision: " + JSONUtils.getJsonErrorMessage(caught));
            }

            public void onSuccess(Long result) {
                if (result > model.getJobsRevision()) {
                    fetchJobs(false);
                }
            }
        });
    }


}
