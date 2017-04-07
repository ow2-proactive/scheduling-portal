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
package org.ow2.proactive_grid_cloud_portal.scheduler.client.controller;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.ow2.proactive_grid_cloud_portal.common.client.json.JSONUtils;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LogModel;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LoginModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Job;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.JobPriority;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.JobStatus;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Scheduler;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerServiceAsync;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.json.SchedulerJSONUtils;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.ExecutionsModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.JobsModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.JobsView;
import org.ow2.proactive_grid_cloud_portal.scheduler.shared.PaginatedItemType;
import org.ow2.proactive_grid_cloud_portal.scheduler.shared.SchedulerConfig;

import com.google.gwt.http.client.Response;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.smartgwt.client.widgets.layout.Layout;


/**
 * Controller for the jobs logic.
 *
 * @author the activeeon team
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

    private static Logger LOGGER = Logger.getLogger(JobsController.class.getName());

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
    public Layout buildView() {
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
     * @param job new job selected. You can use null to cancel the current selection.
     */
    public void selectJob(Job job) {
        Job selectedJob = model.getSelectedJob();
        // cancel async requests relative to the old selection
        if (selectedJob != null && !selectedJob.equals(job)) {
            this.parentController.getParentController().getOutputController().cancelCurrentRequests();
            this.parentController.getParentController().resetPendingTasksRequests();
        }

        this.model.selectJob(job);

        if (job != null) {
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
        final List<Integer> l = new ArrayList<>(jobId.size());
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
     * Restarts all in error tasks the given job, depending its current state.
     *
     * @param jobId id of the job to pause/resume
     */
    public void restartAllInErrorTasks(List<String> jobId) {
        final List<Integer> selectedJobs = new ArrayList<>(jobId.size());
        for (String id : jobId) {
            selectedJobs.add(Integer.parseInt(id));
        }

        SchedulerServiceAsync scheduler = Scheduler.getSchedulerService();
        scheduler.restartAllInErrorTasks(LoginModel.getInstance().getSessionId(),
                                         selectedJobs,
                                         new AsyncCallback<Integer>() {
                                             public void onSuccess(Integer result) {
                                                 LogModel.getInstance()
                                                         .logMessage("Successfully restarted in error tasks  " +
                                                                     result + "/" + selectedJobs.size() +
                                                                     " for selected jobs");
                                                 parentController.getParentController()
                                                                 .getTasksController()
                                                                 .updateTasks(false);
                                             }

                                             public void onFailure(Throwable caught) {
                                                 String message = JSONUtils.getJsonErrorMessage(caught);
                                                 LogModel.getInstance().logImportantMessage(
                                                                                            "Failed to restart all in error tasks for selected jobs : " +
                                                                                            message);
                                             }
                                         });
    }

    /**
     * Pauses or resumes the given job, depending its current state
     * 
     * @param jobId id of the job to pause/resume
     */
    public void resumeJobs(List<String> jobId) {
        final List<Integer> selectedJobs = new ArrayList<>(jobId.size());
        for (String id : jobId) {
            selectedJobs.add(Integer.parseInt(id));
        }

        SchedulerServiceAsync scheduler = Scheduler.getSchedulerService();
        scheduler.resumeJobs(LoginModel.getInstance().getSessionId(), selectedJobs, new AsyncCallback<Integer>() {
            public void onSuccess(Integer result) {
                LogModel.getInstance()
                        .logMessage("Successfully resumed " + result + "/" + selectedJobs.size() + " jobs");
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
        final List<Integer> l = new ArrayList<>(jobId.size());
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
        final List<Integer> l = new ArrayList<>(jobId.size());
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
        final List<Integer> l = new ArrayList<>(jobId.size());
        for (String id : jobId) {
            l.add(Integer.parseInt(id));
        }

        SchedulerServiceAsync scheduler = Scheduler.getSchedulerService();
        scheduler.setPriorityByName(LoginModel.getInstance().getSessionId(),
                                    l,
                                    priority.name(),
                                    new AsyncCallback<Void>() {
                                        public void onSuccess(Void result) {
                                            LogModel.getInstance()
                                                    .logMessage("Successfully set priority to " + priority.name() +
                                                                " for " + l.size() + " jobs");
                                        }

                                        public void onFailure(Throwable caught) {
                                            String message = JSONUtils.getJsonErrorMessage(caught);
                                            LogModel.getInstance()
                                                    .logImportantMessage("Failed to set priority to " +
                                                                         priority.name() + " : " + message);
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
    public void setPaginationController(JobsPaginationController jobsPaginationController) {
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
        Job j = new Job(jobId,
                        name,
                        JobStatus.PENDING,
                        JobPriority.NORMAL,
                        LoginModel.getInstance().getLogin(),
                        new HashMap<>(),
                        new HashMap<>(),
                        0,
                        0,
                        0,
                        0,
                        0,
                        0,
                        0,
                        -1,
                        -1,
                        -1,
                        -1);
        this.model.getJobs().put(jobId, j);
        this.model.jobSubmitted(j);
    }

    /**
     * Fetch the complete JobBag from the server,
     * update the local scheduler revision number,
     * update the model and views
     */
    public void fetchJobs(boolean showUpdating) {
        if (showUpdating) {
            model.jobsUpdating();
        }

        final long t1 = System.currentTimeMillis();

        String startCursor = paginationController.getModel().getStartCursor();
        String endCursor = paginationController.getModel().getEndCursor();
        int pageSize = SchedulerConfig.get().getPageSize(PaginatedItemType.JOB);
        boolean first = paginationController.getModel().isFirst();

        ExecutionsModel executionModel = this.parentController.getModel();
        String user = null;
        if (executionModel.isFetchMyExecutionsOnly()) {
            user = LoginModel.getInstance().getLogin();
        }

        boolean fetchPending = executionModel.isFetchPendingExecutions();
        boolean fetchRunning = executionModel.isFetchRunningExecutions();
        boolean fetchFinished = executionModel.isFetchFinishedExecutions();

        SchedulerServiceAsync scheduler = Scheduler.getSchedulerService();
        scheduler.revisionAndjobsinfo(LoginModel.getInstance().getSessionId(),
                                      startCursor,
                                      endCursor,
                                      pageSize,
                                      first,
                                      user,
                                      fetchPending,
                                      fetchRunning,
                                      fetchFinished,
                                      new AsyncCallback<String>() {

                                          public void onFailure(Throwable caught) {
                                              if (!LoginModel.getInstance().isLoggedIn()) {
                                                  // might have been disconnected in between
                                                  return;
                                              }
                                              int httpErrorCodeFromException = JSONUtils.getJsonErrorCode(caught);
                                              if (httpErrorCodeFromException == Response.SC_UNAUTHORIZED) {
                                                  parentController.getParentController()
                                                                  .teardown("You have been disconnected from the server.");
                                              } else if (httpErrorCodeFromException == Response.SC_FORBIDDEN) {
                                                  LogModel.getInstance()
                                                          .logImportantMessage("Failed to fetch jobs because of permission (automatic refresh will be disabled)" +
                                                                               JSONUtils.getJsonErrorMessage(caught));
                                                  parentController.getParentController().stopTimer();
                                                  // display empty message in jobs view
                                                  model.emptyJobs();
                                              } else {
                                                  LogModel.getInstance()
                                                          .logCriticalMessage("Error while fetching jobs:\n" +
                                                                              JSONUtils.getJsonErrorMessage(caught));
                                              }
                                          }

                                          public void onSuccess(String result) {
                                              Map<Integer, Job> jobs;
                                              try {
                                                  jobs = SchedulerJSONUtils.parseJSONJobs(result,
                                                                                          paginationController.getModel());
                                                  model.setJobs(jobs);

                                                  int jn = jobs.size();
                                                  if (jn > 0) {
                                                      long t = (System.currentTimeMillis() - t1);
                                                      LogModel.getInstance()
                                                              .logMessage("<span style='color:gray;'>Fetched " + jn +
                                                                          " jobs in " + t + " ms</span>");
                                                  }
                                              } catch (org.ow2.proactive_grid_cloud_portal.common.client.json.JSONException e) {
                                                  LogModel.getInstance().logCriticalMessage(e.getMessage());
                                                  LOGGER.log(Level.SEVERE, e.getMessage());
                                              }
                                          }
                                      });
    }

    /**
     * Fetch jobs state revision. If revision is more recent, fetch jobs.
     */
    public void jobsStateRevision() {
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
                LogModel.getInstance().logCriticalMessage("Failed to get Scheduler Revision: " +
                                                          JSONUtils.getJsonErrorMessage(caught));
            }

            public void onSuccess(Long result) {
                fetchJobs(false);
            }
        });
    }

}
