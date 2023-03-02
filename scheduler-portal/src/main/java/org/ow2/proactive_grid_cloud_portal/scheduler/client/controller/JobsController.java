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

import static org.ow2.proactive_grid_cloud_portal.common.client.json.JSONUtils.parseJSON;

import java.util.*;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;

import org.ow2.proactive_grid_cloud_portal.common.client.json.JSONException;
import org.ow2.proactive_grid_cloud_portal.common.client.json.JSONUtils;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LogModel;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LoginModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.*;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.json.SchedulerJSONUtils;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.ExecutionsModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.JobsModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.JobResultView;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.JobsView;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.KeyValueGrid;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.jobs.JobsListGrid;

import com.google.gwt.core.client.GWT;
import com.google.gwt.http.client.Request;
import com.google.gwt.http.client.Response;
import com.google.gwt.json.client.JSONArray;
import com.google.gwt.user.client.Window;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.google.gwt.xhr.client.XMLHttpRequest;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.layout.Layout;


/**
 * Controller for the jobs logic.
 *
 * @author the activeeon team
 */
public class JobsController {
    public static final String STUDIO_URL = "/studio/#workflowscheduler/";

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

    public ExecutionsController getParentController() {
        return parentController;
    }

    /**
     * The view controlled by this controller.
     */
    protected JobsView view;

    private static Logger LOGGER = Logger.getLogger(JobsController.class.getName());

    private static final String STR_JOB = " jobs";

    private static final String HEADER_PA_ERROR = "proactive_error";

    //The job signal that contains the ready_ prefix specifies that the job is ready to receive the given signal
    public static final String PREFIX_SIGNAL_READY = "ready_";

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

    public JobsView getView() {
        return view;
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
            parentController.getParentController().getTasksController().updateTasks(false);
            checkJobsPermissionMethods(new ArrayList<>(Collections.singleton(job.getId().toString())), null);
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
                LogModel.getInstance().logMessage("Successfully paused " + result + "/" + l.size() + STR_JOB);
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
                        .logMessage("Successfully resumed " + result + "/" + selectedJobs.size() + STR_JOB);
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
                LogModel.getInstance().logMessage("Successfully removed " + result + "/" + l.size() + STR_JOB);
            }

            public void onFailure(Throwable caught) {
                String message = JSONUtils.getJsonErrorMessage(caught);
                LogModel.getInstance().logImportantMessage("Failed to remove jobs : " + message);
            }
        });
    }

    /**
     * Gets the signals of a job
     *
     * @param jobId id of the job
     * @param jobsListGrid
     */
    public void getJobSignals(String jobId, JobsListGrid jobsListGrid) {
        SchedulerServiceAsync scheduler = Scheduler.getSchedulerService();
        scheduler.getJobInfoDetails(LoginModel.getInstance().getSessionId(), jobId, new AsyncCallback<String>() {
            public void onSuccess(String result) {
                try {
                    Map<String, Map<String, Map<String, String>>> detailedSignals = SchedulerJSONUtils.getDetailedSignals(result);
                    jobsListGrid.addActionsMenu(jobId, detailedSignals);
                } catch (org.ow2.proactive_grid_cloud_portal.common.client.json.JSONException e) {
                    LogModel.getInstance().logImportantMessage("Failed to parse detailed variables for job " + jobId);
                }
            }

            public void onFailure(Throwable caught) {
                String message = JSONUtils.getJsonErrorMessage(caught);
                LogModel.getInstance().logImportantMessage("Failed to get job details : " + message);
            }
        });
    }

    /**
     * Kills a list of jobs
     *
     * @param jobId id of the job
     */
    public void killJobs(List<String> jobId) {
        final List<Integer> l = new ArrayList<>(jobId.size());
        for (String id : jobId) {
            l.add(Integer.parseInt(id));
        }

        SchedulerServiceAsync scheduler = Scheduler.getSchedulerService();
        scheduler.killJobs(LoginModel.getInstance().getSessionId(), l, new AsyncCallback<Integer>() {
            public void onSuccess(Integer result) {
                LogModel.getInstance().logMessage("Successfully killed " + result + "/" + l.size() + STR_JOB);
            }

            public void onFailure(Throwable caught) {
                String message = JSONUtils.getJsonErrorMessage(caught);
                LogModel.getInstance().logImportantMessage("Failed to kill jobs : " + message);
                SC.warn("Failed to kill jobs: " + message);
            }
        });
    }

    /**
     * Kills a single job
     *
     * @param jobId id of the job
     */
    public void killJob(String jobId) {
        killJobs(Arrays.asList(jobId));
    }

    /**
     * Tries to re-submit then kill a job only and only if re-submission is successful
     * @param jobId
     */
    public void killAndResubmit(String jobId) {
        new SubmitWindow(jobId, this).show();
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
                                                                " for " + l.size() + STR_JOB);
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
     * Export the original Workflow of a job as an XML.
     * Sends a head request first to check if XML is downloadable before downloading.
     * If the user doesn't have the required permissions (not his job and is not an admin) then pops-up an alert.
     *
     * @param jobId id of the job
     */
    public void exportJobXML(String jobId) {
        String jobXmlUrl = GWT.getModuleBaseURL() + "downloadjobxml?jobId=" + jobId + "&sessionId=" +
                           LoginModel.getInstance().getSessionId();
        // Create a head-only request to check if XML is downloadable
        XMLHttpRequest req = XMLHttpRequest.create();
        req.open("HEAD", jobXmlUrl);
        req.setOnReadyStateChange(xhr -> {
            if (xhr.getReadyState() == XMLHttpRequest.DONE) {
                if (xhr.getStatus() == Response.SC_OK) {
                    LogModel.getInstance().logMessage("Downloading XML for job: " + jobId);
                    Window.open(jobXmlUrl, "Download Job XML", "");
                } else {
                    // Check if it's a user-permission related error.
                    String error = xhr.getResponseHeader(HEADER_PA_ERROR);
                    if (error != null) {
                        SC.warn("Could not export job's XML:\n" + error);
                    } else {
                        LogModel.getInstance()
                                .logMessage("Could not download Job XML. Got HTTP response code: " + xhr.getStatus());
                    }
                }
            }
        });
        req.send();
    }

    /**
     * Resubmits a job.
     * Sends a head request first to check if job XML is accessible before downloading.
     * If the user doesn't have the required permissions (not his job and is not an admin) then pops-up an alert.
     *
     * @param jobId id of the job
     */
    public void resubmitJob(String jobId) {
        String jobXmlUrl = GWT.getModuleBaseURL() + "downloadjobxml?jobId=" + jobId + "&sessionId=" +
                           LoginModel.getInstance().getSessionId();
        // Create a head-only request to check if XML is downloadable
        XMLHttpRequest req = XMLHttpRequest.create();
        req.open("HEAD", jobXmlUrl);
        req.setOnReadyStateChange(xhr -> {
            if (xhr.getReadyState() == XMLHttpRequest.DONE) {
                if (xhr.getStatus() == Response.SC_OK) {
                    new SubmitWindow(jobId).show();
                } else {
                    // Check if it's a user-permission related error.
                    String error = xhr.getResponseHeader(HEADER_PA_ERROR);
                    if (error != null) {
                        SC.warn("Could not re-submit job:\n" + error);
                    } else {
                        LogModel.getInstance()
                                .logMessage("Could not re-submit job. Got HTTP response code: " + xhr.getStatus());
                    }
                }
            }
        });
        req.send();
    }

    /**
     * Resubmit a list of jobs
     *
     * @param jobIds List of job Ids
     */
    public void resubmitAllJobs(List<String> jobIds) {
        final List<Integer> selectedJobIds = jobIds.stream().map(Integer::parseInt).collect(Collectors.toList());
        SchedulerServiceAsync scheduler = Scheduler.getSchedulerService();
        scheduler.resubmitAllJobs(LoginModel.getInstance().getSessionId(),
                                  selectedJobIds,
                                  new AsyncCallback<Integer>() {
                                      public void onSuccess(Integer result) {
                                          LogModel.getInstance().logMessage("Successfully resubmitted " + result + "/" +
                                                                            selectedJobIds.size() + STR_JOB);

                                      }

                                      public void onFailure(Throwable caught) {
                                          String message = JSONUtils.getJsonErrorMessage(caught);
                                          LogModel.getInstance()
                                                  .logImportantMessage("Failed to resubmit selected jobs : " + message);
                                      }
                                  });
    }

    /**
     * Open a job in Studio.
     * Open the workflow of selected job in the studio on a new browser tab.
     *
     * @param jobId id of the job
     */
    public void openStudio(String jobId) {

        com.google.gwt.user.client.Window.open(STUDIO_URL + jobId, "_blank", "");

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
        Job j = new JobBuilder().id(jobId)
                                .name(name)
                                .projectName("")
                                .bucketName("")
                                .status(JobStatus.PENDING)
                                .priority(JobPriority.NORMAL)
                                .user(LoginModel.getInstance().getLogin())
                                .tenant("")
                                .genericInformation(new HashMap<>())
                                .variables(new HashMap<>())
                                .detailedVariables(new HashMap<>())
                                .resultMap(new HashMap<>())
                                .pendingTasks(0)
                                .runningTasks(0)
                                .finishedTasks(0)
                                .totalTasks(0)
                                .failedTasks(0)
                                .faultyTasks(0)
                                .inErrorTasks(0)
                                .submitTime(-1)
                                .startTime(-1)
                                .inErrorTime(-1)
                                .finishTime(-1)
                                .description("")
                                .cumulatedCoreTime(0)
                                .parentId(0)
                                .childrenCount(0)
                                .numberOfNodes(0)
                                .numberOfNodesInParallel(0)
                                .submissionMode("")
                                .build();

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
        int pageSize = paginationController.getModel().getPageSize();
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
                                      model.getFilterModel(),
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
                                              parentController.getParentController().setExecutionsDataUpdated(true);
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
                                              } catch (JSONException e) {
                                                  LogModel.getInstance().logCriticalMessage(e.getMessage());
                                                  LOGGER.log(Level.SEVERE, e.getMessage());
                                              }
                                              parentController.getParentController().setExecutionsDataUpdated(true);
                                          }
                                      });
    }

    private Request metadataRequest;

    public void fetchMetadataOfPreciousResults() {
        if (metadataRequest != null) {
            metadataRequest.cancel();
            metadataRequest = null;
        }
        LoginModel loginModel = LoginModel.getInstance();
        String sessionId = loginModel.getSessionId();
        Integer jobId = model.getSelectedJob().getId();
        SchedulerServiceAsync scheduler = Scheduler.getSchedulerService();
        metadataRequest = scheduler.getPreciousTaskName(sessionId, jobId.toString(), new AsyncCallback<String>() {

            @Override
            public void onFailure(Throwable caught) {
                String msg = JSONUtils.getJsonErrorMessage(caught);
                if (msg.contains("HTTP 403 Forbidden")) {
                    getModel().preciousTaskNamesNotAuthorized();
                    LogModel.getInstance().logImportantMessage(JobResultView.NOT_AUTHORIZED);
                } else {
                    LogModel.getInstance().logImportantMessage("Error while fetching metadata of precious results:\n" +
                                                               JSONUtils.getJsonErrorMessage(caught));
                }
            }

            @Override
            public void onSuccess(String jsonString) {
                try {
                    JSONArray array = parseJSON(jsonString).isArray();
                    List<String> preciousTaskNames = new ArrayList<>(array.size());
                    for (int i = 0; i < array.size(); ++i) {
                        String taskName = array.get(i).isString().stringValue();
                        preciousTaskNames.add(taskName);
                    }

                    getModel().setPreciousTaskNamesLoaded(preciousTaskNames);
                } catch (JSONException e) {
                    LogModel.getInstance().logCriticalMessage(e.getMessage());
                }
            }
        });
    }

    public void checkJobPermissionMethod(Job job, Label label, List<KeyValueGrid> variablesGrids,
            KeyValueGrid genericInformationGrid) {

        SchedulerServiceAsync scheduler = Scheduler.getSchedulerService();
        scheduler.checkJobPermissionMethod(LoginModel.getInstance().getSessionId(),
                                           job.getId().toString(),
                                           "getJobState",
                                           new AsyncCallback<String>() {

                                               @Override
                                               public void onFailure(Throwable caught) {
                                                   String msg = JSONUtils.getJsonErrorMessage(caught);
                                                   LogModel.getInstance().logImportantMessage(
                                                                                              "Failed to access REST server endpoint : " +
                                                                                              msg);
                                               }

                                               @Override
                                               public void onSuccess(String result) {
                                                   if (result.contains("false")) {
                                                       label.setContents("You are not authorized to see this job's variables");
                                                       label.show();
                                                       variablesGrids.forEach(Canvas::hide);
                                                       genericInformationGrid.hide();
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
                parentController.getParentController().setExecutionsUpdated(true);
            }

            public void onSuccess(Long result) {
                fetchJobs(false);
                parentController.getParentController().setExecutionsUpdated(true);
            }
        });
    }

    /**
     * Checks the permission of the current user for the given jobIds
     * @param jobIds the selected job ids
     * @param jobsListGrid current jobs listGrid
     */
    public void checkJobsPermissionMethods(List<String> jobIds, JobsListGrid jobsListGrid) {

        LoginModel loginModel = LoginModel.getInstance();
        String sessionId = loginModel.getSessionId();
        List<String> methods = loginModel.getJobPermissionMethods();

        if (loginModel.permissionCashedForJobIds(jobIds)) {
            setTabsStatus(jobIds, jobsListGrid);
            return;
        }

        SchedulerServiceAsync scheduler = Scheduler.getSchedulerService();
        scheduler.checkJobsPermissionMethods(sessionId,
                                             jobIds,
                                             methods,
                                             new AsyncCallback<Map<String, Map<String, Boolean>>>() {
                                                 public void onSuccess(Map<String, Map<String, Boolean>> result) {
                                                     LoginModel.addSchedulerPermissions(result);
                                                     setTabsStatus(jobIds, jobsListGrid);
                                                 }

                                                 public void onFailure(Throwable caught) {
                                                     String message = JSONUtils.getJsonErrorMessage(caught);
                                                     LogModel.getInstance().logImportantMessage(
                                                                                                "Failed to check jobs permission methods : " +
                                                                                                message);
                                                 }
                                             });
    }

    private void setTabsStatus(List<String> jobIds, JobsListGrid jobsListGrid) {
        LoginModel loginModel = LoginModel.getInstance();
        parentController.getParentController()
                        .getSchedulerPage()
                        .disableServerLogsTab(loginModel.userDoesNotHavePermissionToGetJobsServerLogs(jobIds));
        if (loginModel.userDoesNotHavePermissionToGetJobsState(jobIds)) {
            parentController.getParentController().getSchedulerPage().disableOutputTab(true);
            parentController.getParentController().getSchedulerPage().disableVarInfoTab(true);
            parentController.getParentController().getSchedulerPage().disableTasksTab(true);
            parentController.getParentController().getSchedulerPage().disableTaskInfoTab(true);
        } else {
            parentController.getParentController().getSchedulerPage().disableOutputTab(false);
            parentController.getParentController().getSchedulerPage().disableVarInfoTab(false);
            parentController.getParentController().getSchedulerPage().disableTasksTab(false);
            parentController.getParentController().getSchedulerPage().disableTaskInfoTab(false);
        }
        if (loginModel.userDoesNotHavePermissionToGetJobsResult(jobIds)) {
            parentController.getParentController().getSchedulerPage().disableJobResultsTab(true);
            parentController.getParentController().getSchedulerPage().disableTaskResultTab(true);
        } else {
            parentController.getParentController().getSchedulerPage().disableJobResultsTab(false);
            parentController.getParentController().getSchedulerPage().disableTaskResultTab(false);
        }
        parentController.getParentController()
                        .getSchedulerPage()
                        .disableVisualizationTab(loginModel.userDoesNotHavePermissionToGetJobsContent(jobIds));
        if (jobsListGrid != null) {
            jobsListGrid.setMenuItemsStatus(jobIds);
        }
    }

}
