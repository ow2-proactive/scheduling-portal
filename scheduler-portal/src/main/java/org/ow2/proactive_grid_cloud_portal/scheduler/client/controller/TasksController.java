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

import javax.swing.JOptionPane;

import org.ow2.proactive_grid_cloud_portal.common.client.json.JSONUtils;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LogModel;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LoginModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Job;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.NoVncUtils;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Scheduler;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerController;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerModelImpl;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerServiceAsync;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Task;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.json.JSONPaginatedTasks;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.json.SchedulerJSONUtils;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.ExecutionsModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.PaginationModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.TasksModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.TasksNavigationModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.AbstractGridItemsView;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.TasksView;

import com.google.gwt.http.client.Request;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.smartgwt.client.widgets.layout.Layout;


public class TasksController {

    /** pending taskUpdate request, or null */
    protected Request taskUpdateRequest = null;

    protected TasksNavigationController taskNavigationController;

    protected TasksModel model;

    protected AbstractGridItemsView view;

    protected SchedulerController parentController;

    public TasksController(SchedulerController parentController) {
        this.parentController = parentController;
    }

    public SchedulerController getParentController() {
        return parentController;
    }

    public Layout buildView() {
        this.model = new TasksModel((SchedulerModelImpl) this.parentController.getModel());
        this.taskNavigationController = new TasksNavigationController(this);
        this.view = new TasksView(this);
        return this.view.build();
    }

    public Layout rebuildView() {
        this.view = new TasksView(this);
        return this.view.build();
    }

    public TasksModel getModel() {
        return model;
    }

    /**
     * Updates the current task list depending the current job selection in the model 
     */
    public void updateTasks(boolean showUpdating) {
        Job selectedJob = this.model.getParentModel().getExecutionsModel().getJobsModel().getSelectedJob();

        boolean emptyTaskList = (selectedJob == null);
        if (showUpdating) {
            this.model.notifyTasksChanging(emptyTaskList);
        }

        if (emptyTaskList) {
            model.setTasks(new ArrayList<Task>(0), 0);
        } else {
            final String jobId = "" + selectedJob.getId();

            AsyncCallback<String> callback = new AsyncCallback<String>() {

                public void onFailure(Throwable caught) {
                    String msg = JSONUtils.getJsonErrorMessage(caught);

                    model.taskUpdateError(msg);
                    LogModel.getInstance()
                            .logImportantMessage("Failed to update tasks for job " + jobId + ": " + msg);
                }

                public void onSuccess(String result) {
                    try {
                        JSONPaginatedTasks tasks = SchedulerJSONUtils.parseJSONPaginatedTasks(result);
                        model.setTasksDirty(false);
                        model.setTasks(tasks.getTasks(), tasks.getTotalTasks());
                        // do not model.logMessage() : this is repeated by a timer
                    } catch (org.ow2.proactive_grid_cloud_portal.common.client.json.JSONException e) {
                        LogModel.getInstance().logCriticalMessage(e.getMessage());
                    }
                }
            };

            TasksNavigationModel navigationModel = this.model.getTasksNavigationModel();
            String tagFilter = navigationModel.getCurrentTagFilter();

            PaginationModel paginationModel = navigationModel.getPaginationModel();
            int offset = paginationModel.getOffset();
            int limit = paginationModel.getPageSize();
            String sessionId = LoginModel.getInstance().getSessionId();
            SchedulerServiceAsync scheduler = Scheduler.getSchedulerService();

            if (tagFilter.isEmpty()) {
                this.taskUpdateRequest = scheduler.getTasks(sessionId, jobId, offset, limit, callback);
            } else {
                this.taskUpdateRequest = scheduler.getTasksByTag(sessionId, jobId, tagFilter, offset, limit,
                        callback);
            }
        }
    }

    /**
     * Kill a task within a job 
     * @param taskName task name
     */
    public void killTask(final String taskName) {
        final Integer jobId = this.model.getParentModel().getExecutionsModel().getJobsModel().getSelectedJob()
                .getId();
        String sessionId = LoginModel.getInstance().getSessionId();
        SchedulerServiceAsync scheduler = Scheduler.getSchedulerService();
        scheduler.killTask(sessionId, jobId, taskName, new AsyncCallback<Boolean>() {
            @Override
            public void onFailure(Throwable caught) {
                caught.printStackTrace();

                String msg = JSONUtils.getJsonErrorMessage(caught);
                LogModel.getInstance().logImportantMessage("Failed to kill task: " + msg);
            }

            @Override
            public void onSuccess(Boolean result) {
                LogModel.getInstance()
                        .logMessage("Successfully killed task " + taskName + " in job " + jobId);
            }
        });
    }

    public void restartInErrorTask(final String taskName) {
        restartTask(taskName, RestartType.IN_ERROR_TASK);
    }

    public void restartRunningTask(final String taskName) {
        restartTask(taskName, RestartType.RUNNING_TASK);
    }

    protected void restartTask(String taskName, RestartType restartType) {
        ExecutionsModel executionsModel = this.model.getParentModel().getExecutionsModel();
        Job selectedJob = executionsModel.getSelectedJob();
        Integer jobId = selectedJob.getId();

        String sessionId = LoginModel.getInstance().getSessionId();
        SchedulerServiceAsync scheduler = Scheduler.getSchedulerService();

        if (restartType == RestartType.IN_ERROR_TASK) {
            scheduler.restartInErrorTask(sessionId, jobId, taskName,
                    callbackHandlerForRestartTask(taskName, jobId, true));
        } else if (restartType == RestartType.RUNNING_TASK) {
            scheduler.restartRunningTask(sessionId, jobId, taskName,
                    callbackHandlerForRestartTask(taskName, jobId, false));
        }
    }

    private AsyncCallback<Boolean> callbackHandlerForRestartTask(final String taskName, final Integer jobId,
            final boolean isTaskInError) {
        return new AsyncCallback<Boolean>() {

            private String context = "";

            {
                if (isTaskInError) {
                    context = "In-Error ";
                }
            }

            @Override
            public void onFailure(Throwable caught) {
                caught.printStackTrace();
                String msg = JSONUtils.getJsonErrorMessage(caught);
                LogModel.getInstance().logImportantMessage("Failed to restart " + context + "task: " + msg);
            }

            @Override
            public void onSuccess(Boolean result) {
                LogModel.getInstance().logMessage(
                        "Successfully restarted " + context + "task " + taskName + " in job " + jobId);
                getParentController().getTasksController().updateTasks(false);
            }
        };
    }

    /**
     * Preempt a task within a job 
     * @param taskName task name
     */
    public void preemptTask(final String taskName) {
        final Integer jobId = this.model.getParentModel().getExecutionsModel().getJobsModel().getSelectedJob()
                .getId();
        String sessionId = LoginModel.getInstance().getSessionId();
        SchedulerServiceAsync scheduler = Scheduler.getSchedulerService();
        scheduler.preemptTask(sessionId, jobId, taskName, new AsyncCallback<Boolean>() {
            @Override
            public void onFailure(Throwable caught) {
                String msg = JSONUtils.getJsonErrorMessage(caught);
                LogModel.getInstance().logImportantMessage("Failed to preempt task: " + msg);
            }

            @Override
            public void onSuccess(Boolean result) {
                LogModel.getInstance()
                        .logMessage("Successfully preempted task " + taskName + " in job " + jobId);
            }
        });
    }

    /**
     * Mark in-error task as finished and resume job
     * @param taskName task name
     */
    public void markAsFinishedAndResume(final String taskName) {
        ExecutionsModel executionsModel = this.model.getParentModel().getExecutionsModel();
        Job selectedJob = executionsModel.getSelectedJob();
        final Integer jobId = selectedJob.getId();

        String sessionId = LoginModel.getInstance().getSessionId();
        SchedulerServiceAsync scheduler = Scheduler.getSchedulerService();

        scheduler.markAsFinishedAndResume(sessionId, jobId, taskName, 
                new AsyncCallback<Boolean>() {
            @Override
            public void onFailure(Throwable caught) {
                String msg = JSONUtils.getJsonErrorMessage(caught);
                LogModel.getInstance().logImportantMessage("Failed to finish task: " + msg);
            }

            @Override
            public void onSuccess(Boolean result) {
                LogModel.getInstance()
                        .logMessage("Successfully finished task " + taskName + " and resumed job " + jobId);
            }
        });
    }

    public TasksNavigationController getTaskNavigationController() {
        return taskNavigationController;
    }

    public void setTaskNavigationController(TasksNavigationController taskNavigationController) {
        this.taskNavigationController = taskNavigationController;
    }

    public String computeNoVncPageUrl(String taskName) {
        String jobId = String
                .valueOf(model.getParentModel().getExecutionsModel().getJobsModel().getSelectedJob().getId());
        String sessionId = LoginModel.getInstance().getSessionId();
        return NoVncUtils.createNoVncPageUrl(sessionId, jobId, taskName);
    }

    public void resetPendingTasksRequests() {
        if (this.taskUpdateRequest != null) {
            this.taskUpdateRequest.cancel();
            this.taskUpdateRequest = null;
        }
    }

    public void updatingTasks() {
        this.model.setTasksDirty(true);
    }

    /**
     * Select another task.
     *
     * @param task of the new job selection. you can use null to cancel the current selection
     */
    public void selectTask(Task task) {
        this.model.selectTask(task);
    }

    private enum RestartType {

        IN_ERROR_TASK, RUNNING_TASK

    }

}
