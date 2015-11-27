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

import org.ow2.proactive_grid_cloud_portal.common.client.json.JSONException;
import org.ow2.proactive_grid_cloud_portal.common.client.json.JSONUtils;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LogModel;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LoginModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Job;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Scheduler;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerController;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerModelImpl;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerServiceAsync;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Task;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.json.JSONPaginatedTasks;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.json.SchedulerJSONUtils;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.ExecutionsModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.PaginationModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.TasksCentricModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.TasksCentricNavigationModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.TasksCentricView;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.smartgwt.client.widgets.layout.Layout;

import java.util.logging.Level;
import java.util.logging.Logger;

public class TasksCentricController extends TasksController{


    public TasksCentricController(SchedulerController parentController) {
        super(parentController);
    }


    @Override
    public Layout buildView() {
        SchedulerModelImpl schedulerModel = (SchedulerModelImpl) this.parentController.getModel();
        this.model = new TasksCentricModel(schedulerModel);
        schedulerModel.getExecutionsModel().setTasksModel((TasksCentricModel) this.model);
        this.taskNavigationController = new TasksCentricNavigationController(this);
        this.view = new TasksCentricView(this);
        return this.view.build();
    }


    public void tasksStateRevision(boolean forceRefresh){
        TasksCentricNavigationModel navigationModel = (TasksCentricNavigationModel) this.model.getTasksNavigationModel();
        if(navigationModel.getTaskAutoRefreshOption() || forceRefresh){
            this.updateTasks(false);
        }
    }


    /**
     * Updates the current task list depending the current job selection in the model 
     */
    public void updateTasks(boolean showUpdating) {
        if(showUpdating){
            this.model.notifyTasksChanging(false);
        }

        AsyncCallback<String> callback = new AsyncCallback<String>() {

            public void onFailure(Throwable caught) {
                String msg = JSONUtils.getJsonErrorMessage(caught);
                model.taskUpdateError(msg);
                LogModel.getInstance().logImportantMessage("Failed to update tasks for job : " + msg);
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

        TasksCentricNavigationModel navigationModel = (TasksCentricNavigationModel) this.model.getTasksNavigationModel();
        String tagFilter = navigationModel.getCurrentTagFilter();
        long fromDate = navigationModel.getFromDate();
        long toDate  = navigationModel.getToDate(); 

        PaginationModel paginationModel = navigationModel.getPaginationModel();
        int offset = paginationModel.getOffset();
        int limit = paginationModel.getPageSize();
        String sessionId = LoginModel.getInstance().getSessionId();
        SchedulerServiceAsync scheduler = Scheduler.getSchedulerService();

        ExecutionsModel executionsModel = this.model.getParentModel().getExecutionsModel();
        boolean myTasksOnly = executionsModel.isFetchMyExecutionsOnly();
        boolean pending = executionsModel.isFetchPendingExecutions();
        boolean running = executionsModel.isFetchRunningExecutions();
        boolean finished = executionsModel.isFetchFinishedExecutions();

        if (tagFilter.isEmpty()){
            this.taskUpdateRequest = scheduler.getTaskCentric(sessionId, fromDate, toDate, myTasksOnly, pending, 
                    running, finished, offset, limit, callback);
        } else{
            this.taskUpdateRequest = scheduler.getTaskCentricByTag(sessionId, tagFilter, fromDate, toDate, myTasksOnly, pending, 
                    running, finished, offset, limit, callback);
        }
    }



    public TasksPaginationController getPaginationController(){
        return this.taskNavigationController.getPaginationController();
    }

    
    /**
     * Select another task.
     *
     * @param taskId of the new task selection. you can use null to cancel the current selection
     */
    public void selectTask(final Task task) {
        super.selectTask(task);
        if(task != null){
            final String jobId = Long.toString(task.getJobId());
            String sessionId = LoginModel.getInstance().getSessionId();
            SchedulerServiceAsync scheduler = Scheduler.getSchedulerService();
            scheduler.getJobInfoDetails(sessionId, jobId, new AsyncCallback<String>() {
                @Override
                public void onFailure(Throwable caught) {

                    caught.printStackTrace();
                    String msg = JSONUtils.getJsonErrorMessage(caught);
                    LogModel.getInstance().logImportantMessage("Failed to get job info for job " + jobId + ": " + msg);
                }

                @Override
                public void onSuccess(String result) {
                    try {
                        Job job = SchedulerJSONUtils.getJobInfoFromJson(result);
                        ((TasksCentricModel) model).setTaskSelectedJob(job);
                    } catch (JSONException e) {
                        LogModel.getInstance().logCriticalMessage(e.getMessage());
                    }
                }
            });
        }
        else{
            ((TasksCentricModel) model).setTaskSelectedJob(null);
        }
    }
}
