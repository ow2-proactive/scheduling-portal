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

import org.ow2.proactive_grid_cloud_portal.common.client.model.LogModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerController;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerModelImpl;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.ExecutionsModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.ExecutionsView;

import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.SectionStackSection;

public class ExecutionsController {

    protected ExecutionsView view;

    protected JobsController jobsController;

    protected TasksCentricController tasksController;

    protected ExecutionsModel model;

    protected SchedulerController parentController;


    public ExecutionsController(SchedulerController controller) {
        this.parentController = controller;
        SchedulerModelImpl schedulerModel = (SchedulerModelImpl) controller.getModel(); 
        this.model = new ExecutionsModel(schedulerModel);
        schedulerModel.setExecutionsModel(this.model);
    }


    public SectionStackSection buildView(){
        this.view = new ExecutionsView(this);
        return this.view.build();
    }


    public Layout buildJobsView(){
        this.jobsController = new JobsController(this);
        return this.jobsController.buildView();
    }


    public Layout buildTasksView(){
        this.tasksController = new TasksCentricController(this.parentController);
        return this.tasksController.buildView();
    }


    public JobsController getJobsController() {
        return jobsController;
    }


    public void setJobsController(JobsController jobsController) {
        this.jobsController = jobsController;
    }


    public TasksController getTasksController() {
        return tasksController;
    }


    public void setTasksController(TasksCentricController tasksController) {
        this.tasksController = tasksController;
    }


    public void switchMode(String mode){
        if(mode.equals(ExecutionListMode.JOB_CENTRIC.name)){
            this.model.setMode(ExecutionListMode.JOB_CENTRIC);
        }
        else{
            this.model.setMode(ExecutionListMode.TASK_CENTRIC);
        }
        executionStateRevision(true);
    }


    public ExecutionsModel getModel() {
        return model;
    }


    public SchedulerController getParentController() {
        return parentController;
    }


    public void executionStateRevision(boolean forceRefresh){
        switch(this.model.getMode()){
        case JOB_CENTRIC:
            this.jobsController.jobsStateRevision();
            break;
        case TASK_CENTRIC:
            this.tasksController.tasksStateRevision(forceRefresh);
            break;
        }
    }


    protected void fetchFirstPage(){
        switch(this.model.getMode()){
        case JOB_CENTRIC:
            this.jobsController.getPaginationController().firstPage();
            break;
        case TASK_CENTRIC:
            this.tasksController.getPaginationController().firstPage();
            break;
        }
    }


    /**
     * Invalidates the current job list if toggling state,
     * refetch immediately a new job list
     * 
     * @param b true to fetch only jobs submitted by the current user, or false to fetch all jobs
     */
    public void fetchMyExecutionsOnly(boolean b) {
        if (b == model.isFetchMyExecutionsOnly())
            return;

        model.fetchMyExecutionsOnly(b);

        if (b)
            LogModel.getInstance().logMessage("Fetching only my executions");
        else
            LogModel.getInstance().logMessage("Fetching all executions");

        this.fetchFirstPage();
    }

    /**
     * Invalidates the current job list if toggling state,
     * refetch immediately a new job list
     * 
     * @param f true to fetch pending jobs
     */
    public void fetchPending(boolean f) {
        if (f == model.isFetchPendingExecutions())
            return;

        model.fetchPending(f);

        if (f)
            LogModel.getInstance().logMessage("Fetching pending executions");
        else
            LogModel.getInstance().logMessage("Dot not fetch pending executions");

        this.fetchFirstPage();
    }

    /**
     * Invalidates the current job list if toggling state,
     * refetch immediately a new job list
     * 
     * @param f true to fetch running jobs
     */
    public void fetchRunning(boolean f) {
        if (f == model.isFetchRunningExecutions())
            return;

        model.fetchRunning(f);

        if (f)
            LogModel.getInstance().logMessage("Fetching running executions");
        else
            LogModel.getInstance().logMessage("Dot not fetch running executions");

        this.fetchFirstPage();
    }

    /**
     * Invalidates the current job list if toggling state,
     * refetch immediately a new job list
     * 
     * @param f true to fetch finished jobs
     */
    public void fetchFinished(boolean f) {
        if (f == model.isFetchFinishedExecutions())
            return;

        model.fetchFinished(f);

        if (f)
            LogModel.getInstance().logMessage("Fetching finished executions");
        else
            LogModel.getInstance().logMessage("Dot not fetch finished executions");

        this.fetchFirstPage();
    }
}
