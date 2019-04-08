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
package org.ow2.proactive_grid_cloud_portal.scheduler.client.view;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.Job;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerController;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.ExecutionDisplayModeListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.JobSelectedListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.TaskSelectedListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SelectionTarget;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Task;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.AbstractSelectedTargetController;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.ExecutionListMode;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.ExecutionsController;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.AbstractSelectedTargetModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.JobsModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.TasksCentricModel;

import com.smartgwt.client.widgets.form.fields.SelectItem;


public abstract class AbstractSelectedTargetView<M extends AbstractSelectedTargetModel, C extends AbstractSelectedTargetController<M>>
        implements JobSelectedListener, TaskSelectedListener, ExecutionDisplayModeListener {

    /** drop down list of task names */
    protected SelectItem targetSelect = null;

    protected C controller;

    public AbstractSelectedTargetView(C controller) {
        this.controller = controller;

        SchedulerController schedulerController = controller.getParentController();
        ExecutionsController executionsController = schedulerController.getExecutionController();
        JobsModel jobsModel = executionsController.getJobsController().getModel();
        jobsModel.addJobSelectedListener(this);

        TasksCentricModel tasksCentricModel = this.controller.getModel()
                                                             .getParentModel()
                                                             .getExecutionsModel()
                                                             .getTasksModel();
        tasksCentricModel.addTaskSelectedListener(this);
        tasksCentricModel.addJobSelectedListener(this);

        schedulerController.getTasksController().getModel().addTaskSelectedListener(this);

        executionsController.getModel().addExecutionsDisplayModeListener(this);
    }

    /**
     * Called when another job has been selected in the jobs view.
     */
    public void jobSelected(Job job) {
        this.controller.changeSelectedJob(job);
    }

    /**
     * Called when there is no longer job selected in the jobs view.
     */
    public void jobUnselected() {
        this.controller.changeSelectedJob(null);
    }

    /**
     * Called when the target selection has been changed by the user interaction.
     */
    protected void targetSelectChangedHandler() {
        String targetString = this.targetSelect.getValueAsString();
        if (targetString.equals(SelectionTarget.TASK_TARGET.label)) {
            this.controller.changeTargetOutput(SelectionTarget.TASK_TARGET);
        } else {
            this.controller.changeTargetOutput(SelectionTarget.JOB_TARGET);
        }
    }

    /**
     * Called when the selected task in tasks view or task-centric view has been changed by the user interaction.
     */
    @Override
    public void taskSelected(Task task) {
        this.controller.changeSelectedTask(task);
    }

    /**
     * Called when there is no longer selected task in tasks view or task-centric view.
     */
    @Override
    public void taskUnselected() {
        this.controller.changeSelectedTask(null);
    }

    /**
     * Called when job-centric mode or task-centric mode has been switched.
     */
    @Override
    public void modeSwitched(ExecutionListMode mode) {
        this.controller.switchMode(mode);
    }

    protected void buildTargetSelect() {
        this.targetSelect = new SelectItem();
        this.targetSelect.setShowTitle(false);
        this.targetSelect.setValueMap(SelectionTarget.toStringArray());
        this.targetSelect.setValue(SelectionTarget.JOB_TARGET.label);
        this.targetSelect.addChangedHandler(event -> targetSelectChangedHandler());
    }
}
