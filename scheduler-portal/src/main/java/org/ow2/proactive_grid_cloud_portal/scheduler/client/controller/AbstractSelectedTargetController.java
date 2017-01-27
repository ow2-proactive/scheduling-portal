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

import org.ow2.proactive_grid_cloud_portal.scheduler.client.Job;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerController;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SelectionTarget;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Task;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.AbstractSelectedTargetModel;


public abstract class AbstractSelectedTargetController<M extends AbstractSelectedTargetModel> {

    protected SchedulerController parentController;

    protected M model;

    public AbstractSelectedTargetController(SchedulerController parentController) {
        this.parentController = parentController;
    }

    public SchedulerController getParentController() {
        return parentController;
    }

    public void switchMode(ExecutionListMode mode) {
        SelectionTarget target = this.model.getSelectionTarget();
        if (target == SelectionTarget.JOB_TARGET) {
            Job job = this.parentController.getSelectedJob();
            this.changeJobOutputContext(job);
        } else {
            Task task = this.parentController.getSelectedTask();
            this.changeTaskOutputContext(task);
        }
    }

    public void changeSelectedJob(Job job) {
        if (this.model.getSelectionTarget() == SelectionTarget.JOB_TARGET) {
            this.changeJobOutputContext(job);
        }
    }

    public void changeSelectedTask(Task task) {
        if (this.model.getSelectionTarget() == SelectionTarget.TASK_TARGET) {
            this.changeTaskOutputContext(task);
        }
    }

    public abstract void changeJobOutputContext(Job job);

    public abstract void changeTaskOutputContext(Task task);

    public String getNoTargetLabelContent() {
        SelectionTarget target = this.model.getSelectionTarget();
        return target.noSelectionLabel;
    }

    public M getModel() {
        return model;
    }

    public void changeTargetOutput(SelectionTarget target) {
        this.model.setSelectionTarget(target);
        if (target == SelectionTarget.JOB_TARGET) {
            Job job = this.parentController.getSelectedJob();
            this.changeJobOutputContext(job);
        } else {
            Task task = this.parentController.getSelectedTask();
            this.changeTaskOutputContext(task);
        }
    }

    public abstract void refreshOutput();
}
