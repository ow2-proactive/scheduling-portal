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

import java.util.List;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerController;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.ExecutionDisplayModeListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.TaskSelectedListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.TasksUpdatedListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerModelImpl;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Task;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.ExecutionListMode;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.ExecutionsModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.TasksModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.tasks.TaskDetailColumnsFactory;


public class TaskInfoView extends InfoView<Task>
        implements TaskSelectedListener, TasksUpdatedListener, ExecutionDisplayModeListener {

    protected SchedulerController controller;

    public TaskInfoView(SchedulerController controller, TaskDetailColumnsFactory factory) {
        super(factory, "Please select a task from the Tasks tab on the left panel");
        this.controller = controller;
        ExecutionsModel executionsModel = ((SchedulerModelImpl) controller.getModel()).getExecutionsModel();
        TasksModel tasksCentricModel = executionsModel.getTasksModel();
        tasksCentricModel.addTaskSelectedListener(this);
        tasksCentricModel.addTasksUpdatedListener(this);

        TasksModel taskModel = ((SchedulerModelImpl) controller.getModel()).getTasksModel();
        taskModel.addTaskSelectedListener(this);
        taskModel.addTasksUpdatedListener(this);

        executionsModel.addExecutionsDisplayModeListener(this);
    }

    @Override
    public void taskSelected(Task task) {
        this.displayedItem = task;
        this.displayItem();
    }

    @Override
    public void taskUnselected() {
        this.hideDetails();
    }

    @Override
    public void tasksUpdating() {
        // TODO Auto-generated method stub

    }

    @Override
    public void tasksUpdated(List<Task> tasks, long totalTasks) {
        if (this.displayedItem == null)
            return;

        for (Task t : tasks) {
            if (t.getId().equals(this.displayedItem.getId())) {
                taskSelected(t);
            }
        }

    }

    @Override
    public void tasksUpdatedFailure(String message) {
        // TODO Auto-generated method stub

    }

    @Override
    public void modeSwitched(ExecutionListMode mode) {
        Task task = this.controller.getSelectedTask();
        if (task == null) {
            this.taskUnselected();
        } else {
            this.taskSelected(task);
        }
        controller.getSchedulerPage().disableServerLogsTab(false);
        controller.getSchedulerPage().disableOutputTab(false);
        controller.getSchedulerPage().disableJobResultsTab(false);
        controller.getSchedulerPage().disableVarInfoTab(false);
        controller.getSchedulerPage().disableTaskResultTab(false);
        controller.getSchedulerPage().disableTaskInfoTab(false);
    }

}
