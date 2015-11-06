/*
 *  *
 * ProActive Parallel Suite(TM): The Java(TM) library for
 *    Parallel, Distributed, Multi-Core Computing for
 *    Enterprise Grids & Clouds
 *
 * Copyright (C) 1997-2014 INRIA/University of
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
 *  * $$PROACTIVE_INITIAL_DEV$$
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

public class TaskInfoView extends InfoView<Task> implements TaskSelectedListener, TasksUpdatedListener, ExecutionDisplayModeListener{

	protected SchedulerController controller;
	
	public TaskInfoView(SchedulerController controller, TaskDetailColumnsFactory factory) {
		super(factory, "No task selected");
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
		if(task == null){
			this.taskUnselected();
		}
		else{
			this.taskSelected(task);
		}
	}

}
