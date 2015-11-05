package org.ow2.proactive_grid_cloud_portal.scheduler.client.view;

import java.util.List;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerController;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.TaskSelectedListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.TasksUpdatedListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerModelImpl;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Task;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.TasksModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.tasks.TasksCentricColumnsFactory;

public class TaskInfoView extends InfoView<Task> implements TaskSelectedListener, TasksUpdatedListener{

	public TaskInfoView(SchedulerController controller, TasksCentricColumnsFactory factory) {
		super(factory);
		TasksModel tasksModel = ((SchedulerModelImpl) controller.getModel()).getExecutionsModel().getTasksModel();
		tasksModel.addTaskSelectedListener(this);
		tasksModel.addTasksUpdatedListener(this);
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

	
}
