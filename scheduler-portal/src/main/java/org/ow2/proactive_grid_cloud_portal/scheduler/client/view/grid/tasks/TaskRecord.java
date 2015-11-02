package org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.tasks;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.Task;

import com.smartgwt.client.widgets.grid.ListGridRecord;

/**
 * A list grid record that embeds a task.
 */
public class TaskRecord extends ListGridRecord {

    private Task task = null;

    public TaskRecord(Task t) {
        this.task = t;
    }


    public Task getTask() {
        return this.task;
    }
}
