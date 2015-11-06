package org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.tasks;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.Task;

import com.smartgwt.client.data.Record;
import com.smartgwt.client.widgets.grid.ListGridRecord;

/**
 * A list grid record that embeds a task.
 */
public class TaskRecord extends ListGridRecord {

	/**
     * the attribute to store the job.
     */
    private static final String TASK_ATTR = "job";
	

    public TaskRecord(Task t) {
        setAttribute(TASK_ATTR, t);
    }

    
    public static Task getTask(Record record){
    	return (Task) record.getAttributeAsObject(TASK_ATTR);
    }
    
}
