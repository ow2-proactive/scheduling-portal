package org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.tasks;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.Task;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.GridColumns;

import com.smartgwt.client.data.Record;

/**
 * The factory to get the columns specifications, and build record acoording to this specification, for a task-centric grid.
 * @author the activeeon team.
 *
 */
public class TasksCentricColumnsFactory extends TasksColumnsFactory{

    public static GridColumns JOB_ID_ATTR = new GridColumns("jobId", "Job Id", 60, true);
    public static GridColumns JOB_NAME_ATTR = new GridColumns("jobName", "Job Name", 100, true);
    
    @Override
    public GridColumns[] getColumns() {
        return new GridColumns[]{ID_ATTR, STATUS_ATTR, NAME_ATTR, TAG_ATTR, JOB_ID_ATTR, JOB_NAME_ATTR, EXEC_DURATION_ATTR, NODE_COUNT_ATTR, 
                EXECUTIONS_ATTR, NODE_FAILURE_ATTR, HOST_ATTR, START_TIME_ATTR, FINISHED_TIME_ATTR,
                DESCRIPTION_ATTR, VISU_ATTR};
    }

    @Override
    public Record buildRecord(Task item) {
        Record record = super.buildRecord(item);
        buildDetailsColumns(record, item);
        record.setAttribute(JOB_ID_ATTR.getName(), item.getJobId());
        record.setAttribute(JOB_NAME_ATTR.getName(), item.getJobName());
        return record;
    }
}
