package org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.tasks;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.GridColumns;

public class TaskDetailColumnsFactory extends TasksCentricColumnsFactory{

	@Override
    public GridColumns[] getColumns() {
        return new GridColumns[]{ID_ATTR, STATUS_ATTR, NAME_ATTR, TAG_ATTR, JOB_ID_ATTR, JOB_NAME_ATTR, EXEC_DURATION_ATTR, NODE_COUNT_ATTR, 
                EXECUTIONS_ATTR, NODE_FAILURE_ATTR, HOST_ATTR, START_TIME_ATTR, FINISHED_TIME_ATTR,
                DESCRIPTION_ATTR};
    }
}
