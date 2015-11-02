package org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.tasks;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.GridColumns;

/**
 * The factory to get the columns specifications, and build record acoording to this specification, for the expandable task grid.
 * @author the activeeon team.
 *
 */
public class ExpandableTasksColumnsFactory extends TasksColumnsFactory{

    @Override
    public GridColumns[] getColumns() {
        return new GridColumns[]{ID_ATTR, STATUS_ATTR, NAME_ATTR, TAG_ATTR, EXEC_DURATION_ATTR, NODE_COUNT_ATTR, 
                EXECUTIONS_ATTR, NODE_FAILURE_ATTR, VISU_ATTR};
    }
}
