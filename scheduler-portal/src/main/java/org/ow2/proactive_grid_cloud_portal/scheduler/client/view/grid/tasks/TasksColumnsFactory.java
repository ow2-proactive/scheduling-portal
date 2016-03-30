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

package org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.tasks;

import org.ow2.proactive_grid_cloud_portal.common.client.JSUtil;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Task;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.ColumnsFactory;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.GridColumns;
import com.smartgwt.client.data.Record;

/**
 * The factory to get the columns specifications, and build record acoording to this specification.
 * @author the activeeon team
 *
 */
public abstract class TasksColumnsFactory implements ColumnsFactory<Task>{

    public static final GridColumns ID_ATTR = new GridColumns("id", "Id", 80, true, true);
    public static final GridColumns STATUS_ATTR = new GridColumns("status", "Status", 120, true, false);
    public static final GridColumns NAME_ATTR = new GridColumns("name", "Name", 120, true, false);
    public static final GridColumns TAG_ATTR = new GridColumns("tag", "Tag", 120, true, false);
    public static final GridColumns EXEC_DURATION_ATTR = new GridColumns("execDuration", "Duration", 120, true, false);
    public static final GridColumns NODE_COUNT_ATTR = new GridColumns("nodeCount", "Nodes", 80, true, false);
    public static final GridColumns EXECUTIONS_ATTR = new GridColumns("executions", "Executions", 80, true, false);
    public static final GridColumns NODE_FAILURE_ATTR = new GridColumns("nodeFailure", "Node Failures", 80, true, false);
    public static final GridColumns HOST_ATTR = new GridColumns("host", "Host", 120, true, false);
    public static final GridColumns START_TIME_ATTR = new GridColumns("startTime", "Started at", 120, true, false);
    public static final GridColumns FINISHED_TIME_ATTR = new GridColumns("finishedTime", "Finished at", 120, true, false);
    public static final GridColumns DESCRIPTION_ATTR = new GridColumns("description", "Description", -1, true, false);
    public static final GridColumns NEXT_TIME_ATTR = new GridColumns("scheduledAt", "Scheduled at", 120, true, false);
    public static final GridColumns VISU_ATTR = new GridColumns("visu", "Visu", -1, false, false);

    private static final GridColumns[] COLUMNS = new GridColumns[]{
            ID_ATTR, STATUS_ATTR, NAME_ATTR, TAG_ATTR, EXEC_DURATION_ATTR, NODE_COUNT_ATTR,
            EXECUTIONS_ATTR, NODE_FAILURE_ATTR, HOST_ATTR, START_TIME_ATTR, FINISHED_TIME_ATTR,
            DESCRIPTION_ATTR, VISU_ATTR};

    protected static final GridColumns[] COLUMNS_TO_ALIGN = new GridColumns[]{
            TasksColumnsFactory.ID_ATTR, TasksColumnsFactory.STATUS_ATTR,
            TasksColumnsFactory.NAME_ATTR, TasksColumnsFactory.TAG_ATTR,
            TasksColumnsFactory.EXEC_DURATION_ATTR, TasksColumnsFactory.NODE_COUNT_ATTR,
            TasksColumnsFactory.EXECUTIONS_ATTR, TasksColumnsFactory.NODE_FAILURE_ATTR};

    @Override
    public GridColumns[] getColumns() {
        return COLUMNS;
    }

    @Override
    public void buildRecord(Task item, Record record) {
        record.setAttribute(ID_ATTR.getName(), item.getId().longValue());

        String currentExecutionNumber =
                (item.getMaxNumberOfExec() - item.getNumberOfExecLeft())
                        + " / " + item.getMaxNumberOfExec();

        String currentFailureNumber =
                (item.getMaxNumberOfExecOnFailure() - item.getNumberOfExecOnFailureLeft())
                        + " / " + item.getMaxNumberOfExecOnFailure();

        record.setAttribute(NAME_ATTR.getName(), item.getName());
        record.setAttribute(TAG_ATTR.getName(), item.getTag());
        record.setAttribute(STATUS_ATTR.getName(), item.getStatus().toString());
        record.setAttribute(EXEC_DURATION_ATTR.getName(), item.getExecutionTime());
        record.setAttribute(EXECUTIONS_ATTR.getName(), currentExecutionNumber);
        record.setAttribute(NODE_FAILURE_ATTR.getName(), currentFailureNumber);
        record.setAttribute(NODE_COUNT_ATTR.getName(), item.getNodeCount());
    }

    /**
     * Builds the record attribute for data that could be shown in detail section in a expandable grid.
     * @param record the record that contains the attributes.
     * @param item the item that contains the data.
     */
    protected void buildDetailsColumns(Record record, Task item){
        record.setAttribute(HOST_ATTR.getName(), (item.getHostName().equals("null") ? "" : item.getHostName()));
        record.setAttribute(DESCRIPTION_ATTR.getName(), item.getDescription());

        if (item.getStartTime() > 0)
            record.setAttribute(START_TIME_ATTR.getName(), JSUtil.getTime(item.getStartTime()));
        if (item.getFinishTime() > item.getStartTime())
            record.setAttribute(FINISHED_TIME_ATTR.getName(), JSUtil.getTime(item.getFinishTime()));
        if (item.getStartAtTime() > 0)
            record.setAttribute(NEXT_TIME_ATTR.getName(), JSUtil.getTime(item.getStartAtTime()));
    }
}
