package org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid;

import org.ow2.proactive_grid_cloud_portal.common.client.JSUtil;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Task;

import com.smartgwt.client.widgets.grid.ListGridRecord;

/**
 * Entries in the Tasks Grid
 */
public class TaskRecord extends ListGridRecord {

    private Task task = null;

    public TaskRecord(Task t) {
        setAttribute(TasksColumns.ID_ATTR.getName(), t.getId().longValue());
        this.task = t;
        update(t);
    }

    /**
     * Updates the attributes of this record to reflect the fields of the task <code>t</code>
     *
     * @param t a task
     */
    public void update(Task t) {
        String execs = (t.getMaxNumberOfExec() - t.getNumberOfExecLeft()) + " / " +
                t.getMaxNumberOfExec();
        if (t.getNumberOfExecLeft() > 0)
            execs = (t.getMaxNumberOfExec() - t.getNumberOfExecLeft() + 1) + " / " +
                    t.getMaxNumberOfExec();

        String fails = (t.getMaxNumberOfExecOnFailure() - t.getNumberOfExecOnFailureLeft()) + " / " +
                t.getMaxNumberOfExecOnFailure();

        setAttribute(TasksColumns.NAME_ATTR.getName(), t.getName());
        setAttribute(TasksColumns.TAG_ATTR.getName(), t.getTag());
        setAttribute(TasksColumns.STATUS_ATTR.getName(), t.getStatus().toString());
        setAttribute(TasksColumns.EXEC_DURATION_ATTR.getName(), t.getExecutionTime());
        setAttribute(TasksColumns.EXECUTIONS_ATTR.getName(), execs);
        setAttribute(TasksColumns.NODE_FAILURE_ATTR.getName(), fails);
        setAttribute(TasksColumns.NODE_COUNT_ATTR.getName(), t.getNodeCount());

        if (t.getStartTime() > 0)
            setAttribute(TasksColumns.START_TIME_ATTR.getName(), JSUtil.getTime(t.getStartTime()));
        if (t.getFinishTime() > t.getStartTime())
            setAttribute(TasksColumns.FINISHED_TIME_ATTR.getName(), JSUtil.getTime(t.getFinishTime()));
    }

    public Task getTask() {
        return this.task;
    }
}
