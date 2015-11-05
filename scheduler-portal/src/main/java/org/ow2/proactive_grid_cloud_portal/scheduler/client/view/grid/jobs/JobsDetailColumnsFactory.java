package org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.jobs;

import org.ow2.proactive_grid_cloud_portal.common.client.JSUtil;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Job;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.GridColumns;

import com.smartgwt.client.data.Record;
import com.smartgwt.client.widgets.viewer.DetailViewerRecord;

public class JobsDetailColumnsFactory extends JobsColumnsFactory{

    public static GridColumns PENDING_TASKS_ATTR = new GridColumns("pendingTasks", "Pending tasks", 20, true);
    public static GridColumns RUNNING_TASKS_ATTR = new GridColumns("runningTasks", "Running tasks", 20, true);
    public static GridColumns FINISHED_TASKS_ATTR = new GridColumns("finishedTasks", "Finished tasks", 20, true);
    public static GridColumns TOTAL_TASKS_ATTR = new GridColumns("totalTasks", "Total tasks", 20, true);
    public static GridColumns SUBMITTED_TIME_ATTR = new GridColumns("submittedTime", "Submitted time", 50, true);
    public static GridColumns STARTED_TIME_ATTR = new GridColumns("startedTime", "Started time", 50, true);
    public static GridColumns FINISHED_TIME_ATTR = new GridColumns("finishedTime", "Finished time", 50, true);
    public static GridColumns PENDING_DURATION_ATTR = new GridColumns("pendingDuration", "Pending duration", 50, true);
    public static GridColumns TOTAL_DURATION_ATTR = new GridColumns("totalDuration", "Total duration", 50, true);
    
    
    @Override
    public GridColumns[] getColumns() {
    	return new GridColumns[] {ID_ATTR, STATE_ATTR, NAME_ATTR, PRIORITY_ATTR, USER_ATTR, PENDING_TASKS_ATTR, RUNNING_TASKS_ATTR, FINISHED_TASKS_ATTR, TOTAL_TASKS_ATTR,
    			SUBMITTED_TIME_ATTR, STARTED_TIME_ATTR, FINISHED_TIME_ATTR, PENDING_DURATION_ATTR, DURATION_ATTR, TOTAL_DURATION_ATTR
    	};
    }
    
    
    @Override
    public Record buildRecord(Job item) {
    	long submitTime = item.getSubmitTime();
        long startTime = item.getStartTime();
        long finishTime = item.getFinishTime();

        String pendingDuration = "";
        if (startTime > submitTime)
            pendingDuration = Job.formatDuration(startTime - submitTime);
        String execDuration = "";
        String totalDuration = "";
        if (finishTime > startTime) {
            if (startTime > 0)
                execDuration = Job.formatDuration(finishTime - startTime);
            totalDuration = Job.formatDuration(finishTime - submitTime);
        }

        /* currently displayed details */
        DetailViewerRecord curDetails = new DetailViewerRecord();
        curDetails.setAttribute(ID_ATTR.getName(), item.getId());
        curDetails.setAttribute(STATE_ATTR.getName(), item.getStatus().toString());
        curDetails.setAttribute(NAME_ATTR.getName(), item.getName());
        curDetails.setAttribute(PRIORITY_ATTR.getName(), item.getPriority().toString());
        curDetails.setAttribute(USER_ATTR.getName(), item.getUser());
        curDetails.setAttribute(PENDING_TASKS_ATTR.getName(), item.getPendingTasks());
        curDetails.setAttribute(RUNNING_TASKS_ATTR.getName(), item.getRunningTasks());
        curDetails.setAttribute(FINISHED_TASKS_ATTR.getName(), item.getFinishedTasks());
        curDetails.setAttribute(TOTAL_TASKS_ATTR.getName(), item.getTotalTasks());
        curDetails.setAttribute(SUBMITTED_TIME_ATTR.getName(), JSUtil.getTime(submitTime));
        curDetails.setAttribute(STARTED_TIME_ATTR.getName(), (startTime > submitTime) ? JSUtil.getTime(startTime) : "");
        curDetails.setAttribute(FINISHED_TIME_ATTR.getName(), (finishTime > startTime) ? JSUtil.getTime(finishTime)
                : "");
        curDetails.setAttribute(PENDING_DURATION_ATTR.getName(), pendingDuration);
        curDetails.setAttribute(DURATION_ATTR.getName(), execDuration);
        curDetails.setAttribute(TOTAL_DURATION_ATTR.getName(), totalDuration);
        
        return curDetails;
    }
}
