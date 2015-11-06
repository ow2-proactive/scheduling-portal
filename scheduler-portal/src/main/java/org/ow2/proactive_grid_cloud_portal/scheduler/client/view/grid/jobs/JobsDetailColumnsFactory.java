package org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.jobs;

import org.ow2.proactive_grid_cloud_portal.common.client.JSUtil;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Job;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.GridColumns;

import com.smartgwt.client.data.Record;
import com.smartgwt.client.widgets.viewer.DetailViewerRecord;

public class JobsDetailColumnsFactory extends JobsColumnsFactory{

    public static GridColumns PENDING_TASKS_ATTR = new GridColumns("pendingTasks", "Pending tasks", 20, true, false);
    public static GridColumns RUNNING_TASKS_ATTR = new GridColumns("runningTasks", "Running tasks", 20, true, false);
    public static GridColumns FINISHED_TASKS_ATTR = new GridColumns("finishedTasks", "Finished tasks", 20, true, false);
    public static GridColumns TOTAL_TASKS_ATTR = new GridColumns("totalTasks", "Total tasks", 20, true, false);
    public static GridColumns SUBMITTED_TIME_ATTR = new GridColumns("submittedTime", "Submitted time", 50, true, false);
    public static GridColumns STARTED_TIME_ATTR = new GridColumns("startedTime", "Started time", 50, true, false);
    public static GridColumns FINISHED_TIME_ATTR = new GridColumns("finishedTime", "Finished time", 50, true, false);
    public static GridColumns PENDING_DURATION_ATTR = new GridColumns("pendingDuration", "Pending duration", 50, true, false);
    public static GridColumns TOTAL_DURATION_ATTR = new GridColumns("totalDuration", "Total duration", 50, true, false);
    
    
    @Override
    public GridColumns[] getColumns() {
    	return new GridColumns[] {ID_ATTR, STATE_ATTR, NAME_ATTR, PRIORITY_ATTR, USER_ATTR, PENDING_TASKS_ATTR, RUNNING_TASKS_ATTR, FINISHED_TASKS_ATTR, TOTAL_TASKS_ATTR,
    			SUBMITTED_TIME_ATTR, STARTED_TIME_ATTR, FINISHED_TIME_ATTR, PENDING_DURATION_ATTR, DURATION_ATTR, TOTAL_DURATION_ATTR
    	};
    }
    
    
    @Override
    public void buildRecord(Job item, Record record) {
    	super.buildCommonRecordAttributes(item, record);
    	
    	long submitTime = item.getSubmitTime();
        long startTime = item.getStartTime();
        long finishTime = item.getFinishTime();

        String pendingDuration = "";
        if (startTime > submitTime)
            pendingDuration = Job.formatDuration(startTime - submitTime);
   
        String totalDuration = "";
        if (finishTime > startTime) {
            totalDuration = Job.formatDuration(finishTime - submitTime);
        }

        /* currently displayed details */
        //DetailViewerRecord curDetails = new DetailViewerRecord();
        record.setAttribute(PENDING_TASKS_ATTR.getName(), item.getPendingTasks());
        record.setAttribute(RUNNING_TASKS_ATTR.getName(), item.getRunningTasks());
        record.setAttribute(FINISHED_TASKS_ATTR.getName(), item.getFinishedTasks());
        record.setAttribute(TOTAL_TASKS_ATTR.getName(), item.getTotalTasks());
        record.setAttribute(SUBMITTED_TIME_ATTR.getName(), JSUtil.getTime(submitTime));
        record.setAttribute(STARTED_TIME_ATTR.getName(), (startTime > submitTime) ? JSUtil.getTime(startTime) : "");
        record.setAttribute(FINISHED_TIME_ATTR.getName(), (finishTime > startTime) ? JSUtil.getTime(finishTime)
                : "");
        record.setAttribute(PENDING_DURATION_ATTR.getName(), pendingDuration);
        
        record.setAttribute(TOTAL_DURATION_ATTR.getName(), totalDuration);
    }
}
