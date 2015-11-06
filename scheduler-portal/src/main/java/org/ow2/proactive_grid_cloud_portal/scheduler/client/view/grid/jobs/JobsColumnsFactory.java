package org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.jobs;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.Job;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.ColumnsFactory;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.GridColumns;

import com.smartgwt.client.data.Record;

/**
 * A factory that give columns and records for jobs.
 */
public class JobsColumnsFactory implements ColumnsFactory<Job>{

    public static GridColumns ID_ATTR = new GridColumns("id", "id", 80, true, true);
    public static GridColumns STATE_ATTR = new GridColumns("state", "State", 100, true, false);
    public static GridColumns USER_ATTR = new GridColumns("user","User", 140, true, false);
    public static GridColumns PROGRESS_ATTR = new GridColumns("progress", "Progress", 100, true, false);
    public static GridColumns PRIORITY_ATTR = new GridColumns("priority", "Priority",150, true, false);
    public static GridColumns DURATION_ATTR = new GridColumns("duration", "Execution duration", 100, true, false);
    public static GridColumns NAME_ATTR = new GridColumns("name", "Name", -1, true, false);
    
    @Override
    public GridColumns[] getColumns() {
        return new GridColumns[]{ID_ATTR, STATE_ATTR, USER_ATTR, PROGRESS_ATTR, PRIORITY_ATTR, DURATION_ATTR, NAME_ATTR};
    }

    
    protected void buildCommonRecordAttributes(Job item, Record record){
    	record.setAttribute(ID_ATTR.getName(), item.getId());
        
        long duration = -1;
        if (item.getFinishTime() > 0 && item.getStartTime() > 0) {
            duration = item.getFinishTime() - item.getStartTime();
        }

        record.setAttribute(STATE_ATTR.getName(), item.getStatus().toString());
        record.setAttribute(USER_ATTR.getName(), item.getUser());
        record.setAttribute(PRIORITY_ATTR.getName(), item.getPriority().toString());
        record.setAttribute(NAME_ATTR.getName(), item.getName());
        record.setAttribute(DURATION_ATTR.getName(), duration);
    }
    
    
    @Override
    public void buildRecord(Job item, Record record) {
        buildCommonRecordAttributes(item, record);
        
        float progress = (float) item.getFinishedTasks() / (float) item.getTotalTasks();
        record.setAttribute(PROGRESS_ATTR.getName(), progress);
    }
    
}
