package org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.Job;

import com.smartgwt.client.widgets.grid.ListGridRecord;

public class JobRecord extends ListGridRecord {

    private static final String JOB_ATTR = "job";
    
    public JobRecord(Job j) {
        setAttribute(JobsColumns.ID_ATTR.getName(), j.getId());
        update(j);
    }

    /**
     * Updates the attributes of this record to reflect the fields of the job <code>j</code>
     *
     * @param j a Job
     */
    public void update(Job j) {
        float progress = (float) j.getFinishedTasks() / (float) j.getTotalTasks();
        long duration = -1;
        if (j.getFinishTime() > 0 && j.getStartTime() > 0) {
            duration = j.getFinishTime() - j.getStartTime();
        }

        setAttribute(JobsColumns.STATE_ATTR.getName(), j.getStatus().toString());
        setAttribute(JobsColumns.USER_ATTR.getName(), j.getUser());
        setAttribute(JobsColumns.PRIORITY_ATTR.getName(), j.getPriority().toString());
        setAttribute(JobsColumns.NAME_ATTR.getName(), j.getName());
        setAttribute(JobsColumns.DURATION_ATTR.getName(), duration);
        setAttribute(JobsColumns.PROGRESS_ATTR.getName(), progress);
        setAttribute(JOB_ATTR, j);
    }
    
    public static Job getJob(ListGridRecord record){
        return (Job) record.getAttributeAsObject(JOB_ATTR);
    }
    
}
