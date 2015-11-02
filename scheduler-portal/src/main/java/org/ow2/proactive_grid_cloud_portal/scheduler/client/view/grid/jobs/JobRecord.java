package org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.jobs;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.Job;

import com.smartgwt.client.widgets.grid.ListGridRecord;

/**
 * A list grid record that embeds a job.
 * @author The activeeon team.
 *
 */
public class JobRecord extends ListGridRecord {

    /**
     * the attribute to store the job.
     */
    private static final String JOB_ATTR = "job";
    
    public JobRecord(Job j) {
        setAttribute(JOB_ATTR, j);  
    }
    
    /**
     * Gets the job stored in a job record.
     * @param record the record that contains the job.
     * @return the stored job.
     */
    public static Job getJob(ListGridRecord record){
        return (Job) record.getAttributeAsObject(JOB_ATTR);
    }
    
}
