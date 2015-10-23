package org.ow2.proactive_grid_cloud_portal.scheduler.client.json;

import java.util.LinkedHashMap;
import java.util.Map;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.Job;

public class JSONPaginatedJobs {

    protected Map<Integer, Job> jobs;
    
    protected long revision;
    
    protected long total;

    public JSONPaginatedJobs() {
        this.jobs = new LinkedHashMap<Integer, Job>();
    }
    
    
    public Map<Integer, Job> getJobs() {
        return jobs;
    }

    public void setJobs(Map<Integer, Job> jobs) {
        this.jobs = jobs;
    }

    public long getRevision() {
        return revision;
    }

    public void setRevision(long revision) {
        this.revision = revision;
    }

    public long getTotal() {
        return total;
    }

    public void setTotal(long total) {
        this.total = total;
    }

    
    
}
