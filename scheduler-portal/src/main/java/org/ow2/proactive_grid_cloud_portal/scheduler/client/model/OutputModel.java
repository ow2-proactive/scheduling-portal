/*
 * ################################################################
 *
 * ProActive Parallel Suite(TM): The Java(TM) library for
 *    Parallel, Distributed, Multi-Core Computing for
 *    Enterprise Grids & Clouds
 *
 * Copyright (C) 1997-2011 INRIA/University of
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
 * ################################################################
 * $$PROACTIVE_INITIAL_DEV$$
 */

package org.ow2.proactive_grid_cloud_portal.scheduler.client.model;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.Job;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.JobOutput;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.JobStatus;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Task;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.JobOutputListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerModelImpl;

import com.google.gwt.safehtml.shared.SafeHtmlUtils;


/**
 * Model for an output view.
 * @author the actieveon team.
 *
 */
public class OutputModel {

    private static final String PLATFORM_INDEPENDENT_LINE_BREAK = "\r\n?|\n";
    
    private HashMap<Integer, JobOutput> output = null;
    private HashSet<String> isLiveOutput = null;
    private HashMap<String, StringBuffer> liveOutput = null;
    
    private ArrayList<JobOutputListener> jobOutputListeners = null;
    
    
    protected SchedulerModelImpl parentModel; 
    
    public OutputModel(SchedulerModelImpl parentModel) {
        this.parentModel = parentModel;
        this.parentModel.setOutputModel(this);
        
        this.output = new HashMap<Integer, JobOutput>();
        this.isLiveOutput = new HashSet<String>();
        this.liveOutput = new HashMap<String, StringBuffer>();
        
        this.jobOutputListeners = new ArrayList<JobOutputListener>();
    }
    
    /**
     * If it has been previously stored, the model may have cached the partial or
     * complete output of a given job.
     * 
     * @param jobId id of the job for which the output should be fetched
     * @return a wrapper for the job output
     */
    public JobOutput getJobOutput(int jobId, boolean createIfNotExists) {
        JobOutput result = this.output.get(jobId);
        if(result == null && createIfNotExists){
            result = new JobOutput(jobId);
            this.output.put(jobId, result);
        }
        return result;
    }
    
    /**
     * Set the output for a given task in a given job
     * 
     * notify listeners
     * 
     */
    public void setTaskOutput(int jobId, Task task, String output) {
        List<String> lines = new ArrayList<String>();

        for (String line : lineByLine(output)) {
            addRemoteHintIfNecessary(line);
            line = formatLine(line);

            if (!line.trim().isEmpty()) {
                lines.add(line);
            }
        }

        if (this.output.get(jobId) == null) {
            JobOutput jo = new JobOutput(jobId);
            jo.update(task, lines);
            this.output.put(jobId, jo);
        } else {
            this.output.get(jobId).update(task, lines);
        }

        this.updateOutput(jobId);
    }
    
    
    
    /**
     * Notify listeners that the output of a given job has changed
     * 
     * @param jobId the job for which the output changed
     */
    public void updateOutput(int jobId) {
        if (this.output.get(jobId) == null) {
            JobOutput jo = new JobOutput(jobId);
            this.output.put(jobId, jo);
        }

        for (JobOutputListener listener : this.jobOutputListeners) {
            listener.jobOutputUpdated(this.output.get(jobId));
        }
    }

    /**
     * Append a job output fragment to the stored live output
     * @param jobId id of the job to which this fragment belongs
     * @param out job output fragment
     */
    public void appendLiveOutput(String jobId, String out) {
        String[] expl = lineByLine(out);
        out = "";
        for (String str : expl) {
            addRemoteHintIfNecessary(str);
            out += formatLine(str);
        }

        StringBuffer buf = this.liveOutput.get(jobId);
        if (buf == null) {
            buf = new StringBuffer();
            this.liveOutput.put(jobId, buf);
        }
        buf.append(out);

        for (JobOutputListener list : this.jobOutputListeners) {
            list.liveOutputUpdated(jobId, buf.toString());
        }
    }
    
    
    /**
     * The locally stored live log for the given job
     * @param jobId id of the job
     * @return output of the given job, as stored locally. may not contain
     *  the actual output fully
     */
    public String getLiveOutput(String jobId) {
        StringBuffer buf = this.liveOutput.get(jobId);
        if (buf == null) {
            return "";
        } else {
            return buf.toString();
        }
    }

    
    /**
     * @param jobId the id of a job
     * @return true if the specified job's output is streamed
     */
    public boolean isLiveOutput(String jobId) {
        return this.isLiveOutput.contains(jobId);
    }
    
    
    /**
     * The output for this job should be fetched live
     * @param jobId id of the job
     * @param isLiveOutput true to live fetch
     */
    public void setLiveOutput(String jobId, boolean isLiveOutput) {
        if (isLiveOutput) {
            this.isLiveOutput.add(jobId);
            if(!liveOutput.containsKey(jobId)){
                this.liveOutput.put(jobId, new StringBuffer());
            }
        } else {
            this.isLiveOutput.remove(jobId);
        }
    }
    
    
    private String[] lineByLine(String lines) {
        return lines.split(PLATFORM_INDEPENDENT_LINE_BREAK);
    }

    private void addRemoteHintIfNecessary(String line) {
        if (line.contains(TasksModel.PA_REMOTE_CONNECTION)) {
            parentModel.getTasksModel().addRemoteHint(line);
        }
    }
    
    private String formatLine(String str) {
        if (str.matches("\\[.*\\].*")) {
            str = SafeHtmlUtils.htmlEscape(str).replaceFirst("]", "]</span>");
            return "<nobr><span style='color:gray;'>" + str +"</nobr><br>";
        }
        return "";
    }
    
    
    /*
     * (non-Javadoc)
     * @see org.ow2.proactive_grid_cloud_portal.client.EventDispatcher#addJobOutputListener(org.ow2.proactive_grid_cloud_portal.client.Listeners.JobOutputListener)
     */
    public void addJobOutputListener(JobOutputListener listener) {
        this.jobOutputListeners.add(listener);
    }

    public SchedulerModelImpl getParentModel() {
        return parentModel;
    }
    
}
