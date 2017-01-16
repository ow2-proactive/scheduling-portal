/*
 * ProActive Parallel Suite(TM):
 * The Open Source library for parallel and distributed
 * Workflows & Scheduling, Orchestration, Cloud Automation
 * and Big Data Analysis on Enterprise Grids & Clouds.
 *
 * Copyright (c) 2007 - 2017 ActiveEon
 * Contact: contact@activeeon.com
 *
 * This library is free software: you can redistribute it and/or
 * modify it under the terms of the GNU Affero General Public License
 * as published by the Free Software Foundation: version 3 of
 * the License.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 *
 * If needed, contact us to obtain a release under GPL Version 2 or 3
 * or a different license than the AGPL.
 */
package org.ow2.proactive_grid_cloud_portal.scheduler.client.model;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.JobOutput;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.OutputMode;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.JobOutputListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerModelImpl;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SelectionTarget;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Task;


/**
 * Model for an output view.
 * @author the activeeon team.
 *
 */
public class OutputModel extends AbstractSelectedTargetModel {

    protected HashMap<String, JobOutput> outputs = null;

    protected JobOutput currentOutput = null;

    protected ArrayList<JobOutputListener> jobOutputListeners = null;

    protected OutputMode outputMode = OutputMode.LOG_OUT_ERR;

    protected boolean liveEnabled = false;

    protected boolean live = false;

    public OutputModel(SchedulerModelImpl parentModel) {
        super(parentModel);
        this.parentModel.setOutputModel(this);

        this.outputs = new HashMap<String, JobOutput>();

        this.jobOutputListeners = new ArrayList<JobOutputListener>();
    }

    /**
     * If it has been previously stored, the model may have cached the partial or
     * complete output of a given job.
     * 
     * @param jobId id of the job for which the output should be fetched
     * @return a wrapper for the job output
     */
    public JobOutput getJobOutput(String jobId, boolean createIfNotExists) {
        JobOutput result = this.outputs.get(jobId);
        if (result == null && createIfNotExists) {
            result = new JobOutput(jobId);
            this.outputs.put(jobId, result);
        }
        return result;
    }

    /**
     * Set the output for a given task in a given job
     * 
     * notify listeners
     * 
     */
    public void setTaskOutput(String jobId, Task task, String output) {
        JobOutput jobOutput = this.getJobOutput(jobId, true);
        List<String> remoteVisuLines = jobOutput.update(task, output, this.outputMode);

        processRemoteVisuLines(remoteVisuLines);

        if (this.currentOutput != null && this.currentOutput.getJobId() == jobId) {
            this.notifyUpdatedCurrentOutput();
        }
    }

    /**
     * Append a job output fragment to the stored live output
     * @param jobId id of the job to which this fragment belongs
     * @param out job output fragment
     */
    public void appendLiveOutput(String jobId, String out) {
        JobOutput jobOutput = this.getJobOutput(jobId, true);
        List<String> remoteVisuLines = jobOutput.updateLive(out);

        processRemoteVisuLines(remoteVisuLines);

        this.notifyUpdatedCurrentOutput();
    }

    protected void processRemoteVisuLines(List<String> lines) {
        for (String line : lines) {
            this.parentModel.getTasksModel().addRemoteHint(line);
        }
    }

    /**
     * Notify listeners that the output of a given job has changed
     * 
     * @param jobId the job for which the output changed
     */
    public void notifyUpdatedCurrentOutput() {
        for (JobOutputListener listener : this.jobOutputListeners) {
            listener.jobOutputUpdated(this.currentOutput, this.selectionTarget);
        }
    }

    public void notifyToggleLive(boolean newValue) {
        for (JobOutputListener listener : this.jobOutputListeners) {
            listener.liveToggled(newValue);
        }
    }

    public void notifyLiveEnabled(boolean enabled) {
        for (JobOutputListener listener : this.jobOutputListeners) {
            listener.liveEnabled(enabled);
        }
    }

    public void setLive(boolean live, boolean affectCurrentOutput) {
        if (live != this.live) {
            this.live = live;
            if (affectCurrentOutput && this.currentOutput != null) {
                this.currentOutput.setLive(live);
                this.notifyUpdatedCurrentOutput();
            }

            this.notifyToggleLive(live);
        }
    }

    public void setLiveEnabled(boolean liveEnabled, boolean affectCurrentOutput) {
        if (this.liveEnabled != liveEnabled) {
            this.liveEnabled = liveEnabled;
            this.notifyLiveEnabled(liveEnabled);

            if (affectCurrentOutput && this.currentOutput != null) {
                this.currentOutput.setLiveEnabled(this.liveEnabled);
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.ow2.proactive_grid_cloud_portal.client.EventDispatcher#addJobOutputListener(org.ow2.
     * proactive_grid_cloud_portal.client.Listeners.JobOutputListener)
     */
    public void addJobOutputListener(JobOutputListener listener) {
        this.jobOutputListeners.add(listener);
    }

    public JobOutput getCurrentOutput() {
        return currentOutput;
    }

    public void setCurrentOutput(JobOutput output) {
        this.currentOutput = output;

        if (this.currentOutput == null) {
            this.setLiveEnabled(false, false);
        } else {
            if (this.selectionTarget == SelectionTarget.JOB_TARGET) {
                boolean live = this.currentOutput.isLive();
                this.setLive(live, false);
                this.setLiveEnabled(this.currentOutput.isLiveEnabled() || live, false);
            }
        }

        this.notifyUpdatedCurrentOutput();
    }

    public OutputMode getOutputMode() {
        return outputMode;
    }

    public boolean setOutputMode(OutputMode outputMode) {
        if (this.outputMode != outputMode) {
            this.outputMode = outputMode;
            this.notifyUpdatedCurrentOutput();
            return true;
        } else {
            return false;
        }
    }

    public boolean isLiveEnabled() {
        return liveEnabled;
    }

    public boolean isLive() {
        return live;
    }

}
