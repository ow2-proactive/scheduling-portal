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

import org.ow2.proactive_grid_cloud_portal.scheduler.client.Job;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.JobSelectedListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerModelImpl;


public class TasksCentricModel extends TasksModel {

    protected Job selectedTaskJob;

    /**
     * Listener for the job selection changes.
     */
    private ArrayList<JobSelectedListener> jobSelectedListeners = null;

    public TasksCentricModel(SchedulerModelImpl parentModel) {
        super(parentModel);
        this.jobSelectedListeners = new ArrayList<>();
    }

    @Override
    protected void initNavigationModel() {
        this.tasksNavigationModel = new TasksCentricNavigationModel(this);
    }

    public void setTaskSelectedJob(Job job) {
        boolean selJobChanged = !((job == null && this.selectedTaskJob == null) || job.equals(this.selectedTaskJob));
        this.selectedTaskJob = job;
        if (selJobChanged) {
            for (JobSelectedListener listener : this.jobSelectedListeners) {
                if (this.selectedTaskJob != null) {
                    listener.jobSelected(this.selectedTaskJob);
                } else {
                    listener.jobUnselected();
                }
            }
        }
    }

    public void addJobSelectedListener(JobSelectedListener listener) {
        this.jobSelectedListeners.add(listener);
    }

    public Job getSelectedTaskJob() {
        return selectedTaskJob;
    }

}
