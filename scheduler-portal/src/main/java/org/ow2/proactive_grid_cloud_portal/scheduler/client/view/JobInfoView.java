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
package org.ow2.proactive_grid_cloud_portal.scheduler.client.view;

import java.util.Map;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.Job;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerController;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.ExecutionDisplayModeListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.JobSelectedListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.JobsUpdatedListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerModelImpl;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.ExecutionListMode;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.JobsModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.ColumnsFactory;


/**
 * Displays detailed info about the currently selected job
 *
 *
 * @author the activeeon team
 *
 */
public class JobInfoView extends InfoView<Job> implements JobSelectedListener, JobsUpdatedListener, ExecutionDisplayModeListener {

    /**
     * @param controller the Controller that created this View
     */
    public JobInfoView(SchedulerController controller, ColumnsFactory<Job> factory) {
        super(factory, "No job selected");
    	JobsModel jobsModel = ((SchedulerModelImpl) controller.getModel()).getExecutionsModel().getJobsModel();
        jobsModel.addJobSelectedListener(this);
        jobsModel.addJobsUpdatedListener(this);
    }

    
    public void jobSelected(Job job) {
        this.displayedItem = job;
        this.displayItem();
    }

    public void jobsUpdating() {
    }

    public void jobSubmitted(Job j) {
    }

    public void jobsUpdated(Map<Integer, Job> jobs) {
        if (this.displayedItem == null)
            return;

        for (Job j : jobs.values()) {
            if (j.getId().equals(this.displayedItem.getId())) {
                jobSelected(j);
            }
        }
    }

    public void jobUnselected() {
    	this.hideDetails();
    }
    

    @Override
    public void selectedJobUpdated() {
        // TODO Auto-generated method stub
    }


	@Override
	public void modeSwitched(ExecutionListMode mode) {
		// TODO Auto-generated method stub
		
	}

}
