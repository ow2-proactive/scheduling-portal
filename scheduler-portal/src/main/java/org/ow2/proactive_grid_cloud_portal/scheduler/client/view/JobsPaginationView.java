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
package org.ow2.proactive_grid_cloud_portal.scheduler.client.view;

import java.util.Map;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.Job;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.JobsUpdatedListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.JobsController;


public class JobsPaginationView extends PaginationView implements JobsUpdatedListener {

    public JobsPaginationView(JobsController controller) {
        this.itemTypeName = "Jobs";
        this.paginationController = controller.getPaginationController();
        this.paginationController.getModel().addPaginationListener(this);
        controller.getModel().addJobsUpdatedListener(this);
    }

    @Override
    public void jobsUpdated(Map<Integer, Job> jobs, long totalJobs) {
        itemsUpdated(totalJobs);
    }

    @Override
    public void jobsUpdating() {
        // TODO Auto-generated method stub

    }

    @Override
    public void jobSubmitted(Job j) {
        // TODO Auto-generated method stub

    }

}
