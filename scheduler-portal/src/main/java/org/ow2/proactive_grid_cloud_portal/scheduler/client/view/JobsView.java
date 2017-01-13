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
import org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.PaginationController;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.jobs.JobsListGrid;

import com.smartgwt.client.widgets.layout.Layout;


/**
 * Contains the ListGrid that displays jobs
 */
public class JobsView extends FilteringGridItemView implements JobsUpdatedListener {

    private JobsController controller = null;

    /**
     * @param controller Controller used to create this view
     */
    public JobsView(JobsController controller) {
        this.controller = controller;
        this.controller.getModel().addJobsUpdatedListener(this);
        this.hasToolBar = false;
        this.itemName = "jobs";
    }

    public void jobsUpdating() {
        this.itemUpdating();
    }

    public void jobSubmitted(Job j) {

    }

    @Override
    public void jobsUpdated(Map<Integer, Job> jobs, long totalJobs) {
        this.itemUpdated();
    }

    @Override
    protected Layout buildPagination() {
        PaginationController paginationController = this.controller.getPaginationController();
        return this.buildPagination(paginationController);
    }

    @Override
    protected void buildGrid() {
        this.itemsGrid = new JobsListGrid(this.controller);
        this.itemsGrid.build();
    }

    @Override
    public void pageChanged() {
        this.controller.selectJob(null);
    }

    @Override
    public void totalItemChanged() {
        // TODO Auto-generated method stub

    }

}
