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
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.JobsUpdatedListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.JobsController;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.PaginationController;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.jobs.JobsListGrid;

import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;


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
    }

    public void jobsUpdating() {
        this.itemUpdating();
    }

    public void jobSubmitted(Job j) {
        
    }

    public void jobsUpdated(Map<Integer, Job> jobs) {
        this.itemUpdated();
    }


    @Override
    protected Layout buildToolbar() {
        ToolStrip toolbar = new ToolStrip();
        toolbar.addStyleName("itemViewNav");
        toolbar.setHeight(34);
        toolbar.setWidth100();
        toolbar.setBackgroundImage("");
        toolbar.setBackgroundColor("#fafafa");
        toolbar.setBorder("0px");
        
        return toolbar;
    }

    @Override
    protected Layout buildPagination() {
        PaginationController paginationController =  this.controller.getPaginationController();
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
