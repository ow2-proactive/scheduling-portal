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
import org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.JobsPaginationController;

import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;


public class JobsPaginationView extends PaginationView<JobsPaginationController> implements JobsUpdatedListener {

    /**
     * Label that indicates the max number of jobs
     */
    private Label jobsTotalLabel = null;

    private static String JOBS_TOTAL_TEXT = "Jobs (Total or Filtered): ";

    public JobsPaginationView(JobsController controller) {
        super(controller.getPaginationController());
        this.paginationController.getModel().addPaginationListener(this);
        controller.getModel().addJobsUpdatedListener(this);
    }

    private void setTotalJobsLabel() {
        jobsTotalLabel.setContents(JOBS_TOTAL_TEXT + paginationController.getModel().getTotalItems());
    }

    /**
     * Builds the view content.
     * @return a layout containing the view content.
     */
    @Override
    public Layout buildLayout() {

        this.jobsTotalLabel = new Label();
        setTotalJobsLabel();
        this.jobsTotalLabel.setAlign(Alignment.CENTER);
        this.jobsTotalLabel.setWidth100();
        this.jobsTotalLabel.setMinWidth(40);
        this.jobsTotalLabel.setMargin(4);

        HLayout labelLayout = new HLayout();
        labelLayout.addStyleName("labelPaginationLayout");
        labelLayout.addMember(this.jobsTotalLabel);

        ToolStrip paginationLayout = getToolStripPaginationLayout();

        this.pageFirstButton.setTitle("<< Newest Jobs");
        this.pagePreviousButton.setTitle("< Newer Jobs");
        this.pageNextButton.setTitle("Older Jobs >");
        this.pageLastButton.setTitle("Oldest Jobs >>");

        paginationLayout.addMember(this.pageFirstButton);
        paginationLayout.addMember(this.pagePreviousButton);
        paginationLayout.addMember(labelLayout);
        paginationLayout.addMember(this.pageLastButton);
        paginationLayout.addMember(this.pageNextButton);

        return paginationLayout;
    }

    @Override
    public void pageChanged() {
        this.disableAllControls();
        this.enablePaginationControls();
    }

    @Override
    public void totalItemChanged() {
        setTotalJobsLabel();
        pageChanged();
    }

    protected void itemsUpdated() {
        totalItemChanged();
    }

    @Override
    public void jobsUpdated(Map<Integer, Job> jobs) {
        itemsUpdated();
    }

    @Override
    public void jobsUpdating() {

    }

    @Override
    public void jobSubmitted(Job j) {
    }

}
