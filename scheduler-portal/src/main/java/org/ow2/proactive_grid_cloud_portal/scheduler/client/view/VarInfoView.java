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

import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VStack;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Job;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerController;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.ExecutionDisplayModeListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.JobSelectedListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.JobsUpdatedListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerModelImpl;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.ExecutionListMode;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.ExecutionsModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.JobsModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.ColumnsFactory;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.KeyValueGrid;

import java.util.Map;


/**
 * Displays detailed info about the currently selected job
 *
 *
 * @author the activeeon team
 *
 */
public class VarInfoView extends InfoView<Job>
        implements JobSelectedListener, JobsUpdatedListener, ExecutionDisplayModeListener {

    protected SchedulerController controller;

    /** Generic information label text */
    private static final String GENERIC_INFORMATION_LABEL_TEXT = "Generic Information";

    /** Variables label text */
    private static final String JOB_VARIABLES_LABEL_TEXT = "Submitted Job Variables";

    /** Generic information grid */
    private KeyValueGrid genericInformationGrid;

    /** Variables grid */
    private KeyValueGrid variablesGrid;

    /**
     * @param controller the Controller that created this View
     */
    public VarInfoView(SchedulerController controller, ColumnsFactory<Job> factory) {
        super(factory, "No job selected");
        this.controller = controller;
        ExecutionsModel executionModel = ((SchedulerModelImpl) controller.getModel()).getExecutionsModel();
        JobsModel jobsModel = executionModel.getJobsModel();
        jobsModel.addJobSelectedListener(this);
        jobsModel.addJobsUpdatedListener(this);

        executionModel.getTasksModel().addJobSelectedListener(this);
        executionModel.addExecutionsDisplayModeListener(this);
    }

    @Override
    public Layout build() {
        return getLayout();
    }

    public void jobSelected(Job job) {
        this.displayedItem = job;
        this.displayItem();
    }

    @Override
    public void displayItem() {
        displayExtraMembers(this.displayedItem);

        this.label.hide();

    }

    public void jobsUpdating() {
    }

    public void jobSubmitted(Job j) {
    }

    @Override
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
        this.displayedItem = null;
        this.hideDetails();
    }

    @Override
    public void selectedJobUpdated(Job job) {
        jobSelected(job);
    }

    @Override
    public void modeSwitched(ExecutionListMode mode) {
        Job job = this.controller.getSelectedJob();
        if (job == null) {
            this.jobUnselected();
        } else {
            jobSelected(job);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.ow2.proactive_grid_cloud_portal.scheduler.client.view.InfoView#addRootExtraMembers(com.
     * smartgwt.client.widgets.layout.Layout)
     */
    @Override
    protected Layout getLayout() {

        VStack root = new VStack();
        root.setWidth100();

        this.label = new Label(this.emptyMessage);
        this.label.setWidth100();
        this.label.setAlign(Alignment.CENTER);

        root.addMember(label);

        this.genericInformationGrid = new KeyValueGrid(GENERIC_INFORMATION_LABEL_TEXT);
        this.genericInformationGrid.showTopMargin();
        this.genericInformationGrid.setWidth100();
        this.genericInformationGrid.hide();

        this.variablesGrid = new KeyValueGrid(JOB_VARIABLES_LABEL_TEXT);
        this.variablesGrid.showTopMargin();
        this.variablesGrid.setWidth100();
        this.variablesGrid.hide();

        root.addMember(variablesGrid);
        root.addMember(genericInformationGrid);
        return root;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.ow2.proactive_grid_cloud_portal.scheduler.client.view.InfoView#displayExtraMembers(
     * boolean)
     */
    @Override
    protected void displayExtraMembers(Job job) {
        genericInformationGrid.buildEntries(job.getGenericInformation());
        variablesGrid.buildEntries(job.getVariables());

        this.genericInformationGrid.show();
        this.variablesGrid.show();
        this.label.hide();
    }



    /*
     * (non-Javadoc)
     * 
     * @see org.ow2.proactive_grid_cloud_portal.scheduler.client.view.InfoView#hideExtraMembers()
     */
    @Override
    protected void hideExtraMembers() {
        this.genericInformationGrid.hide();
        this.variablesGrid.hide();
        this.label.show();
    }

}
