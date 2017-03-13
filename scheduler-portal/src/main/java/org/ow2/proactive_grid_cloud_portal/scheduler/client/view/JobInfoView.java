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
import java.util.Set;
import java.util.logging.Logger;

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
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.GridColumns;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.jobs.GenericInformationColumnsFactory;

import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VStack;


/**
 * Displays detailed info about the currently selected job
 *
 *
 * @author the activeeon team
 *
 */
public class JobInfoView extends InfoView<Job>
        implements JobSelectedListener, JobsUpdatedListener, ExecutionDisplayModeListener {

    protected SchedulerController controller;

    /** generic information table */
    private ListGrid genericInformationGrid = null;

    /** Generic information column factory */
    private GenericInformationColumnsFactory genericInformationColumnsFactory;

    /** Generic information label text */
    private static final String GENERIC_INFORMATION_LABEL_TEXT = "Generic information";

    /** Generic information label */
    private Label genericInformationLabel = null;

    /**
     * @param controller the Controller that created this View
     */
    public JobInfoView(SchedulerController controller, ColumnsFactory<Job> factory) {
        super(factory, "No job selected");
        this.controller = controller;
        this.genericInformationColumnsFactory = new GenericInformationColumnsFactory();
        ExecutionsModel executionModel = ((SchedulerModelImpl) controller.getModel()).getExecutionsModel();
        JobsModel jobsModel = executionModel.getJobsModel();
        jobsModel.addJobSelectedListener(this);
        jobsModel.addJobsUpdatedListener(this);

        executionModel.getTasksModel().addJobSelectedListener(this);
        executionModel.addExecutionsDisplayModeListener(this);
    }

    public void jobSelected(Job job) {
        this.displayedItem = job;
        this.displayItem();
    }

    public void jobsUpdating() {
    }

    public void jobSubmitted(Job j) {
    }

    @Override
    public void jobsUpdated(Map<Integer, Job> jobs, long totalJobs) {
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

        Layout root = new VStack();
        root.setWidth100();
        root.setHeight100();

        this.genericInformationLabel = new Label(GENERIC_INFORMATION_LABEL_TEXT);
        this.genericInformationLabel.setHeight("10%");
        this.genericInformationLabel.hide();

        this.genericInformationGrid = new ListGrid();
        this.genericInformationGrid.setWidth100();

        GridColumns[] columns = this.genericInformationColumnsFactory.getColumns();
        ListGridField[] fields = new ListGridField[columns.length];
        for (int i = 0; i < columns.length; i++) {
            GridColumns column = columns[i];
            fields[i] = new ListGridField(column.getName(), column.getTitle());
        }

        this.genericInformationGrid.setFields(fields);
        this.genericInformationGrid.hide();

        root.addMember(super.getLayout());
        root.addMember(genericInformationLabel);
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
        Set<Map.Entry<String, String>> genericInformationSet = job.getGenericInformation().entrySet();
        ListGridRecord[] records = new ListGridRecord[genericInformationSet.size()];

        int index = 0;
        for (Map.Entry<String, String> genericInformationEntry : genericInformationSet) {
            ListGridRecord record = new ListGridRecord();
            this.genericInformationColumnsFactory.buildRecord(genericInformationEntry, record);
            records[index] = record;
            index++;
        }

        this.genericInformationGrid.setData(records);
        this.genericInformationGrid.show();
        this.genericInformationLabel.show();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.ow2.proactive_grid_cloud_portal.scheduler.client.view.InfoView#hideExtraMembers()
     */
    @Override
    protected void hideExtraMembers() {
        this.genericInformationGrid.hide();
        this.genericInformationLabel.hide();
    }

}
