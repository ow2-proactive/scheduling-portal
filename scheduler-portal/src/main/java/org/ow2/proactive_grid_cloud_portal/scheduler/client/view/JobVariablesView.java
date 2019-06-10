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

import org.ow2.proactive_grid_cloud_portal.scheduler.client.Job;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerController;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.JobSelectedListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerModelImpl;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.ExecutionsModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.JobsModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.KeyValueGrid;

import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.layout.VStack;


public class JobVariablesView implements JobSelectedListener {

    /** Generic information label text */
    private static final String GENERIC_INFORMATION_LABEL_TEXT = "Generic Information";

    /** Generic information grid */
    private KeyValueGrid genericInformationGrid;

    /** Variables label text */
    private static final String JOB_VARIABLES_LABEL_TEXT = "Submitted Job Variables";

    /** Variables grid */
    private KeyValueGrid variablesGrid;

    private static final String NO_JOB_SELECTED = "No job selected.";

    /**
     * label when no job is selected or job is not finished
     */
    private Label placeHolderLabel;

    private SchedulerController controller;

    public JobVariablesView(SchedulerController controller) {
        this.controller = controller;
        ExecutionsModel executionModel = ((SchedulerModelImpl) controller.getModel()).getExecutionsModel();
        JobsModel jobsModel = executionModel.getJobsModel();
        jobsModel.addJobSelectedListener(this);
    }

    /**
     * @return the Widget to display, ready to be added in a container
     */
    public Layout build() {
        VStack root = new VStack();
        root.setWidth100();

        placeHolderLabel = new Label(NO_JOB_SELECTED);
        placeHolderLabel.setWidth100();
        placeHolderLabel.setAlign(Alignment.CENTER);
        root.addMember(placeHolderLabel);

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

    @Override
    public void jobSelected(Job job) {
        displayMembers(job);
    }

    @Override
    public void jobUnselected() {
        showNoJobSelected();
    }

    @Override
    public void selectedJobUpdated(Job job) {
        displayMembers(job);
    }

    protected void displayMembers(Job job) {
        variablesGrid.buildEntries(job.getVariables());
        genericInformationGrid.buildEntries(job.getGenericInformation());

        this.variablesGrid.show();
        this.genericInformationGrid.show();
    }

    private void showNoJobSelected() {
        placeHolderLabel.setContents(NO_JOB_SELECTED);
        placeHolderLabel.show();
        this.genericInformationGrid.hide();
        this.variablesGrid.hide();
    }

}
