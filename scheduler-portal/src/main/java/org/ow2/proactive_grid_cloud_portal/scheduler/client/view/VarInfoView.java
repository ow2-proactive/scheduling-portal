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
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerController;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.ExecutionDisplayModeListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.JobSelectedListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerModelImpl;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.ExecutionListMode;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.ExecutionsModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.JobsModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.KeyValueGrid;

import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VStack;


/**
 * Displays Job Variables and Generic Info about the currently selected job
 */
public class VarInfoView implements JobSelectedListener, ExecutionDisplayModeListener {

    protected SchedulerController controller;

    /** Generic information label text */
    private static final String GENERIC_INFORMATION_LABEL_TEXT = "Generic Information";

    /** Variables label text */
    private static final String JOB_VARIABLES_LABEL_TEXT = "Submitted Job Variables";

    /** Label to show that now Job Is selected */
    private Label label;

    /** Generic information grid */
    private KeyValueGrid genericInformationGrid;

    /** Variables grid */
    private KeyValueGrid variablesGrid;

    /**
     * @param controller the Controller that created this View
     */
    public VarInfoView(SchedulerController controller) {
        this.controller = controller;

        ExecutionsModel executionModel = ((SchedulerModelImpl) controller.getModel()).getExecutionsModel();
        JobsModel jobsModel = executionModel.getJobsModel();
        jobsModel.addJobSelectedListener(this);

    }

    public Layout build() {
        return getLayout();
    }

    @Override
    public void jobSelected(Job job) {
        variablesGrid.buildEntries(job.getVariables());
        genericInformationGrid.buildEntries(job.getGenericInformation());

        label.hide();
        variablesGrid.show();
        genericInformationGrid.show();
    }

    public void jobUnselected() {
        label.show();
        variablesGrid.hide();
        genericInformationGrid.hide();
    }

    @Override
    public void selectedJobUpdated(Job job) {
        jobSelected(job);
    }

    @Override
    public void modeSwitched(ExecutionListMode mode) {
        Job job = controller.getSelectedJob();
        if (job == null) {
            jobUnselected();
        } else {
            jobSelected(job);
        }
    }

    protected Layout getLayout() {
        VStack root = new VStack();
        root.setWidth100();

        label = new Label("No job selected");
        label.setWidth100();
        label.setAlign(Alignment.CENTER);
        root.addMember(label);

        variablesGrid = new KeyValueGrid(JOB_VARIABLES_LABEL_TEXT);
        variablesGrid.showTopMargin();
        variablesGrid.setWidth100();
        variablesGrid.hide();
        root.addMember(variablesGrid);

        genericInformationGrid = new KeyValueGrid(GENERIC_INFORMATION_LABEL_TEXT);
        genericInformationGrid.showTopMargin();
        genericInformationGrid.setWidth100();
        genericInformationGrid.hide();
        root.addMember(genericInformationGrid);

        return root;
    }
}
