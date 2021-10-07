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

import java.util.*;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.Job;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerController;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.ExecutionDisplayModeListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.JobSelectedListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerModelImpl;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.ExecutionListMode;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.ExecutionsModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.JobsModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.KeyValueGrid;

import com.google.gwt.i18n.client.HasDirection;
import com.google.gwt.user.client.ui.CheckBox;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.VerticalAlignment;
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

    /** Submitted Job Variables label */
    private Label submittedJobVariablesLabel;

    /** Advanced checkBox value */
    private boolean advanced = false;

    /** Hidden checkBox value */
    private boolean hidden = false;

    /** CheckBox layout*/
    private Layout checkBoxLayout;

    /** Generic information grid */
    private KeyValueGrid genericInformationGrid;

    /** List of variables grids */
    private List<KeyValueGrid> variablesGrids;

    private VStack root;

    /** Currently selected job */
    private Job selectedJob;

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
        controller.getExecutionController()
                  .getJobsController()
                  .checkJobPermissionMethod(job, label, variablesGrids, genericInformationGrid);

        controller.setJobDetailedVariables(job, this);
    }

    public void buildVariablesEntries(Job job) {
        selectedJob = job;
        variablesGrids.forEach(variablesGrid -> root.removeChild(variablesGrid));
        variablesGrids.clear();
        root.removeChild(genericInformationGrid);
        root.removeChild(checkBoxLayout);
        root.redraw();
        root.addMember(checkBoxLayout);
        Map<String, Set<String>> variablesByGroup = getVariablesByGroup(job);
        buildDetailedVariablesEntries(job, variablesByGroup);
        genericInformationGrid.buildEntries(job.getGenericInformation());
        root.addMember(genericInformationGrid);
        label.hide();
        submittedJobVariablesLabel.show();
        checkBoxLayout.show();
        genericInformationGrid.show();
    }

    private void buildDetailedVariablesEntries(Job job, Map<String, Set<String>> variablesByGroup) {
        variablesByGroup.keySet().forEach(group -> {
            KeyValueGrid variablesGrid = new KeyValueGrid("<u>" + group + "</u>");
            variablesGrid.showTopMargin();
            variablesGrid.setWidth100();
            variablesGrids.add(variablesGrid);
            root.addMember(variablesGrid);
            Map<String, String> variables = new HashMap<>();
            variablesByGroup.get(group)
                            .forEach(variableName -> variables.put(variableName, job.getVariables().get(variableName)));
            variablesGrid.buildEntries(variables);
            variablesGrid.setVariableDescription(job.getDetailedVariables());
            variablesGrid.show();
        });
    }

    private Map<String, Set<String>> getVariablesByGroup(Job job) {
        Map<String, Set<String>> variablesByGroup = new TreeMap<>();
        job.getDetailedVariables().keySet().forEach(variableName -> {
            if ((!Boolean.valueOf(job.getDetailedVariables().get(variableName).get("advanced")) ||
                 Boolean.valueOf(job.getDetailedVariables().get(variableName).get("advanced")) == advanced) &&
                (!Boolean.valueOf(job.getDetailedVariables().get(variableName).get("hidden")) ||
                 Boolean.valueOf(job.getDetailedVariables().get(variableName).get("hidden")) == hidden)) {
                String group = job.getDetailedVariables().get(variableName).get("group");
                Set<String> variableNames = variablesByGroup.containsKey(group) ? variablesByGroup.get(group)
                                                                                : new HashSet<>();
                variableNames.add(variableName);
                variablesByGroup.put(group, variableNames);
            }
        });
        return variablesByGroup;
    }

    public void jobUnselected() {
        label.show();
        submittedJobVariablesLabel.hide();
        checkBoxLayout.hide();
        variablesGrids.clear();
        variablesGrids.forEach(variablesGrid -> variablesGrid.hide());
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
        root = new VStack();
        root.setWidth100();

        label = new Label("No job selected");
        label.setWidth100();
        label.setAlign(Alignment.CENTER);
        root.addMember(label);

        submittedJobVariablesLabel = new Label("<b><u>" + JOB_VARIABLES_LABEL_TEXT + "</u></b>");
        submittedJobVariablesLabel.setValign(VerticalAlignment.BOTTOM);
        submittedJobVariablesLabel.setAutoHeight();
        submittedJobVariablesLabel.hide();
        root.addMember(submittedJobVariablesLabel);

        checkBoxLayout = new Layout();
        checkBoxLayout.setAlign(Alignment.RIGHT);
        checkBoxLayout.setPadding(0);
        checkBoxLayout.setAutoHeight();

        CheckBox advancedCheckBox = new CheckBox("Advanced", HasDirection.Direction.RTL);
        advancedCheckBox.setValue(advanced);
        advancedCheckBox.addClickHandler(event -> {
            advanced = advancedCheckBox.getValue();
            buildVariablesEntries(selectedJob);
        });
        checkBoxLayout.addMember(advancedCheckBox);

        CheckBox hiddenCheckBox = new CheckBox("Hidden", HasDirection.Direction.RTL);
        hiddenCheckBox.setValue(hidden);
        hiddenCheckBox.addClickHandler(event -> {
            hidden = hiddenCheckBox.getValue();
            buildVariablesEntries(selectedJob);
        });
        checkBoxLayout.addMember(hiddenCheckBox);
        checkBoxLayout.hide();
        root.addMember(checkBoxLayout);

        variablesGrids = new ArrayList<>();

        genericInformationGrid = new KeyValueGrid("<b><u>" + GENERIC_INFORMATION_LABEL_TEXT + "</u></b><br>");
        genericInformationGrid.showTopMargin();
        genericInformationGrid.setWidth100();
        genericInformationGrid.hide();
        root.addMember(genericInformationGrid);

        return root;
    }
}
