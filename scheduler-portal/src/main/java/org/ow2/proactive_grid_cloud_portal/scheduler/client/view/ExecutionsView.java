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

import java.util.Arrays;

import org.ow2.proactive_grid_cloud_portal.common.client.Images;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LoginModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.ExecutionDisplayModeListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.ExecutionListMode;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.ExecutionsController;

import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.LayoutSpacer;
import com.smartgwt.client.widgets.layout.SectionStackSection;


public class ExecutionsView implements ExecutionDisplayModeListener {

    protected ExecutionsController controller;

    protected Layout jobsPane;

    protected Layout tasksPane;

    protected CheckboxItem chkMy;

    protected CheckboxItem chkPending;

    protected CheckboxItem chkRunning;

    protected CheckboxItem chkFinished;

    protected IButton filter;

    public ExecutionsView(ExecutionsController controller) {
        this.controller = controller;
        this.controller.getModel().addExecutionsDisplayModeListener(this);
    }

    public SectionStackSection build() {
        this.jobsPane = this.controller.buildJobsView();
        this.tasksPane = this.controller.buildTasksView();
        this.tasksPane.setStyleName("executionView");

        HLayout panesLayout = new HLayout();
        panesLayout.addMember(jobsPane);
        panesLayout.addMember(tasksPane);

        SectionStackSection executionsSection = new SectionStackSection();
        executionsSection.setTitle("Executions list");
        executionsSection.setExpanded(true);
        executionsSection.setItems(panesLayout);

        this.tasksPane.hide();
        controller.checkIfJobIdToOpen();
        chkMy = new CheckboxItem("myjobs", "My jobs");
        chkMy.setValue(false);
        chkMy.addChangedHandler(event -> controller.fetchMyExecutionsOnly(chkMy.getValueAsBoolean()));
        chkPending = new CheckboxItem("pending", "Pending");
        chkPending.setValue(true);
        chkPending.addChangedHandler(event -> controller.fetchPending(chkPending.getValueAsBoolean()));
        chkRunning = new CheckboxItem("running", "Current");
        chkRunning.setValue(true);
        chkRunning.addChangedHandler(event -> controller.fetchRunning(chkRunning.getValueAsBoolean()));
        chkFinished = new CheckboxItem("finished", "Past");
        chkFinished.setValue(true);
        chkFinished.addChangedHandler(event -> controller.fetchFinished(chkFinished.getValueAsBoolean()));

        final SelectItem modeSelect = new SelectItem();
        modeSelect.setValueMap(ExecutionListMode.JOB_CENTRIC.name, ExecutionListMode.TASK_CENTRIC.name);
        modeSelect.setValue(ExecutionListMode.JOB_CENTRIC.name);
        modeSelect.setShowTitle(false);
        modeSelect.addChangedHandler(event -> controller.switchMode((String) modeSelect.getValue()));

        filter = new IButton("Filter");
        filter.setIcon(Images.instance.filter_32().getSafeUri().asString());
        filter.addClickHandler(event -> {
            if (modeSelect.getValueAsString().equals(ExecutionListMode.JOB_CENTRIC.name)) {
                controller.getJobsController().getView().toggleFilterPane();
            } else {
                controller.getTasksController().getView().toggleFilterPane();
            }
        });

        chkMy.setWidth(60);
        chkPending.setWidth(60);
        chkRunning.setWidth(60);
        chkFinished.setWidth(80);

        DynamicForm checkBoxes = new DynamicForm();
        checkBoxes.setNumCols(10);
        checkBoxes.setItems(chkMy, chkPending, chkRunning, chkFinished, modeSelect);

        String user = LoginModel.getInstance().getLogin();
        // login unknown: credentials login; fetching only my jobs will be impossible server side
        if (user == null || user.trim().length() == 0) {
            chkMy.setDisabled(true);
        }

        LayoutSpacer space = new LayoutSpacer(1, 10);
        executionsSection.setControls(checkBoxes, space, filter);

        return executionsSection;
    }

    @Override
    public void modeSwitched(ExecutionListMode mode) {
        switch (mode) {
            case JOB_CENTRIC:
                this.tasksPane.hide();
                this.jobsPane.show();
                showJobFilters();
                break;
            case TASK_CENTRIC:
                this.jobsPane.hide();
                this.tasksPane.show();
                hideJobFilters();
        }
    }

    private void hideJobFilters() {
        Arrays.asList(chkPending, chkRunning, chkFinished).forEach(FormItem::hide);
    }

    private void showJobFilters() {
        Arrays.asList(chkPending, chkRunning, chkFinished).forEach(FormItem::show);
    }
}
