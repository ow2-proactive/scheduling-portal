/*
 *  *
 * ProActive Parallel Suite(TM): The Java(TM) library for
 *    Parallel, Distributed, Multi-Core Computing for
 *    Enterprise Grids & Clouds
 *
 * Copyright (C) 1997-2014 INRIA/University of
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
 *  * $$PROACTIVE_INITIAL_DEV$$
 */

package org.ow2.proactive_grid_cloud_portal.scheduler.client.view;

import com.smartgwt.client.widgets.layout.LayoutSpacer;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LoginModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.ExecutionDisplayModeListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.ExecutionListMode;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.ExecutionsController;

import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.SectionStackSection;

public class ExecutionsView implements ExecutionDisplayModeListener{

    protected ExecutionsController controller;

    protected Layout jobsPane;

    protected Layout tasksPane;

    protected CheckboxItem chkMy;

    protected CheckboxItem chkPending;

    protected CheckboxItem chkRunning;

    protected CheckboxItem chkFinished;

    public ExecutionsView(ExecutionsController controller) {
        this.controller = controller;
        this.controller.getModel().addExecutionsDisplayModeListener(this);
    }

    public SectionStackSection build(){
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

        chkMy = new CheckboxItem("myjobs", "My jobs");
        chkMy.setValue(false);
        chkMy.addChangedHandler(new ChangedHandler() {
            public void onChanged(ChangedEvent event) {
                controller.fetchMyExecutionsOnly(chkMy.getValueAsBoolean());
            }
        });
        chkPending = new CheckboxItem("pending", "Pending");
        chkPending.setValue(true);
        chkPending.addChangedHandler(new ChangedHandler() {
            public void onChanged(ChangedEvent event) {
                controller.fetchPending(chkPending.getValueAsBoolean());
            }
        });
        chkRunning = new CheckboxItem("running", "Running");
        chkRunning.setValue(true);
        chkRunning.addChangedHandler(new ChangedHandler() {
            public void onChanged(ChangedEvent event) {
                controller.fetchRunning(chkRunning.getValueAsBoolean());
            }
        });
        chkFinished = new CheckboxItem("finished", "Finished");
        chkFinished.setValue(true);
        chkFinished.addChangedHandler(new ChangedHandler() {
            public void onChanged(ChangedEvent event) {
                controller.fetchFinished(chkFinished.getValueAsBoolean());
            }
        });

        final SelectItem modeSelect = new SelectItem();
        modeSelect.setValueMap(ExecutionListMode.JOB_CENTRIC.name, ExecutionListMode.TASK_CENTRIC.name);
        modeSelect.setValue(ExecutionListMode.JOB_CENTRIC.name);
        modeSelect.setShowTitle(false);
        modeSelect.addChangedHandler(new ChangedHandler() {
            @Override
            public void onChanged(ChangedEvent event) {
                controller.switchMode((String) modeSelect.getValue());
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
        executionsSection.setControls(checkBoxes, space);

        return executionsSection;
    }


    @Override
    public void modeSwitched(ExecutionListMode mode) {
        switch(mode){
        case JOB_CENTRIC:
            this.tasksPane.hide();
            this.jobsPane.show();
            chkMy.setTitle("My jobs");
            chkMy.redraw();
            break;
        case TASK_CENTRIC:
            this.jobsPane.hide();
            this.tasksPane.show();
            chkMy.setTitle("My tasks");
            chkMy.redraw();
        }

    }
}
