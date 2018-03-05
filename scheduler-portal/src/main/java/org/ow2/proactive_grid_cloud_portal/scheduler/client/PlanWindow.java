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
package org.ow2.proactive_grid_cloud_portal.scheduler.client;

import org.ow2.proactive_grid_cloud_portal.common.client.Images;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.ui.Button;
import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.FormPanel;
import com.google.gwt.user.client.ui.FormPanel.SubmitCompleteEvent;
import com.google.gwt.user.client.ui.FormPanel.SubmitCompleteHandler;
import com.google.gwt.user.client.ui.InlineLabel;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;


/**
 * Popup Window for opening the job planner portal
 */
public class PlanWindow {

    private static final int WIDTH = 400;

    private static final int HEIGHT = 150;

    private static final String JPP_URL = "/automation-dashboard/#/portal/job-planner-calendar-def-workflows";

    private Window window;

    private SchedulerController controller;

    // the main layout of the window
    private VLayout rootPage;

    //buttons
    private HLayout yesNoButtons;

    private IButton yesButton;

    private IButton noButton;

    /**
     * Default constructor
     *
     * @param controller
     */
    public PlanWindow(SchedulerController controller) {
        this.controller = controller;
        this.build();
    }

    /**
     * Shows the window created by the constructor
     */
    public void show() {
        this.window.show();
    }

    /**
     * Destroy the window, you may null the reference after this as it will not
     * be usable again
     */
    public void destroy() {
        this.window.destroy();
    }

    private void initRootPage() {
        rootPage = new VLayout();
        rootPage.setMargin(20);
        rootPage.setWidth100();
        rootPage.setHeight100();
    }

    private void initMessagesPart() {
        VLayout messagePanel = new VLayout();

        Label confirmationLabel = new Label("You will open a new tab with the Job Planner Portal. Do you confirm?");
        confirmationLabel.setHeight(30);
        confirmationLabel.setWidth100();
        confirmationLabel.setMargin(10);
        confirmationLabel.setAlign(Alignment.CENTER);

        messagePanel.addMember(confirmationLabel);
        rootPage.addMember(messagePanel);
    }

    private void initButtonsPart() {
        yesNoButtons = new HLayout();
        yesNoButtons.setMargin(10);
        yesNoButtons.setMembersMargin(5);
        yesNoButtons.setHeight(30);
        yesNoButtons.setWidth100();
        yesNoButtons.setAlign(Alignment.RIGHT);

        yesButton = new IButton("Yes");
        yesButton.addClickHandler(clickEvent -> {
            com.google.gwt.user.client.Window.open(JPP_URL, JPP_URL, "");
            PlanWindow.this.window.hide();
            PlanWindow.this.destroy();
        });

        noButton = new IButton("No");
        noButton.addClickHandler(clickEvent -> {
            PlanWindow.this.window.hide();
            PlanWindow.this.destroy();
        });

        yesNoButtons.setMembers(yesButton, noButton);
        rootPage.addMember(yesNoButtons);
    }

    private void build() {

        initRootPage(); // ------------ root page of the window
        initMessagesPart(); // -------- Select workflow Panel
        initButtonsPart(); // ------------ Fill workflow variables Panel

        this.window = new Window();
        this.window.setTitle("Plan a job");
        this.window.setShowMinimizeButton(false);
        this.window.setIsModal(true);
        this.window.setShowModalMask(true);
        this.window.addItem(rootPage);
        this.window.setWidth(WIDTH);
        this.window.setHeight(HEIGHT);
        this.window.centerInPage();
        this.window.setCanDragResize(true);
    }
}
