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

import static org.ow2.proactive_grid_cloud_portal.scheduler.client.util.JobColumnsUtil.START_AT;

import java.util.*;
import java.util.stream.Collectors;

import org.ow2.proactive_grid_cloud_portal.common.client.Images;
import org.ow2.proactive_grid_cloud_portal.common.client.JSUtil;
import org.ow2.proactive_grid_cloud_portal.common.client.WindowUtils;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.JobsController;

import com.smartgwt.client.types.*;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.*;
import com.smartgwt.client.widgets.form.validator.RegExpValidator;
import com.smartgwt.client.widgets.form.validator.Validator;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.LayoutSpacer;
import com.smartgwt.client.widgets.layout.VLayout;


public class StartAtUpdateWindow {

    private static final String ISO_8601_REGEX = "^\\d{4}(-\\d{2}){2}((T|\\s)(\\d{2}:){2}\\d{2}(\\.\\d+)?(Z|[+-]\\d{2}:\\d{2})?)?$";

    private static final String CURRENT_START_AT_LABEL_CONTENT = "Current start at value: ";

    private Window window = null;

    private JobsController controller;

    private TextItem startAtInput;

    private Label updateResultLabel;

    private Label currentStartAtLabel;

    private List<Integer> jobIds;

    private String newStartValue;

    public StartAtUpdateWindow(JobsController controller, ListGridRecord[] selectedRecords) {
        this.controller = controller;
        this.build();
        this.show();
    }

    private void build() {
        HLayout pane = new HLayout();
        pane.setWidth100();
        pane.setHeight100();
        pane.setBackgroundColor("#ffffff");
        pane.setAlign(Alignment.LEFT);

        jobIds = controller.getModel().getSelectedJobsIds();
        String initialLabelContent = "";
        if (jobIds.size() == 1) {
            Long currentStartAt = controller.getModel().getJob(jobIds.get(0)).getStartAtTime();
            String formattedStartAt = null;
            if (currentStartAt != null) {
                formattedStartAt = JSUtil.getTime(currentStartAt);
            }
            initialLabelContent = CURRENT_START_AT_LABEL_CONTENT + "<b>" + formattedStartAt + "</b>";
        } else {
            initialLabelContent = "You are about to change the Start At value of jobs " +
                                  jobIds.stream().map(String::valueOf).collect(Collectors.joining(",")) + ".";
        }

        Label introLabel = new Label("The Start At value of a job defines when the job will be scheduled for execution");
        introLabel.setHeight(30);

        currentStartAtLabel = new Label(initialLabelContent);
        currentStartAtLabel.setHeight(30);

        updateResultLabel = new Label();
        updateResultLabel.setVisible(false);
        updateResultLabel.setHeight(30);

        final DynamicForm addEntryForm = new DynamicForm();
        addEntryForm.setAutoHeight();
        addEntryForm.setWidth(240);
        addEntryForm.setTitleAlign(Alignment.LEFT);

        startAtInput = new TextItem("startAt", "Start At");
        startAtInput.setRequired(true);
        startAtInput.setWidth(175);
        startAtInput.setHeight(26);
        startAtInput.setValidators(getIso8601Validator());
        startAtInput.setValidateOnChange(true);

        addEntryForm.setTitleOrientation(TitleOrientation.LEFT);
        addEntryForm.setFields(startAtInput);

        pane.setMembers(addEntryForm);

        HLayout buttons = new HLayout();
        buttons.setAlign(Alignment.CENTER);
        buttons.setWidth100();
        buttons.setHeight(10);
        buttons.setMargin(10);

        IButton closeBtn = new IButton("Close");
        closeBtn.setIcon(Images.instance.cancel_16().getSafeUri().asString());
        closeBtn.addClickHandler(event -> StartAtUpdateWindow.this.hide());

        IButton applyBtn = new IButton("Apply");
        applyBtn.setIcon(Images.instance.ok_16().getSafeUri().asString());
        applyBtn.setShowDisabledIcon(false);
        applyBtn.addClickHandler(event -> applyUpdate());

        addEntryForm.addItemChangedHandler(event -> applyBtn.setDisabled(!addEntryForm.validate()));

        LayoutSpacer space = new LayoutSpacer();
        space.setWidth(20);

        buttons.addMembers(applyBtn, space, closeBtn);

        VLayout root = new VLayout();
        root.setMembersMargin(10);
        root.setMargin(5);
        root.setWidth(400);
        root.setHeight100();

        root.addMembers(introLabel, currentStartAtLabel, pane, updateResultLabel, buttons);

        this.window = new Window();
        this.window.setTitle("Update the Start At of the selected job");
        this.window.setShowMinimizeButton(false);
        this.window.setIsModal(true);
        this.window.setShowModalMask(true);
        this.window.addItem(root);
        this.window.setAutoSize(true);
        this.window.setAutoCenter(true);
        this.window.setCanDragReposition(false);
        WindowUtils.setWindowAsClosableWhenEscapeKeyPressed(this.window);
    }

    public void clearAfterSuccess() {
        showSuccessMessage();
        startAtInput.clearValue();
        currentStartAtLabel.setContents(CURRENT_START_AT_LABEL_CONTENT + "<b>" + newStartValue + "</b>");
    }

    private void showSuccessMessage() {
        updateResultLabel.setContents("<p style=\"color:green;\">Start At value has been successfully updated !</p>");
        updateResultLabel.setVisible(true);
        com.google.gwt.user.client.Timer timer = new com.google.gwt.user.client.Timer() {
            @Override
            public void run() {
                updateResultLabel.animateFade(0, earlyFinish -> {
                    updateResultLabel.setVisible(false);
                    window.hide();
                }, 500);
            }
        };
        timer.schedule(2000);
    }

    public void displayFailureMessage(String httpMsg) {
        updateResultLabel.setContents("<p style=\"color:red;\">Failed to update Start At value: " + httpMsg + "</p>");
        updateResultLabel.setVisible(true);
    }

    private void applyUpdate() {
        if (!startAtInput.hasErrors()) {
            newStartValue = startAtInput.getValueAsString();
            controller.updateStartAt(newStartValue,
                                     jobIds.stream().map(Object::toString).collect(Collectors.toList()),
                                     this);
        }
    }

    private Validator getIso8601Validator() {
        RegExpValidator iso8601Validator = new RegExpValidator(ISO_8601_REGEX);
        iso8601Validator.setErrorMessage("Start At value must comply with ISO8601. Example: 2025-06-20T18:00:00+02:00");
        return iso8601Validator;
    }

    /**
     * Bring up the modal window
     */
    public void show() {
        this.window.show();
    }

    /**
     * Hide the modal window
     */
    public void hide() {
        this.window.hide();
    }

    /**
     * Destroy the window
     */
    public void destroy() {
        this.hide();
        this.window.destroy();
    }
}
