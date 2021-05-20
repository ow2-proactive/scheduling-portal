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
package org.ow2.proactive_grid_cloud_portal.rm.client;

import org.ow2.proactive_grid_cloud_portal.common.client.Images;

import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.events.CloseClickEvent;
import com.smartgwt.client.widgets.events.CloseClickHandler;
import com.smartgwt.client.widgets.form.validator.CustomValidator;
import com.smartgwt.client.widgets.form.validator.Validator;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;


/**
 * Client-side Loggers settings menu
 *
 * Changes are effective immediately
 *
 * Changes or newly added records are made in memory and not persisted
 *
 */

public class LoggersWindow {

    private Window window;

    private RMController controller;

    private ListGrid loggersGrid;

    public LoggersWindow(RMController controller) {
        this.controller = controller;
        this.build();
    }

    public void show() {
        this.window.show();
    }

    public void destroy() {
        this.window.destroy();
    }

    private void build() {
        Layout layout = new VLayout();
        layout.setMembersMargin(15);
        layout.setAutoHeight();
        layout.setMargin(10);

        Label label = new Label("Edit current loggers levels of server instance and/or add new logger/level configurations. Please note that changes are made in memory and not persisted.");
        label.setHeight(30);

        loggersGrid = new ListGrid();
        loggersGrid.setWidth(500);
        loggersGrid.setHeight(244);
        loggersGrid.setAlternateRecordStyles(true);
        loggersGrid.setShowAllRecords(true);

        ListGridField loggerField = new ListGridField("logger", "Logger", 385);
        // prevent logger field validation if cell is empty or contains white spaces
        Validator loggerValidator = new CustomValidator() {
            @Override
            protected boolean condition(Object value) {
                if (value == null || value.toString().trim().isEmpty()) {
                    return false;
                }
                return true;
            }
        };
        loggerValidator.setErrorMessage("Field is required");
        loggerField.setValidators(loggerValidator);

        ListGridField levelField = new ListGridField("level", "Level");
        levelField.setValueMap("TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL", "OFF");
        levelField.setCanEdit(true);

        ListGridField removeField = new ListGridField(" ", 20);
        removeField.setType(ListGridFieldType.IMAGE);
        removeField.setCanEdit(false);
        removeField.setCanSort(false);
        removeField.setDefaultValue("blank.png");

        loggersGrid.setFields(loggerField, levelField, removeField);
        loggersGrid.setCanResizeFields(true);
        loggersGrid.setValidateOnChange(true);

        this.controller.fetchLoggersSettings(loggersGrid);

        removeField.addRecordClickHandler(event -> {
            if (loggersGrid.getRecord(event.getRecordNum())
                           .getAttribute(" ")
                           .equals(Images.instance.remove().getSafeUri().asString())) {
                loggersGrid.removeData(loggersGrid.getRecord(event.getRecordNum()));
            }
        });

        IButton addRecordButton = new IButton("Add");
        addRecordButton.setTooltip("Add a new logger record");
        addRecordButton.addClickHandler(new ClickHandler() {
            public void onClick(ClickEvent event) {
                ListGridRecord newRecord = new ListGridRecord();
                loggersGrid.addData(newRecord);
                newRecord.setAttribute("level", "INFO");
                newRecord.setAttribute(" ", Images.instance.remove().getSafeUri().asString());
                newRecord.set_canEdit(true);
                loggersGrid.startEditing(loggersGrid.getRowNum(newRecord));
                newRecord.setAttribute("logger", "");
            }
        });

        IButton resetGridButton = new IButton("Reset");
        resetGridButton.setTooltip("Reset to current server configuration");
        resetGridButton.addClickHandler(new ClickHandler() {
            public void onClick(ClickEvent event) {
                controller.fetchLoggersSettings(loggersGrid);
                loggersGrid.redraw();
            }
        });

        IButton applyChangesButton = new IButton("Apply");
        applyChangesButton.setTooltip("Apply changes");
        applyChangesButton.setIcon(Images.instance.ok_16().getSafeUri().asString());
        applyChangesButton.addClickHandler(new ClickHandler() {
            public void onClick(ClickEvent event) {
                loggersGrid.endEditing();
                if (loggersGrid.hasErrors()) {
                    for (int j = 0; j < loggersGrid.getTotalRows(); j++) {
                        if (!loggersGrid.validateRow(j)) {
                            loggersGrid.startEditing(j);
                            return;
                        }
                    }
                } else {
                    loggersGrid.saveEdits();
                    controller.setLoggersSettings(loggersGrid);
                    loggersGrid.redraw();
                    for (int j = 0; j < loggersGrid.getTotalRows(); j++) {
                        loggersGrid.refreshRow(j);
                    }
                    controller.fetchLoggersSettings(loggersGrid);
                    window.hide();
                }
            }
        });

        IButton cancelChangesButton = new IButton("Cancel");
        cancelChangesButton.setTooltip("Cancel changes");
        cancelChangesButton.setIcon(Images.instance.cancel_16().getSafeUri().asString());
        cancelChangesButton.addClickHandler(new ClickHandler() {
            public void onClick(ClickEvent event) {
                controller.fetchLoggersSettings(loggersGrid);
                loggersGrid.redraw();
                window.hide();
            }
        });

        HLayout hLayout = new HLayout(15);
        hLayout.setHeight(20);
        hLayout.setAlign(Alignment.RIGHT);
        hLayout.addMembers(addRecordButton, resetGridButton, applyChangesButton, cancelChangesButton);

        layout.addMembers(label, loggersGrid);
        layout.addMember(hLayout);

        this.window = new Window();
        this.window.setTitle("Loggers Settings");
        this.window.setShowMinimizeButton(false);
        this.window.setIsModal(true);
        this.window.setShowModalMask(true);
        this.window.addItem(layout);
        this.window.setWidth(532);
        this.window.setHeight(380);
        this.window.centerInPage();
        this.window.addCloseClickHandler(new CloseClickHandler() {
            public void onCloseClick(CloseClickEvent event) {
                controller.fetchLoggersSettings(loggersGrid);
                loggersGrid.redraw();
            }
        });

    }

}
