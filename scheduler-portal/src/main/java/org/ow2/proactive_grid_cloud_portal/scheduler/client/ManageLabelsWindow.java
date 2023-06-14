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

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.ow2.proactive_grid_cloud_portal.common.client.Images;

import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.validator.CustomValidator;
import com.smartgwt.client.widgets.form.validator.Validator;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;


/**
 * Client-side Manage labels menu
 *
 * Changes or newly added records are made in memory and not persisted
 *
 */
public class ManageLabelsWindow {

    private Window window;

    private final SchedulerController controller;

    private ListGrid labelsListGrid;

    public ManageLabelsWindow(SchedulerController controller) {
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

        labelsListGrid = new ListGrid();
        labelsListGrid.setWidth(440);
        labelsListGrid.setHeight(150);
        labelsListGrid.setAlternateRecordStyles(true);
        labelsListGrid.setShowAllRecords(true);

        ListGridField labelField = new ListGridField("label", "Label", 390);
        Validator requiredValidator = new CustomValidator() {
            @Override
            protected boolean condition(Object value) {
                return value != null && !value.toString().trim().isEmpty();
            }
        };
        requiredValidator.setErrorMessage("Field is required");
        labelField.setValidators(requiredValidator);
        labelField.setValidateOnChange(true);
        labelField.setCanEdit(true);

        ListGridField removeField = new ListGridField(" ", 30);
        removeField.setType(ListGridFieldType.IMAGE);
        removeField.setCanEdit(false);
        removeField.setCanSort(false);
        removeField.setDefaultValue(Images.instance.remove().getSafeUri().asString());
        removeField.addRecordClickHandler(event -> labelsListGrid.removeData(labelsListGrid.getRecord(event.getRecordNum())));

        labelsListGrid.setFields(labelField, removeField);
        labelsListGrid.setCanResizeFields(true);
        labelsListGrid.setValidateOnChange(true);

        controller.getJobLabels(this);

        IButton addRecordButton = new IButton("Add");
        addRecordButton.setTooltip("Add a new label");
        addRecordButton.addClickHandler(event -> {
            ListGridRecord newRecord = new ListGridRecord();
            labelsListGrid.addData(newRecord);
            newRecord.setAttribute(" ", Images.instance.remove().getSafeUri().asString());
            newRecord.set_canEdit(true);
            labelsListGrid.startEditing(labelsListGrid.getRowNum(newRecord));
            newRecord.setAttribute("label", "");
        });

        IButton resetGridButton = new IButton("Reset");
        resetGridButton.setTooltip("Cancel the current modifications and keep previous configuration");
        resetGridButton.addClickHandler(event -> {
            controller.getJobLabels(this);
            labelsListGrid.redraw();
        });

        IButton applyChangesButton = new IButton("Apply");
        applyChangesButton.setTooltip("Apply modifications");
        applyChangesButton.setIcon(Images.instance.ok_16().getSafeUri().asString());
        applyChangesButton.addClickHandler(event -> {
            labelsListGrid.endEditing();
            if (labelsListGrid.hasErrors()) {
                for (int j = 0; j < labelsListGrid.getTotalRows(); j++) {
                    if (!labelsListGrid.validateRow(j)) {
                        labelsListGrid.startEditing(j);
                        return;
                    }
                }
            } else {
                labelsListGrid.saveEdits();
                labelsListGrid.redraw();
                List<String> labels = new ArrayList<>();
                for (int j = 0; j < labelsListGrid.getTotalRows(); j++) {
                    labels.add(labelsListGrid.getRecord(j).getAttribute("label"));
                }
                controller.setLabels(this, labels);
            }
        });

        IButton cancelChangesButton = new IButton("Cancel");
        cancelChangesButton.setTooltip("Cancel the current modifications and close window");
        cancelChangesButton.setIcon(Images.instance.cancel_16().getSafeUri().asString());
        cancelChangesButton.addClickHandler(event -> {
            controller.getJobLabels(this);
            labelsListGrid.redraw();
            window.hide();
        });

        HLayout hLayout = new HLayout(15);
        hLayout.setHeight(20);
        hLayout.setAlign(Alignment.CENTER);
        hLayout.addMembers(addRecordButton, resetGridButton, applyChangesButton, cancelChangesButton);

        VLayout layout = new VLayout();
        layout.setMembersMargin(20);
        layout.setAutoHeight();
        layout.setMargin(10);
        layout.addMembers(labelsListGrid, hLayout);

        this.window = new Window();
        this.window.setTitle("Manage Labels");
        this.window.setShowMinimizeButton(false);
        this.window.setIsModal(true);
        this.window.setShowModalMask(true);
        this.window.addItem(layout);
        this.window.setWidth(470);
        this.window.setHeight(260);
        this.window.setCanDragResize(true);
        this.window.centerInPage();

    }

    public void setData(Map<String, String> labels) {
        ListGridRecord[] labelRecords = new ListGridRecord[labels.size()];
        int i = 0;
        for (String id : labels.keySet()) {
            ListGridRecord labelRecord = new ListGridRecord();
            labelRecord.setAttribute("label", labels.get(id));
            labelRecord.setAttribute(" ", Images.instance.remove().getSafeUri().asString());
            labelRecords[i] = labelRecord;
            i++;
        }
        labelsListGrid.setData(labelRecords);
    }

    public void hide() {
        window.hide();
    }

}
