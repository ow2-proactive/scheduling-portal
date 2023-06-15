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
import org.ow2.proactive_grid_cloud_portal.common.client.model.LogModel;

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

    private static final String LABEL_REGEX_PROPERTY = "pa.scheduler.label.regex";

    private static final String LABEL_MAX_LENGTH_PROPERTY = "pa.scheduler.label.max.length";

    private Window window;

    private final SchedulerController controller;

    private ListGrid labelsListGrid;

    public ManageLabelsWindow(SchedulerController controller) {
        this.controller = controller;
        controller.getSchedulerProperties(this);
    }

    public void show() {
        this.window.show();
    }

    public void destroy() {
        this.window.destroy();
    }

    public void build(Map<String, Object> properties) {

        labelsListGrid = new ListGrid();
        labelsListGrid.setWidth(440);
        labelsListGrid.setHeight(150);
        labelsListGrid.setAlternateRecordStyles(true);
        labelsListGrid.setShowAllRecords(true);

        ListGridField labelField = getLabelField(properties);
        ListGridField removeField = getRemoveField();

        labelsListGrid.setFields(labelField, removeField);
        labelsListGrid.setCanResizeFields(true);
        labelsListGrid.setValidateOnChange(true);

        controller.getJobLabels(this);

        IButton addRecordButton = getAddRecordButton();
        IButton resetGridButton = getResetGridButton();
        IButton applyChangesButton = getApplyChangesButton();
        IButton cancelChangesButton = getCancelChangesButton();

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

    private IButton getCancelChangesButton() {
        IButton cancelChangesButton = new IButton("Cancel");
        cancelChangesButton.setTooltip("Cancel the current modifications and close window");
        cancelChangesButton.setIcon(Images.instance.cancel_16().getSafeUri().asString());
        cancelChangesButton.addClickHandler(event -> {
            resetGrid();
            window.hide();
        });
        return cancelChangesButton;
    }

    private IButton getApplyChangesButton() {
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
                applyChanges();
            }
        });
        return applyChangesButton;
    }

    private void applyChanges() {
        labelsListGrid.saveEdits();
        labelsListGrid.redraw();
        List<String> labels = new ArrayList<>();
        for (int j = 0; j < labelsListGrid.getTotalRows(); j++) {
            labels.add(labelsListGrid.getRecord(j).getAttribute("label"));
        }
        controller.setLabels(this, labels);
    }

    private IButton getResetGridButton() {
        IButton resetGridButton = new IButton("Reset");
        resetGridButton.setTooltip("Cancel the current modifications and keep previous configuration");
        resetGridButton.addClickHandler(event -> resetGrid());
        return resetGridButton;
    }

    private void resetGrid() {
        controller.getJobLabels(this);
        labelsListGrid.redraw();
    }

    private IButton getAddRecordButton() {
        IButton addRecordButton = new IButton("Add");
        addRecordButton.setTooltip("Add a new label");
        addRecordButton.addClickHandler(event -> addNewRecord());
        return addRecordButton;
    }

    private void addNewRecord() {
        ListGridRecord newRecord = new ListGridRecord();
        labelsListGrid.addData(newRecord);
        newRecord.setAttribute(" ", Images.instance.remove().getSafeUri().asString());
        newRecord.set_canEdit(true);
        labelsListGrid.startEditing(labelsListGrid.getRowNum(newRecord));
        newRecord.setAttribute("label", "");
    }

    private ListGridField getRemoveField() {
        ListGridField removeField = new ListGridField(" ", 30);
        removeField.setType(ListGridFieldType.IMAGE);
        removeField.setCanEdit(false);
        removeField.setCanSort(false);
        removeField.setDefaultValue(Images.instance.remove().getSafeUri().asString());
        removeField.addRecordClickHandler(event -> labelsListGrid.removeData(labelsListGrid.getRecord(event.getRecordNum())));
        return removeField;
    }

    private ListGridField getLabelField(Map<String, Object> properties) {
        ListGridField labelField = new ListGridField("label", "Label", 390);
        Validator requiredValidator = getRequiredValidator();
        Validator lengthValidator = getLengthValidator(properties);
        Validator regexpValidator = getRegexValidator(properties);
        if (lengthValidator != null && regexpValidator != null) {
            labelField.setValidators(requiredValidator, lengthValidator, regexpValidator);
        } else if (lengthValidator != null) {
            labelField.setValidators(requiredValidator, lengthValidator);
        } else if (regexpValidator != null) {
            labelField.setValidators(requiredValidator, regexpValidator);
        } else {
            labelField.setValidators(requiredValidator);
        }
        labelField.setValidateOnChange(true);
        labelField.setCanEdit(true);
        return labelField;
    }

    private Validator getRegexValidator(Map<String, Object> properties) {
        if (properties.get(LABEL_REGEX_PROPERTY) != null) {
            String labelRegex = properties.get(LABEL_REGEX_PROPERTY).toString();
            Validator regexpValidator = new CustomValidator() {
                @Override
                protected boolean condition(Object value) {
                    return value == null || value.toString().matches(labelRegex);
                }
            };
            regexpValidator.setErrorMessage("Field does not matched regex " + labelRegex);
            return regexpValidator;
        }
        return null;
    }

    private Validator getLengthValidator(Map<String, Object> properties) {
        if (properties.get(LABEL_MAX_LENGTH_PROPERTY) != null) {
            int labelMaxLength = Integer.parseInt(properties.get(LABEL_MAX_LENGTH_PROPERTY).toString());
            Validator lengthValidator = new CustomValidator() {
                @Override
                protected boolean condition(Object value) {
                    return value == null || value.toString().length() < labelMaxLength;
                }
            };
            lengthValidator.setErrorMessage("The maximum length is  " + labelMaxLength + " characters");
            return lengthValidator;
        }
        return null;
    }

    private Validator getRequiredValidator() {
        Validator requiredValidator = new CustomValidator() {
            @Override
            protected boolean condition(Object value) {
                return value != null && !value.toString().trim().isEmpty();
            }
        };
        requiredValidator.setErrorMessage("Field is required");
        return requiredValidator;
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
