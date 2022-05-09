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

import java.util.Date;

import org.ow2.proactive_grid_cloud_portal.common.client.Images;
import org.ow2.proactive_grid_cloud_portal.common.client.Settings;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LogModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.JobPriority;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.JobStatus;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerImages;
import org.ow2.proactive_grid_cloud_portal.scheduler.shared.filter.Action;
import org.ow2.proactive_grid_cloud_portal.scheduler.shared.filter.Field;
import org.ow2.proactive_grid_cloud_portal.scheduler.shared.filter.FilterModel;

import com.google.gwt.core.client.GWT;
import com.google.gwt.dom.client.Style;
import com.google.gwt.dom.client.Style.VerticalAlign;
import com.google.gwt.event.dom.client.ChangeEvent;
import com.google.gwt.event.dom.client.ChangeHandler;
import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.i18n.client.DateTimeFormat;
import com.google.gwt.json.client.*;
import com.google.gwt.resources.client.ImageResource;
import com.google.gwt.user.client.Window;
import com.google.gwt.user.client.ui.*;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.HStack;
import com.smartgwt.client.widgets.layout.VStack;


/**
 * @author ActiveEon Team
 * @since Apr 24, 2017
 */
public class FilterView extends VStack {

    private boolean importActivated = false;

    private FormPanel importFromFileformPanel;

    private HLayout fromFilePanel;

    private FileUpload fileUpload;

    private RadioButton matchAllButton;

    private RadioButton matchAnyButton;

    private TextBox filterNameTextBox;

    private VStack filterPanel;

    private Anchor addButton;

    private Label errorLabel;

    private final static ImageResource ADD_BUTTON_IMAGE = SchedulerImages.instance.add();

    private final static ImageResource REMOVE_BUTTON_IMAGE = Images.instance.remove();

    private final static ImageResource DELETE_BUTTON_IMAGE = SchedulerImages.instance.cancel_14();

    private final static ImageResource IMPORT_BUTTON_IMAGE = SchedulerImages.instance.open();

    private final static ImageResource EXPORT_BUTTON_IMAGE = SchedulerImages.instance.job_export_16();

    private static final String DATE_FORMAT_FULL = "yyyy-MM-dd HH:mm:ss ZZ";

    private static final String DATE_FORMAT_MEDIUM = "yyyy-MM-dd HH:mm:ss";

    private static final String DATE_FORMAT_DATE_ONLY = "yyyy-MM-dd";

    private static final String DATE_FORMAT_HOUR_ONLY = "HH:mm:ss";

    private static final String ERROR_MESSAGE = "errorMessage";

    public FilterView() {
        initComp();
        setLayout();
    }

    private void clearMessagePanel() {
        if (filterPanel.contains(errorLabel)) {
            filterPanel.removeMember(errorLabel);
        }
    }

    private void displayErrorMessage(String message) {

        clearMessagePanel();

        errorLabel = new Label(message);
        errorLabel.setHeight(30);
        errorLabel.setWidth100();
        errorLabel.setMargin(10);
        errorLabel.setAlign(Alignment.CENTER);
        errorLabel.setStyleName(ERROR_MESSAGE);

        filterPanel.addMember(errorLabel);
        filterPanel.reflow();
    }

    private String validateField(Field field, String value) {
        switch (field) {
            case ID:
            case NUMBER_OF_PENDING_TASKS:
            case NUMBER_OF_RUNNING_TASKS:
            case NUMBER_OF_FINISHED_TASKS:
            case NUMBER_OF_FAILED_TASKS:
            case NUMBER_OF_FAULTY_TASKS:
            case NUMBER_OF_IN_ERROR_TASKS:
                try {
                    Integer.parseInt(value);
                } catch (NumberFormatException e) {
                    displayErrorMessage("Invalid value for field " + field + " : \"" + value + "\" is not an integer.");
                }
                break;
            case SUBMITTED_TIME:
            case START_TIME:
            case LAST_UPDATED_TIME:
            case FINISHED_TIME:
                try {
                    return "" + getMillisFromDate(value, field.getName());
                } catch (IllegalArgumentException e) {
                    displayErrorMessage(e.getMessage());
                }
                break;
            default:
                break;
        }
        return value;
    }

    public FilterModel getFilterModel() {

        clearMessagePanel();

        FilterModel model = new FilterModel();
        int widgetCount = filterPanel.getMembersLength();

        model.setMatchAny(matchAnyButton.getValue());
        for (int i = 0; i < widgetCount; i++) {
            RowFilter row = (RowFilter) filterPanel.getMember(i);
            Field field = Field.get(row.fieldsList.getSelectedValue());
            Action action = Action.get(row.actionList.getSelectedValue());
            String value = row.getValue();

            value = validateField(field, value);

            model.addConstraint(field, action, value);
        }

        Settings.get().setSetting("filterConfig", generateFilterJsonString());

        return model;
    }

    public void clearCriteria() {
        filterPanel.removeMembers(filterPanel.getMembers());
        addRow();
    }

    public String generateFilterJsonString() {
        JSONObject jsonObject = new JSONObject();
        int widgetCount = filterPanel.getMembersLength();
        JSONArray constraintsArray = new JSONArray();
        for (int i = 0; i < widgetCount; i++) {
            RowFilter row = (RowFilter) filterPanel.getMember(i);
            String field = row.fieldsList.getSelectedValue();
            String action = row.actionList.getSelectedValue();
            String value = row.getValue();

            JSONObject constraintObject = new JSONObject();
            constraintObject.put("field", new JSONString(field));
            constraintObject.put("action", new JSONString(action));
            constraintObject.put("value", new JSONString(value));

            constraintsArray.set(i, constraintObject);
        }
        jsonObject.put("filterName", new JSONString(filterNameTextBox.getText()));
        jsonObject.put("matchAny", new JSONString(matchAnyButton.getValue().toString()));
        jsonObject.put("constraints", constraintsArray);

        return jsonObject.toString();
    }

    public void exportFilterConfiguration() {
        String jsonConfig = generateFilterJsonString();
        Settings.get().setSetting("filterConfig", jsonConfig);

        String url = GWT.getModuleBaseURL() + "exportfilter";
        url += "?filterConfigJson=" + jsonConfig;
        url += "&filterName=" + filterNameTextBox.getText();

        Window.open(url, "_blank", "");
    }

    public void loadFilterConfiguration(String jsonContent) {
        JSONObject jsonObject = null;

        try {
            JSONValue parsedJsonString = JSONParser.parseStrict(new HTML(jsonContent).getText());
            jsonObject = parsedJsonString.isObject();

        } catch (JSONException e) {
            LogModel.getInstance().logCriticalMessage(e.getMessage());
        }

        filterPanel.removeMembers(filterPanel.getMembers());

        JSONString filterName = jsonObject.get("filterName").isString();
        JSONString matchAny = jsonObject.get("matchAny").isString();
        JSONArray constraints = jsonObject.get("constraints").isArray();
        for (int i = 0; i < constraints.size(); i++) {
            JSONObject field = constraints.get(i).isObject();
            RowFilter newRow = new RowFilter();
            newRow.fieldsList.setSelectedIndex(getSelectedValueIndex(newRow.fieldsList,
                                                                     field.get("field").isString().stringValue()));
            newRow.setActionsAccordingToSelectedField();
            newRow.actionList.setSelectedIndex(getSelectedValueIndex(newRow.actionList,
                                                                     field.get("action").isString().stringValue()));
            newRow.setValue(field.get("value").isString().stringValue());
            filterPanel.addMember(newRow);
            updateRemoveButtonStatus();
        }
        if ("true".equals(matchAny.isString().stringValue())) {
            matchAnyButton.setValue(true);
        } else {
            matchAllButton.setValue(true);
        }
        filterNameTextBox.setText(filterName.stringValue());
        updateRemoveButtonStatus();

    }

    private void filterUploader() {
        fileUpload = new FileUpload();
        fileUpload.setName("filter");

        importFromFileformPanel = new FormPanel();
        fileUpload.addChangeHandler(onFileChosen -> {
            if (!fileUpload.getFilename().isEmpty()) {
                importFromFileformPanel.submit();

            }
        });
        importFromFileformPanel.setHeight("15px");
        importFromFileformPanel.add(fileUpload);
        importFromFileformPanel.setEncoding(FormPanel.ENCODING_MULTIPART);
        importFromFileformPanel.setMethod(FormPanel.METHOD_POST);
        importFromFileformPanel.setAction(GWT.getModuleBaseURL() + "importfilter");
        importFromFileformPanel.addSubmitCompleteHandler(new FormPanel.SubmitCompleteHandler() {
            public void onSubmitComplete(FormPanel.SubmitCompleteEvent event) {
                String jsonContent = event.getResults();
                fileUpload.setVisible(false);
                importFromFileformPanel.reset();
                fromFilePanel.setVisible(false);
                removeMember(fromFilePanel);
                loadFilterConfiguration(jsonContent);
                importActivated = false;

            }
        });

        fromFilePanel = new HLayout();
        fromFilePanel.setAlign(Alignment.RIGHT);
        fromFilePanel.addChild(importFromFileformPanel);
        fromFilePanel.setHeight("15px");

    }

    //This method is used to retrieve the index of an input string in a listBox. Unfortunately, GWT does not provide such functionality
    private int getSelectedValueIndex(ListBox fieldsList, String str) {
        int indexToFind = -1;
        for (int i = 0; i < fieldsList.getItemCount(); i++) {
            if (fieldsList.getItemText(i).equals(str)) {
                indexToFind = i;
            }
        }
        return indexToFind;
    }

    private void setLayout() {
        HorizontalPanel radioPanel = new HorizontalPanel();
        radioPanel.add(matchAllButton);
        radioPanel.add(matchAnyButton);
        radioPanel.setHeight("30px");

        VStack rowsPanel = new VStack();
        rowsPanel.addMember(filterPanel);
        rowsPanel.addMember(addButton);

        HorizontalPanel importExportPanel = new HorizontalPanel();
        importExportPanel.setHeight("30px");
        importExportPanel.setSpacing(5);

        filterNameTextBox.setStyleName("filter-name-textbox");
        filterNameTextBox.setEnabled(true);
        filterNameTextBox.getElement().setPropertyString("placeholder", "Enter filter name");
        filterNameTextBox.getElement().getStyle().setColor("black");
        filterNameTextBox.getElement().getStyle().setFontStyle(Style.FontStyle.ITALIC);
        filterNameTextBox.setVisibleLength(26);
        Image deleteNameImage = new Image(DELETE_BUTTON_IMAGE);

        deleteNameImage.setTitle("Delete filter name");
        deleteNameImage.addClickHandler(new ClickHandler() {
            public void onClick(ClickEvent event) {
                filterNameTextBox.setText("");
            }
        });

        IButton importButton = new IButton("Import");
        importButton.setTooltip("Import a filter configuration from a JSON file");
        importButton.setIcon(IMPORT_BUTTON_IMAGE.getSafeUri().asString());
        importButton.setShowDisabledIcon(false);
        importButton.addClickHandler(new com.smartgwt.client.widgets.events.ClickHandler() {
            @Override
            public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
                if (importActivated) {
                    fileUpload.setVisible(false);
                    importFromFileformPanel.reset();
                    fromFilePanel.setVisible(false);
                    removeMember(fromFilePanel);
                    importActivated = false;
                } else {
                    filterUploader();
                    addMember(fromFilePanel, 1);
                    importActivated = true;
                }
            }
        });

        IButton exportButton = new IButton("Export");
        exportButton.setTooltip("Export the filter configuration as a JSON file");
        exportButton.setIcon(EXPORT_BUTTON_IMAGE.getSafeUri().asString());
        exportButton.setShowDisabledIcon(false);
        exportButton.addClickHandler(new com.smartgwt.client.widgets.events.ClickHandler() {
            @Override
            public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
                if (filterNameTextBox.getText().isEmpty()) {
                    displayErrorMessage("The name of the filter cannot be empty");
                } else {
                    clearMessagePanel();
                    exportFilterConfiguration();
                }
            }
        });

        HLayout buttonsLayout = new HLayout();
        buttonsLayout.setAlign(Alignment.RIGHT);
        buttonsLayout.setHeight(20);
        buttonsLayout.setMembersMargin(5);
        buttonsLayout.setMembers(importButton, exportButton);

        importExportPanel.add(buttonsLayout);
        importExportPanel.add(filterNameTextBox);
        importExportPanel.add(deleteNameImage);

        HTML html = new HTML("<hr  style=\"width:460px;\" />");

        HorizontalPanel hp = new HorizontalPanel();
        hp.add(html);
        hp.setHeight("10px");
        hp.setSpacing(5);

        addMember(importExportPanel);
        addMember(hp);
        addMember(radioPanel);
        addMember(rowsPanel);
        setWidth100();
    }

    private void initComp() {
        filterNameTextBox = new TextBox();
        filterNameTextBox.setTitle("Click to edit filter name");
        matchAllButton = new RadioButton("matchGroup", "Match All");
        matchAllButton.setValue(true);
        matchAllButton.setTitle("Use \"Match All\" to filter jobs that match all filter criteria");
        matchAnyButton = new RadioButton("matchGroup", "Match Any");
        matchAnyButton.setTitle("Use \"Match Any\" to filter jobs that match any filter criteria");

        filterPanel = new VStack();
        filterPanel.setWidth100();
        filterPanel.addMember(new RowFilter());

        addButton = new Anchor();
        addButton.setPixelSize(12, 12);
        addButton.addClickHandler(event -> addRow());
        addButton.getElement().appendChild(new Image(ADD_BUTTON_IMAGE).getElement());
        updateRemoveButtonStatus();
    }

    private void addRow() {
        RowFilter newRow = new RowFilter();
        filterPanel.addMember(newRow);
        updateRemoveButtonStatus();
    }

    private void removeRow(RowFilter element) {
        filterPanel.removeMember(element);
        updateRemoveButtonStatus();
    }

    private void updateRemoveButtonStatus() {
        int widgetCount = filterPanel.getMembersLength();
        if (widgetCount >= 1) {
            ((RowFilter) filterPanel.getMember(0)).enableRemoveButton(widgetCount > 1);
        }
    }

    public long getMillisFromDate(String value, String fieldName) {
        Date now = new Date();
        String nowDateString = DateTimeFormat.getFormat(DATE_FORMAT_DATE_ONLY).format(now);
        Date answer;

        try {
            answer = DateTimeFormat.getFormat(DATE_FORMAT_DATE_ONLY).parse(value.trim());
        } catch (Exception e) {
            try {
                answer = DateTimeFormat.getFormat(DATE_FORMAT_MEDIUM).parse(nowDateString + " " + value.trim());
            } catch (Exception e1) {
                try {
                    answer = DateTimeFormat.getFormat(DATE_FORMAT_MEDIUM).parse(value.trim());
                } catch (Exception e2) {
                    try {

                        answer = DateTimeFormat.getFormat(DATE_FORMAT_FULL).parse(value.trim());
                    } catch (Exception e3) {
                        throw new IllegalArgumentException("Invalid value for field '" + fieldName + "' : \"" + value +
                                                           "\" is not a valid date. (" + DATE_FORMAT_HOUR_ONLY + "|" +
                                                           DATE_FORMAT_DATE_ONLY + "|" + DATE_FORMAT_MEDIUM + "|" +
                                                           DATE_FORMAT_FULL + ")", e3);
                    }
                }
            }
        }

        return answer.getTime();

    }

    private class RowFilter extends HStack implements ClickHandler, ChangeHandler {

        private ListBox fieldsList;

        private ListBox actionList;

        private Anchor removeButton;

        private TextBox textBox;

        private TextBox dateTextBox;

        private ListBox priorityList;

        private ListBox stateList;

        private SimplePanel valuePanel;

        private Image buttonImage;

        public RowFilter() {
            initComp();
            setHeight("35px");
            setMembersMargin(5);
            addMember(removeButton);
            addMember(fieldsList);
            addMember(actionList);
            valuePanel.setWidget(textBox);
            addMember(valuePanel);
        }

        @Override
        public void onClick(ClickEvent event) {
            if (removeButton.isEnabled()) {
                removeRow(this);
            }
        }

        @Override
        public void onChange(ChangeEvent event) {
            setActionsAccordingToSelectedField();
        }

        public String getValue() {
            Widget valueWidget = valuePanel.getWidget();
            if (valueWidget == priorityList)
                return priorityList.getSelectedValue();
            if (valueWidget == stateList)
                return stateList.getSelectedValue();
            if (valueWidget == dateTextBox)
                return dateTextBox.getText();
            return textBox.getText();
        }

        public void setValue(String value) {
            Widget valueWidget = valuePanel.getWidget();
            if (valueWidget == priorityList)
                priorityList.setSelectedIndex(getSelectedValueIndex(priorityList, value));
            if (valueWidget == stateList)
                stateList.setSelectedIndex(getSelectedValueIndex(stateList, value));
            if (valueWidget == dateTextBox)
                dateTextBox.setText(value);
            if (valueWidget == textBox)
                textBox.setText(value);
        }

        private void setActionsAccordingToSelectedField() {
            Field field = Field.get(fieldsList.getSelectedValue());
            actionList.clear();
            switch (field) {
                case ID: {
                    actionList.addItem(Action.EQUALS.getName());
                    actionList.addItem(Action.LESS_THAN_OR_EQUAL_TO.getName());
                    actionList.addItem(Action.GREATER_THAN_OR_EQUAL_TO.getName());
                    valuePanel.setWidget(textBox);
                    break;
                }
                case PROJECT_NAME:
                case USER:
                case TENANT:
                case NAME: {
                    actionList.addItem(Action.EQUALS.getName());
                    actionList.addItem(Action.NOT_EQUAL.getName());
                    actionList.addItem(Action.CONTAINS.getName());
                    actionList.addItem(Action.NOT_CONTAIN.getName());
                    actionList.addItem(Action.STARTS_WITH.getName());
                    valuePanel.setWidget(textBox);
                    break;
                }
                case PRIORITY: {
                    actionList.addItem(Action.EQUALS.getName());
                    valuePanel.setWidget(priorityList);
                    break;
                }
                case STATE: {
                    actionList.addItem(Action.EQUALS.getName());
                    valuePanel.setWidget(stateList);
                    break;
                }
                case SUBMITTED_TIME:
                case START_TIME:
                case LAST_UPDATED_TIME:
                case FINISHED_TIME: {
                    actionList.addItem(Action.LESS_THAN_OR_EQUAL_TO.getName());
                    actionList.addItem(Action.GREATER_THAN_OR_EQUAL_TO.getName());

                    dateTextBox.getElement().setPropertyString("placeholder", "yyyy-MM-dd HH:mm:ss");

                    valuePanel.setWidget(dateTextBox);
                    break;
                }
                case NUMBER_OF_PENDING_TASKS:
                case NUMBER_OF_RUNNING_TASKS:
                case NUMBER_OF_FINISHED_TASKS:
                case NUMBER_OF_FAULTY_TASKS:
                case NUMBER_OF_FAILED_TASKS:
                case NUMBER_OF_IN_ERROR_TASKS: {
                    actionList.addItem(Action.LESS_THAN_OR_EQUAL_TO.getName());
                    actionList.addItem(Action.GREATER_THAN_OR_EQUAL_TO.getName());
                    valuePanel.setWidget(textBox);
                    break;
                }

                default:
                    break;
            }
        }

        private void enableRemoveButton(boolean value) {
            removeButton.setEnabled(value);
            buttonImage.getElement().getStyle().setOpacity(value ? 1 : 0.5);
        }

        private void initComp() {

            buttonImage = new Image(REMOVE_BUTTON_IMAGE);
            buttonImage.getElement().getStyle().setVerticalAlign(VerticalAlign.MIDDLE);

            removeButton = new Anchor();
            removeButton.addClickHandler(this);
            removeButton.setPixelSize(12, 12);
            removeButton.getElement().appendChild(buttonImage.getElement());

            fieldsList = new ListBox();
            fieldsList.addChangeHandler(this);
            fieldsList.setPixelSize(100, 20);
            for (Field field : Field.values()) {
                fieldsList.addItem(field.getName());
            }

            actionList = new ListBox();
            actionList.setPixelSize(140, 20);

            textBox = new TextBox();
            textBox.setPixelSize(140, 12);

            dateTextBox = new TextBox();
            dateTextBox.setPixelSize(140, 12);

            priorityList = new ListBox();
            priorityList.setPixelSize(140, 20);
            for (JobPriority priority : JobPriority.values()) {
                priorityList.addItem(priority.name());
            }

            stateList = new ListBox();
            stateList.setPixelSize(140, 20);
            for (JobStatus status : JobStatus.values()) {
                stateList.addItem(status.name());
            }

            valuePanel = new SimplePanel();
            valuePanel.setPixelSize(150, 25);

            setActionsAccordingToSelectedField();
        }
    }
}
