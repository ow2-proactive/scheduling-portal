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

import org.ow2.proactive_grid_cloud_portal.scheduler.client.JobPriority;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.JobStatus;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerImages;
import org.ow2.proactive_grid_cloud_portal.scheduler.shared.filter.Action;
import org.ow2.proactive_grid_cloud_portal.scheduler.shared.filter.Field;
import org.ow2.proactive_grid_cloud_portal.scheduler.shared.filter.FilterModel;

import com.google.gwt.dom.client.Style.VerticalAlign;
import com.google.gwt.event.dom.client.ChangeEvent;
import com.google.gwt.event.dom.client.ChangeHandler;
import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.i18n.client.DateTimeFormat;
import com.google.gwt.resources.client.ImageResource;
import com.google.gwt.user.client.ui.*;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.layout.HStack;
import com.smartgwt.client.widgets.layout.VStack;


/**
 * @author ActiveEon Team
 * @since Apr 24, 2017
 */
public class FilterView extends VStack {

    private RadioButton matchAllButton;

    private RadioButton matchAnyButton;

    private VStack filterPanel;

    private Anchor addButton;

    private Label errorLabel;

    private final static ImageResource ADD_BUTTON_IMAGE = SchedulerImages.instance.add();

    private final static ImageResource REMOVE_BUTTON_IMAGE = SchedulerImages.instance.remove();

    private static final String ERROR_MESSAGE = "errorMessage";

    private static final String DATE_FORMAT = "yyyy-MM-dd HH:mm:ss";

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

    private void validateField(Field field, String value) {
        switch (field) {
            case ID:
                try {
                    Integer.parseInt(value);
                } catch (NumberFormatException e) {
                    displayErrorMessage("Invalid value for field " + field + " : \"" + value + "\" is not an integer.");
                }
                break;
            case SUBMITTED_TIME:
                try {
                    DateTimeFormat.getFormat(DATE_FORMAT).parse(value);
                } catch (IllegalArgumentException e) {
                    displayErrorMessage("Invalid value for field " + field + " : \"" + value +
                                        "\" is not a valid date. (" + DATE_FORMAT + ")");
                }
                break;
            default:
                break;
        }
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

            validateField(field, value);

            model.addConstraint(field, action, value);
        }

        return model;
    }

    public void clearCriteria() {
        filterPanel.removeMembers(filterPanel.getMembers());
        addRow();
    }

    private void setLayout() {
        HorizontalPanel radioPanel = new HorizontalPanel();
        radioPanel.add(matchAllButton);
        radioPanel.add(matchAnyButton);
        radioPanel.setHeight("30px");
        radioPanel.setSpacing(10);

        VStack rowsPanel = new VStack();
        rowsPanel.addMember(filterPanel);
        rowsPanel.addMember(addButton);

        addMember(radioPanel);
        addMember(rowsPanel);
        setWidth100();
    }

    private void initComp() {
        matchAllButton = new RadioButton("matchGroup", "Match All");
        matchAllButton.setValue(true);
        matchAnyButton = new RadioButton("matchGroup", "Match Any");

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
                case SUBMITTED_TIME: {
                    actionList.addItem(Action.LESS_THAN_OR_EQUAL_TO.getName());
                    actionList.addItem(Action.GREATER_THAN_OR_EQUAL_TO.getName());

                    dateTextBox.getElement().setPropertyString("placeholder", "ISO 8601 format");

                    valuePanel.setWidget(dateTextBox);
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
