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

import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerImages;
import org.ow2.proactive_grid_cloud_portal.scheduler.shared.filter.Action;
import org.ow2.proactive_grid_cloud_portal.scheduler.shared.filter.Field;
import org.ow2.proactive_grid_cloud_portal.scheduler.shared.filter.FilterModel;

import com.google.gwt.dom.client.Style.VerticalAlign;
import com.google.gwt.event.dom.client.ChangeEvent;
import com.google.gwt.event.dom.client.ChangeHandler;
import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.resources.client.ImageResource;
import com.google.gwt.user.client.ui.Anchor;
import com.google.gwt.user.client.ui.HorizontalPanel;
import com.google.gwt.user.client.ui.Image;
import com.google.gwt.user.client.ui.ListBox;
import com.google.gwt.user.client.ui.RadioButton;
import com.google.gwt.user.client.ui.TextBox;
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

    private final static ImageResource ADD_BUTTON_IMAGE = SchedulerImages.instance.add();

    private final static ImageResource REMOVE_BUTTON_IMAGE = SchedulerImages.instance.remove();

    public FilterView() {
        initComp();
        setLayout();
    }

    public FilterModel getFilterModel() {
        FilterModel model = new FilterModel();
        int widgetCount = filterPanel.getMembersLength();

        if (widgetCount == 1 && ((RowFilter) filterPanel.getMember(0)).textBox.getText().isEmpty()) {
            return model;
        }

        model.setMatchAny(matchAnyButton.getValue());
        for (int i = 0; i < widgetCount; i++) {
            RowFilter row = (RowFilter) filterPanel.getMember(i);
            Field field = Field.get(row.fieldsList.getSelectedValue());
            Action action = Action.get(row.actionList.getSelectedValue());
            String value = row.textBox.getText();
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

        private Image buttonImage;

        public RowFilter() {
            initComp();
            setHeight("35px");
            setMembersMargin(5);
            addMember(removeButton);
            addMember(fieldsList);
            addMember(actionList);
            addMember(textBox);
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

        private void setActionsAccordingToSelectedField() {
            Field field = Field.get(fieldsList.getSelectedValue());
            actionList.clear();
            actionList.addItem(Action.EQUALS.getName());
            switch (field) {
                case ID: {
                    actionList.addItem(Action.LESS_THAN_OR_EQUAL_TO.getName());
                    actionList.addItem(Action.GREATER_THAN_OR_EQUAL_TO.getName());
                    break;
                }
                case USER:
                case NAME: {
                    actionList.addItem(Action.CONTAINS.getName());
                    actionList.addItem(Action.STARTS_WITH.getName());
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
            setActionsAccordingToSelectedField();

            textBox = new TextBox();
            textBox.setPixelSize(140, 12);
        }
    }
}
