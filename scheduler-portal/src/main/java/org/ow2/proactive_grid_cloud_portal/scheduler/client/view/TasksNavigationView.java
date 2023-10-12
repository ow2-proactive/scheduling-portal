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

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.Job;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.JobSelectedListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.TagSuggestionListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.TasksUpdatedListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Task;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.TasksNavigationController;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.TasksPaginationController;

import com.google.gwt.event.dom.client.KeyCodes;
import com.google.gwt.user.client.ui.Button;
import com.google.gwt.user.client.ui.SuggestBox;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;


/**
 * View for the tasks navigation bar.
 * @author The activeeon team
 *
 */
public class TasksNavigationView implements TasksUpdatedListener, TagSuggestionListener, JobSelectedListener {

    /**
     * Test field to specify the tag filter.
     */
    private SuggestBox tagSearchTextBox;

    /**
     * Controller for the navigation logic.
     */
    protected TasksNavigationController controller;

    private Map<String, Boolean> filters = new HashMap<>();

    /**
     * Build the view for the tasks navigation.
     * @param controller the main controller.
     */
    public TasksNavigationView(TasksNavigationController controller) {
        this.controller = controller;
        this.controller.getModel().addTagSuggestionListener(this);
        this.controller.getModel()
                       .getParentModel()
                       .getParentModel()
                       .getExecutionsModel()
                       .getJobsModel()
                       .addJobSelectedListener(this);
        this.controller.getModel().getParentModel().addTasksUpdatedListener(this);
    }

    /**
     * Builds the view content.
     * @return a layout containing the view content.
     */
    public Layout build() {
        this.tagSearchTextBox = new SuggestBox(this.controller.getTagSuggestionOracle());
        this.tagSearchTextBox.addStyleName("searchBox");
        this.tagSearchTextBox.getElement().setAttribute("placeholder", "Tag");
        this.tagSearchTextBox.setEnabled(false);
        this.tagSearchTextBox.addKeyDownHandler(event -> {
            if (event.getNativeKeyCode() == KeyCodes.KEY_ENTER) {
                changeTagFilterHandler();
            }
        });

        Button btnFilter = new Button("Filter");
        btnFilter.addStyleName("btnBoxCombo");
        btnFilter.addClickHandler(event -> changeTagFilterHandler());

        CheckboxItem autoRefreshOption = new CheckboxItem("autoRefreshOption", "Auto-refresh");
        autoRefreshOption.setCellStyle("navBarOption");
        autoRefreshOption.setTextBoxStyle("navBarOptionTextBox");
        autoRefreshOption.setTitleStyle("navbarOptionTitle");
        autoRefreshOption.setPrintTitleStyle("navBarOptionPrintTitle");
        autoRefreshOption.setValue(true);
        autoRefreshOption.setWidth(50);
        autoRefreshOption.addChangedHandler(event -> controller.setTaskAutoRefreshOption(autoRefreshOption.getValueAsBoolean()));

        DynamicForm autoRefreshForm = new DynamicForm();
        autoRefreshForm.setItems(autoRefreshOption);
        autoRefreshForm.addStyleName("form");

        HLayout navTools = new HLayout();
        navTools.addStyleName("itemViewNav");
        navTools.setHeight(34);

        navTools.addMember(tagSearchTextBox);
        navTools.addMember(btnFilter);

        Label filterLabel = new Label("Filters: ");
        filterLabel.setWidth("10");

        DynamicForm statusesForm = new DynamicForm();
        statusesForm.addStyleName("form");
        statusesForm.setNumCols(10);

        List<CheckboxItem> statusBoxes = Stream.of("Submitted", "Pending", "Current", "Past", "Error").map(status -> {
            CheckboxItem checkboxItem = new CheckboxItem(status, status);
            checkboxItem.setValue(true);
            checkboxItem.setWidth("9%");
            checkboxItem.addChangeHandler(event -> {
                String allFilters = setOneFilterValueReturnAll(status, (Boolean) event.getValue());
                controller.fitlerByStatuses(allFilters);
            });
            setOneFilterValueReturnAll(status, true);
            return checkboxItem;
        }).collect(Collectors.toList());

        statusesForm.setItems(statusBoxes.toArray(new CheckboxItem[0]));

        HLayout filterLayout = new HLayout();
        filterLayout.addMember(filterLabel);
        filterLayout.addMember(statusesForm);

        HLayout leftLayout = new HLayout();
        leftLayout.setMembersMargin(40);
        leftLayout.addMember(autoRefreshForm);
        leftLayout.addMember(filterLayout);

        navTools.addMember(leftLayout);

        controller.getModel().setStatusFilter(returnAllFilters());

        return navTools;
    }

    private String setOneFilterValueReturnAll(String status, Boolean value) {
        filters.put(status, value);
        return returnAllFilters();
    }

    private String returnAllFilters() {
        return filters.entrySet()
                      .stream()
                      .filter(Map.Entry::getValue)
                      .map(Map.Entry::getKey)
                      .collect(Collectors.joining(";"));
    }

    @Override
    public void tasksUpdating() {
        this.tagSearchTextBox.setEnabled(false);
    }

    @Override
    public void tasksUpdated(List<Task> tasks, long totalTasks) {
        this.tagSearchTextBox.setEnabled(true);
    }

    @Override
    public void tasksUpdatedFailure(String message) {
        this.tagSearchTextBox.setEnabled(false);
    }

    @Override
    public void tagSuggestionListUpdated() {
        this.tagSearchTextBox.showSuggestionList();
    }

    /**
     * Handler when the tag in the textbox changed.
     */
    protected void changeTagFilterHandler() {
        String tag = tagSearchTextBox.getText();
        this.controller.setTaskTagFilter(tag);
    }

    @Override
    public void jobSelected(Job job) {
        this.tagSearchTextBox.setEnabled(true);
        this.tagSearchTextBox.setText("");
        this.controller.resetNavigation();
    }

    @Override
    public void jobUnselected() {
        this.tagSearchTextBox.setText("");
        this.tagSearchTextBox.setEnabled(false);
        TasksView taskView = (TasksView) this.controller.getParentController().getView();
        taskView.itemsGrid.build();
        this.controller.stopNavigation();
    }

    /**
     * Gets the tasks pagination controller.
     * @return the tasks pagination controller.
     */
    public TasksPaginationController getTaskPaginationController() {
        return controller.getPaginationController();
    }

    @Override
    public void selectedJobUpdated(Job job) {
        this.controller.refresh();
    }

}
