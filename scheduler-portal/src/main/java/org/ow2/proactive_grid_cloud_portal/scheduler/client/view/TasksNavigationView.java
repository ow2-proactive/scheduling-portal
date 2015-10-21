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

import java.util.List;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.Job;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerController;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.JobSelectedListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.TagSuggestionListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.TasksUpdatedListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Task;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.TasksNavigationController;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.TasksPaginationController;

import com.google.gwt.event.dom.client.KeyCodes;
import com.google.gwt.event.dom.client.KeyDownEvent;
import com.google.gwt.event.dom.client.KeyDownHandler;
import com.google.gwt.user.client.ui.Button;
import com.google.gwt.user.client.ui.SuggestBox;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;

/**
 * View for the tasks navigation bar.
 * @author The activeeon team
 *
 */
public class TasksNavigationView implements TasksUpdatedListener, TagSuggestionListener, JobSelectedListener{

    /**
     * Test field to specify the tag filter.
     */
    private SuggestBox tagSearchTextBox;

    /**
     * Checkbox for enabling auto-refresh.
     */
    private CheckboxItem autoRefreshOption;

    
    /**
     * The main controller of the application.
     */
    private SchedulerController schedulerController;
    
    
    /**
     * Controller for the navigation logic.
     */
    private TasksNavigationController navigationController;
    
    


    /**
     * Build the view for the tasks navigation.
     * @param controller the main controller.
     */
    public TasksNavigationView(SchedulerController controller){
        this.navigationController = new TasksNavigationController(controller);
        this.schedulerController = controller;
        this.schedulerController.setTaskNavigationController(this.navigationController);

        this.navigationController.getModel().addTagSuggestionListener(this);
        this.schedulerController.getEventDispatcher().addJobSelectedListener(this);
        this.schedulerController.getEventDispatcher().addTasksUpdatedListener(this);
    }


    /**
     * Builds the view content.
     * @return a layout containing the view content.
     */
    public Layout build() {
        this.tagSearchTextBox = new SuggestBox(this.navigationController.getTagSuggestionOracle());

        this.tagSearchTextBox.addStyleName("searchBox");
        this.tagSearchTextBox.getElement().setAttribute("placeholder", "tag...");
        this.tagSearchTextBox.setEnabled(false);
        this.tagSearchTextBox.addKeyDownHandler(new KeyDownHandler() {
            @Override
            public void onKeyDown(KeyDownEvent event) {
                if(event.getNativeKeyCode() == KeyCodes.KEY_ENTER){
                    changeTagFilterHandler();
                }
            }
        });

        Button btnFilter = new Button("Filter");
        btnFilter.setWidth("40");
        btnFilter.addStyleName("btnBoxCombo");
        btnFilter.addClickHandler(new com.google.gwt.event.dom.client.ClickHandler() {
            @Override
            public void onClick(com.google.gwt.event.dom.client.ClickEvent event) {
                changeTagFilterHandler();
            }
        });


        this.autoRefreshOption = new CheckboxItem("autoRefreshOption", "Auto-refresh");
        this.autoRefreshOption.setCellStyle("navBarOption");
        this.autoRefreshOption.setTextBoxStyle("navBarOptionTextBox");
        this.autoRefreshOption.setTitleStyle("navbarOptionTitle");
        this.autoRefreshOption.setPrintTitleStyle("navBarOptionPrintTitle");
        this.autoRefreshOption.addChangedHandler(new ChangedHandler() {
            @Override
            public void onChanged(ChangedEvent event) {
                navigationController.setTaskAutoRefreshOption(autoRefreshOption.getValueAsBoolean());
            }
        });

        DynamicForm checkBoxes = new DynamicForm();
        checkBoxes.setNumCols(1);
        checkBoxes.setItems(autoRefreshOption);
        checkBoxes.addStyleName("checkBoxForm");

        ToolStrip navTools = new ToolStrip();
        navTools.addStyleName("itemViewNav");
        navTools.setHeight(34);
        navTools.setWidth100();
        navTools.setBackgroundImage("");
        navTools.setBackgroundColor("#fafafa");
        navTools.setBorder("0px");

        navTools.addMember(tagSearchTextBox);
        navTools.addMember(btnFilter);
        navTools.addMember(checkBoxes);

        return navTools;
    }



    @Override
    public void tasksUpdating(boolean jobChanged) {
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
    public void tagSuggestionListUpdated(){
        this.tagSearchTextBox.showSuggestionList();
    }


    /**
     * Handler when the tag in the textbox changed.
     */
    protected void changeTagFilterHandler(){
        String tag = tagSearchTextBox.getText();
        this.navigationController.setTaskTagFilter(tag);
    }





    @Override
    public void jobSelected(Job job) {
        this.tagSearchTextBox.setEnabled(true);
        this.tagSearchTextBox.setText("");
        this.navigationController.resetNavigation();
    }


    @Override
    public void jobUnselected() {
        this.tagSearchTextBox.setText("");
        this.tagSearchTextBox.setEnabled(false);
        this.navigationController.stopNavigation();
       
    }


    /**
     * Gets the tasks pagination controller.
     * @return the tasks pagination controller.
     */
    public TasksPaginationController getTaskPaginationController() {
        return navigationController.getPaginationController();
    }
    
    

    @Override
    public void selectedJobUpdated() {
        this.navigationController.refresh();
    }

}
