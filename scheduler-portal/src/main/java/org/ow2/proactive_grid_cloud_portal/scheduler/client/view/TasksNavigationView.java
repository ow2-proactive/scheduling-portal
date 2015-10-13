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
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.PaginationListener;
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
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * View for the tasks navigation bar.
 * @author The activeeon team
 *
 */
public class TasksNavigationView implements TasksUpdatedListener, PaginationListener, TagSuggestionListener, JobSelectedListener{

    /**
     * Test field to specify the tag filter.
     */
    private SuggestBox tagSearchTextBox;

    /**
     * Checkbox for enabling auto-refresh.
     */
    private CheckboxItem autoRefreshOption;

    /**
     * Task page number
     */
    private Label pageLabel = null;
    /**
     * Task previous page button
     */
    private ToolStripButton pagePreviousButton = null;
    /**
     * Task next page button
     */
    private ToolStripButton pageNextButton = null;

    /**
     * Controller for the navigation logic.
     */
    private TasksNavigationController controller;


    /**
     * Build the view for the tasks navigation.
     * @param controller the main controller.
     */
    public TasksNavigationView(SchedulerController controller){
        this.controller = new TasksNavigationController(controller);
        controller.setTaskNavigationController(this.controller);


        this.controller.getPaginationController().getModel().addPaginationListener(this);
        this.controller.getModel().addTagSuggestionListener(this);
        controller.getEventDispatcher().addJobSelectedListener(this);
        controller.getEventDispatcher().addTasksUpdatedListener(this);
    }


    /**
     * Builds the view content.
     * @return a layout containing the view content.
     */
    public Layout build() {
        this.tagSearchTextBox = new SuggestBox(this.controller.getTagSuggestionOracle());

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
                controller.setTaskAutoRefreshOption(autoRefreshOption.getValueAsBoolean());
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


        /* Task pagination buttons and indicator label */
        this.pageNextButton = new ToolStripButton("Next >");
        this.pageNextButton.disable();
        this.pageNextButton.addStyleName("navPaginationButton");
        this.pageNextButton.addClickHandler(new ClickHandler() {
            public void onClick(ClickEvent event) {
                controller.getPaginationController().nextPage();
            }
        });
        this.pagePreviousButton = new ToolStripButton("< Previous");
        this.pagePreviousButton.disable();
        this.pagePreviousButton.addStyleName("navPaginationButton");
        this.pagePreviousButton.addClickHandler(new ClickHandler() {
            public void onClick(ClickEvent event) {
                controller.getPaginationController().previousPage();
            }
        });

        this.pageLabel = new Label("");
        this.pageLabel.addStyleName("navPaginationLabel");
        this.pageLabel.setAlign(Alignment.CENTER);
        this.pageLabel.setWidth(60);
        this.pageLabel.setMargin(0);
        this.pageLabel.setPadding(0);


        navTools.addMember(this.pageNextButton);
        navTools.addMember(this.pageLabel);
        navTools.addMember(this.pagePreviousButton);

        return navTools;
    }



    @Override
    public void tasksUpdating(boolean jobChanged) {
        this.tagSearchTextBox.setEnabled(false);
    }

    @Override
    public void tasksUpdated(List<Task> tasks) {
        this.tagSearchTextBox.setEnabled(true);


        this.pageNextButton.disable();
        this.pagePreviousButton.disable();

        if (this.controller.getPaginationController().hasPrevious())
            this.pagePreviousButton.enable();

        if (tasks != null && this.controller.getPaginationController().hasNext(tasks.size()))
            this.pageNextButton.enable();
    }

    @Override
    public void tasksUpdatedFailure(String message) {
        this.tagSearchTextBox.setEnabled(false);
    }


    public void pageChanged() {
        this.pageNextButton.disable();
        this.pagePreviousButton.disable();
        this.pageLabel.setContents(this.controller.getPaginationController().getPaginationLabel());
    }


    public void tagSuggestionListUpdated(){
        this.tagSearchTextBox.showSuggestionList();
    }



    protected void changeTagFilterHandler(){
        String tag = tagSearchTextBox.getText();
        this.controller.setTaskTagFilter(tag);
    }





    @Override
    public void jobSelected(Job job) {
        this.tagSearchTextBox.setEnabled(true);
        this.tagSearchTextBox.setText("");
    }


    @Override
    public void jobUnselected() {
        this.tagSearchTextBox.setText("");
        this.tagSearchTextBox.setEnabled(false);
    }


    public TasksPaginationController getTaskPaginationController() {
        return controller.getPaginationController();
    }

}
