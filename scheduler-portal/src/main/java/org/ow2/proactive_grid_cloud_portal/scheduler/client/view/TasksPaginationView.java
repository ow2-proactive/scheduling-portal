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

import java.util.List;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.TasksUpdatedListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Task;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.TasksController;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.TasksPaginationController;

import com.google.gwt.event.dom.client.KeyCodes;
import com.google.gwt.event.dom.client.KeyDownEvent;
import com.google.gwt.event.dom.client.KeyDownHandler;
import com.google.gwt.user.client.ui.TextBox;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;


public class TasksPaginationView extends PaginationView<TasksPaginationController>
        implements TasksUpdatedListener {

    /**
     * Task page number
     */
    private Label offsetRangeLabel = null;

    /**
     * Label that indicates the max number of pages
     */
    private Label pageMaxLabel = null;

    /**
     * A textbox that allows the user to choose which page to display.
     */
    private TextBox txtPageNumber = null;

    private String itemTypeName;

    public TasksPaginationView(TasksController controller) {
        super(controller.getTaskNavigationController().getPaginationController());
        this.itemTypeName = "Tasks";
        this.paginationController.getModel().addPaginationListener(this);
        controller.getModel().addTasksUpdatedListener(this);
    }

    /**
     * Builds the view content.
     * @return a layout containing the view content.
     */
    public Layout buildLayout() {

        this.offsetRangeLabel = new Label(this.itemTypeName + " 0 - 0");
        this.offsetRangeLabel.addStyleName("navPaginationLabel");
        this.offsetRangeLabel.setAlign(Alignment.CENTER);
        this.offsetRangeLabel.setWidth100();
        this.offsetRangeLabel.setPadding(0);

        Label pageLabel = new Label("Page");
        pageLabel.setWidth100();
        pageLabel.setAlign(Alignment.RIGHT);
        pageLabel.setMinWidth(30);
        pageLabel.setMargin(4);

        this.pageMaxLabel = new Label("of 0");
        this.pageMaxLabel.setAlign(Alignment.LEFT);
        this.pageMaxLabel.setWidth100();
        this.pageMaxLabel.setMinWidth(40);
        this.pageMaxLabel.setMargin(4);

        this.txtPageNumber = new TextBox();
        this.txtPageNumber.setValue("0");
        this.txtPageNumber.setWidth("25px");
        this.txtPageNumber.addStyleName("txtPageNumber");
        this.txtPageNumber.addKeyDownHandler(new KeyDownHandler() {
            @Override
            public void onKeyDown(KeyDownEvent event) {
                if (event.getNativeKeyCode() == KeyCodes.KEY_ENTER) {
                    changePageNumberHandler();
                }
            }
        });

        HLayout labelLayout = new HLayout();
        labelLayout.addStyleName("labelPaginationLayout");
        labelLayout.addMember(pageLabel);
        labelLayout.addMember(this.txtPageNumber);
        labelLayout.addMember(this.pageMaxLabel);
        labelLayout.addMember(this.offsetRangeLabel);

        ToolStrip paginationLayout = new ToolStrip();
        paginationLayout.addStyleName("itemPaginationBar");
        paginationLayout.setHeight(30);
        paginationLayout.setWidth100();
        paginationLayout.setBackgroundImage("");
        paginationLayout.setBackgroundColor("#fafafa");
        paginationLayout.setBorder("0px");

        paginationLayout.addMember(this.pageFirstButton);
        paginationLayout.addMember(this.pagePreviousButton);
        paginationLayout.addMember(labelLayout);
        paginationLayout.addMember(this.pageLastButton);
        paginationLayout.addMember(this.pageNextButton);

        return paginationLayout;
    }

    protected void changePageNumberHandler() {
        String text = this.txtPageNumber.getText();
        try {
            int pageNumber = Integer.parseInt(text) - 1;
            this.paginationController.goToPage(pageNumber);
        } catch (Exception ex) {

        }
    }

    /**
     * Disable all the buttons for the pagination.
     */
    protected void disableAllControls() {
        super.disableAllControls();
        this.txtPageNumber.setEnabled(false);
    }

    /**
     * Enables the pagination button according to the navigation status.
     */
    @Override
    protected void enablePaginationControls() {
        super.enablePaginationControls();
        this.txtPageNumber.setEnabled(true);
    }

    @Override
    public void pageChanged() {
        this.disableAllControls();
        this.offsetRangeLabel
                .setContents(this.itemTypeName + " " + this.paginationController.getPaginationRangeLabel());
        this.txtPageNumber.setText(this.paginationController.getNumberPageText());
    }

    @Override
    public void totalItemChanged() {
        this.pageMaxLabel.setContents("of " + this.paginationController.getMaxPageNumberLabel());
        this.enablePaginationControls();
    }

    protected void itemsUpdated(long totalItems) {
        this.disableAllControls();
        this.paginationController.computeMaxPage(totalItems);
        this.offsetRangeLabel
                .setContents(this.itemTypeName + " " + this.paginationController.getPaginationRangeLabel());
        this.txtPageNumber.setText(this.paginationController.getNumberPageText());
    }

    @Override
    public void tasksUpdating() {
    }

    @Override
    public void tasksUpdated(List<Task> tasks, long totalTasks) {
        itemsUpdated(totalTasks);
    }

    @Override
    public void tasksUpdatedFailure(String message) {
    }
}
