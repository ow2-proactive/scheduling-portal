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

import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerController;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.PaginationListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.TasksUpdatedListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Task;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.TasksPaginationController;

import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

public class TasksPaginationView implements TasksUpdatedListener, PaginationListener{

    private TasksPaginationController paginationController;

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

    public TasksPaginationView(SchedulerController controller){
        this.paginationController = new TasksPaginationController(controller);
        this.paginationController.getModel().addPaginationListener(this);
        controller.getEventDispatcher().addTasksUpdatedListener(this);
    }

    /**
     * Builds the view content.
     * @return a layout containing the view content.
     */
    public Layout build() {
        /* Task pagination buttons and indicator label */
        this.pageNextButton = new ToolStripButton("Next >");
        this.pageNextButton.disable();
        this.pageNextButton.addStyleName("navNextPaginationButton");
        this.pageNextButton.addClickHandler(new ClickHandler() {
            public void onClick(ClickEvent event) {
                paginationController.nextPage();
            }
        });
        this.pagePreviousButton = new ToolStripButton("< Previous");
        this.pagePreviousButton.disable();
        this.pagePreviousButton.addStyleName("navPreviousPaginationButton");
        this.pagePreviousButton.addClickHandler(new ClickHandler() {
            public void onClick(ClickEvent event) {
                paginationController.previousPage();
            }
        });

        this.pageLabel = new Label("");
        this.pageLabel.addStyleName("navPaginationLabel");
        this.pageLabel.setAlign(Alignment.CENTER);
        this.pageLabel.setWidth(60);
        this.pageLabel.setMargin(0);
        this.pageLabel.setPadding(0);

        ToolStrip paginationLayout = new ToolStrip();
        paginationLayout.addStyleName("itemPaginationBar");
        paginationLayout.setHeight(34);
        paginationLayout.setWidth100();
        paginationLayout.setBackgroundImage("");
        paginationLayout.setBackgroundColor("#fafafa");
        paginationLayout.setBorder("0px");
        
        paginationLayout.addMember(this.pagePreviousButton);
        paginationLayout.addMember(this.pageLabel);
        paginationLayout.addMember(this.pageNextButton);

        return paginationLayout;
    }

    @Override
    public void tasksUpdating(boolean jobChanged) {
        // TODO Auto-generated method stub

    }

    @Override
    public void tasksUpdated(List<Task> tasks) {
        this.pageNextButton.disable();
        this.pagePreviousButton.disable();

        if (this.paginationController.hasPrevious())
            this.pagePreviousButton.enable();

        if (tasks != null && this.paginationController.hasNext(tasks.size()))
            this.pageNextButton.enable();
    }

    @Override
    public void tasksUpdatedFailure(String message) {
        // TODO Auto-generated method stub

    }


    public void pageChanged() {
        this.pageNextButton.disable();
        this.pagePreviousButton.disable();
        this.pageLabel.setContents(this.paginationController.getPaginationLabel());
    }


}
