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

import com.google.gwt.event.dom.client.KeyCodes;
import com.google.gwt.event.dom.client.KeyDownEvent;
import com.google.gwt.event.dom.client.KeyDownHandler;
import com.google.gwt.user.client.ui.TextBox;
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
    private Label offsetRangeLabel = null;
    
    /**
     * Task previous page button
     */
    private ToolStripButton pagePreviousButton = null;
    /**
     * Task next page button
     */
    private ToolStripButton pageNextButton = null;
    
    
    /**
     * Task first page button
     */
    private ToolStripButton pageFirstButton = null;
    /**
     * Task last page button
     */
    private ToolStripButton pageLastButton = null;
    
    /**
     * Label that indicates the max number of pages
     */
    private Label pageMaxLabel = null;
    
    /**
     * A textbox that allows the user to choose which page to display.
     */
    private TextBox txtPageNumber = null;

    
    public TasksPaginationView(SchedulerController controller){
        this.paginationController = new TasksPaginationController(controller);
        controller.setTasksPaginationController(paginationController);
        this.paginationController.getModel().addPaginationListener(this);
        controller.getEventDispatcher().addTasksUpdatedListener(this);
    }

    /**
     * Builds the view content.
     * @return a layout containing the view content.
     */
    public Layout build() {
        this.pageFirstButton = new ToolStripButton("<< First");
        this.pageFirstButton.disable();
        this.pageFirstButton.addStyleName("navPreviousPaginationButton");
        this.pageFirstButton.addClickHandler(new ClickHandler() {
            public void onClick(ClickEvent event) {
                paginationController.firstPage();
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
        
        
        this.pageNextButton = new ToolStripButton("Next >");
        this.pageNextButton.disable();
        this.pageNextButton.addStyleName("navNextPaginationButton");
        this.pageNextButton.addClickHandler(new ClickHandler() {
            public void onClick(ClickEvent event) {
                paginationController.nextPage();
            }
        });
        
        
        this.pageLastButton = new ToolStripButton("Last >>");
        this.pageLastButton.disable();
        this.pageLastButton.addStyleName("navNextPaginationButton");
        this.pageLastButton.addClickHandler(new ClickHandler() {
            public void onClick(ClickEvent event) {
                paginationController.lastPage();
            }
        });

        this.offsetRangeLabel = new Label("tasks 0 - 0");
        this.offsetRangeLabel.addStyleName("navPaginationLabel");
        this.offsetRangeLabel.setAlign(Alignment.CENTER);
        this.offsetRangeLabel.setWidth100();
        this.offsetRangeLabel.setPadding(0);
        
        
        Label pageLabel = new Label("page");
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
                if(event.getNativeKeyCode() == KeyCodes.KEY_ENTER){
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

    @Override
    public void tasksUpdating(boolean jobChanged) {
    }

    @Override
    public void tasksUpdated(List<Task> tasks, long totalTasks) {
        this.disableAllControls();
        this.paginationController.computeMaxPage(totalTasks);
        this.offsetRangeLabel.setContents("tasks " + this.paginationController.getPaginationRangeLabel());
        this.txtPageNumber.setText(this.paginationController.getNumberPageText());
    }

    @Override
    public void tasksUpdatedFailure(String message) {
    }


    @Override
    public void pageChanged() {
        this.disableAllControls();
        this.offsetRangeLabel.setContents("tasks " + this.paginationController.getPaginationRangeLabel());
        this.txtPageNumber.setText(this.paginationController.getNumberPageText());
    }
    
    
    @Override
    public void totalItemChanged() {
        this.pageMaxLabel.setContents("of " + this.paginationController.getMaxPageNumberLabel());
        this.enablePaginationControls();
    }
    
    /**
     * Disable all the buttons for the pagination.
     */
    protected void disableAllControls(){
        this.pageFirstButton.disable();
        this.pagePreviousButton.disable();
        this.pageNextButton.disable();
        this.pageLastButton.disable();
        this.txtPageNumber.setEnabled(false);
    }


    /**
     * Enables the pagination button according to the navigation status.
     */
    protected void enablePaginationControls(){
        if (this.paginationController.hasPrevious()){
            this.pageFirstButton.enable();
            this.pagePreviousButton.enable();
        }

        if (this.paginationController.hasNext()){
            this.pageNextButton.enable();
            this.pageLastButton.enable();
        }
        this.txtPageNumber.setEnabled(true);
    }
    
    
    protected void changePageNumberHandler(){
        String text = this.txtPageNumber.getText();
        try{
            int pageNumber = Integer.parseInt(text) - 1;
            this.paginationController.goToPage(pageNumber);
        }
        catch(Exception ex){
            
        }
    }
}
