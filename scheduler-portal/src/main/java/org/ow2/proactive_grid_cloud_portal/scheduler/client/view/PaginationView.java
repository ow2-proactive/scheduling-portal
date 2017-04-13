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

import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.PaginationListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.PaginationController;

import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;


public abstract class PaginationView<T extends PaginationController<?>> implements PaginationListener {

    protected T paginationController;

    /**
     * Previous page button
     */
    protected ToolStripButton pagePreviousButton = null;

    /**
     * Next page button
     */
    protected ToolStripButton pageNextButton = null;

    /**
     * First page button
     */
    protected ToolStripButton pageFirstButton = null;

    /**
     * Last page button
     */
    protected ToolStripButton pageLastButton = null;

    public PaginationView(T paginationController) {
        this.paginationController = paginationController;
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

        return buildLayout();
    }

    protected abstract Layout buildLayout();

    /**
     * Disable all the buttons for the pagination.
     */
    protected void disableAllControls() {
        this.pageFirstButton.disable();
        this.pagePreviousButton.disable();
        this.pageNextButton.disable();
        this.pageLastButton.disable();
    }

    /**
     * Enables the pagination button according to the navigation status.
     */
    protected void enablePaginationControls() {
        if (this.paginationController.hasPrevious()) {
            this.pageFirstButton.enable();
            this.pagePreviousButton.enable();
        }

        if (this.paginationController.hasNext()) {
            this.pageNextButton.enable();
            this.pageLastButton.enable();
        }
    }

    protected ToolStrip getToolStripPaginationLayout() {
        ToolStrip paginationLayout = new ToolStrip();
        paginationLayout.addStyleName("itemPaginationBar");
        paginationLayout.setHeight(30);
        paginationLayout.setWidth100();
        paginationLayout.setBackgroundImage("");
        paginationLayout.setBackgroundColor("#fafafa");
        paginationLayout.setBorder("0px");
        return paginationLayout;
    }
}
