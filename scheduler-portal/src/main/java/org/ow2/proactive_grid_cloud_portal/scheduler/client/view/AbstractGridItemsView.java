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
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.ItemsListGrid;

import com.google.gwt.event.shared.HandlerRegistration;
import com.smartgwt.client.data.SortSpecifier;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.grid.events.SortChangedHandler;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;


public abstract class AbstractGridItemsView<T> implements PaginationListener {

    /**
     * the Grid widget displayed in the view
     */
    protected ItemsListGrid<T> itemsGrid = null;

    /**
     * shown when loading
     */
    protected Label loadingLabel = null;

    /**
     * shown upon error
     */
    protected Label errorLabel = null;

    protected String itemName;

    protected boolean hasToolBar = true;

    protected void itemUpdating() {
        this.errorLabel.hide();
        this.itemsGrid.hide();
        this.loadingLabel.show();
    }

    protected void itemUpdatedFailure(String message) {
        this.errorLabel.setContents(message);
        this.itemsGrid.hide();
        this.loadingLabel.hide();
        this.errorLabel.show();
    }

    protected void itemUpdated() {
        this.errorLabel.hide();
        this.loadingLabel.hide();
        this.itemsGrid.show();
    }

    protected Layout buildToolbar() {
        return null;
    }

    protected abstract Layout buildPagination();

    protected Layout buildPagination(PaginationController<?> paginationController) {
        Layout result = paginationController.buildView();
        paginationController.getModel().addPaginationListener(this);
        return result;
    }

    protected abstract void buildGrid();

    protected Layout buildContent() {
        VLayout layout = new VLayout();
        this.buildGrid();

        this.loadingLabel = new Label("fetching " + this.itemName + "...");
        this.loadingLabel.setIcon("loading.gif");
        this.loadingLabel.setWidth100();
        this.loadingLabel.setHeight100();
        this.loadingLabel.setAlign(Alignment.CENTER);
        this.loadingLabel.hide();

        this.errorLabel = new Label("");
        this.errorLabel.setWidth100();
        this.errorLabel.setAlign(Alignment.CENTER);
        this.errorLabel.hide();

        layout.addMember(this.itemsGrid);
        layout.addMember(this.loadingLabel);
        layout.addMember(this.errorLabel);

        return layout;
    }

    public Layout build() {
        VLayout itemsViewLayout = new VLayout();

        if (this.hasToolBar) {
            Layout navTools = this.buildToolbar();
            itemsViewLayout.addMember(navTools);
        }

        Layout content = this.buildContent();
        itemsViewLayout.addMember(content);

        Layout paginationBar = this.buildPagination();
        itemsViewLayout.addMember(paginationBar);

        return itemsViewLayout;
    }

    public SortSpecifier[] getSort() {
        return itemsGrid.getSort();
    }

    public HandlerRegistration addSortChangedHandler(SortChangedHandler sortChangedHandler) {
        return itemsGrid.addSortChangedHandler(sortChangedHandler);
    }
}
