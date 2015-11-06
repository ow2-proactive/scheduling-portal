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

import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.PaginationListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.PaginationController;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.ItemsListGrid;

import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;


public abstract class AbstractGridItemsView implements PaginationListener{

    /**
     * the Grid widget displayed in the view
     */
    protected ItemsListGrid itemsGrid = null;
    /**
     * shown when loading
     */
    protected Label loadingLabel = null;
    /**
     * shown upon error
     */
    protected Label errorLabel = null;

    protected String itemName;


    protected void itemUpdating(){
        this.errorLabel.hide();
        this.itemsGrid.hide();
        this.loadingLabel.show();
    }


    protected void itemUpdatedFailure(String message){
        this.errorLabel.setContents(message);
        this.itemsGrid.hide();
        this.loadingLabel.hide();
        this.errorLabel.show();
    }


    protected void itemUpdated(){
        this.errorLabel.hide();
        this.loadingLabel.hide();
        this.itemsGrid.show();
    }

    protected abstract Layout buildToolbar();

    protected abstract Layout buildPagination();

    protected Layout buildPagination(PaginationController paginationController){
        Layout result = paginationController.buildView();
        paginationController.getModel().addPaginationListener(this);
        return result;
    }

    protected abstract void buildGrid();


    protected Layout buildContent(){
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
        Layout content = this.buildContent();
        Layout navTools = this.buildToolbar();
        Layout paginationBar = this.buildPagination();

        VLayout itemsViewLayout = new VLayout();
        itemsViewLayout.addMember(navTools);
        itemsViewLayout.addMember(content);
        itemsViewLayout.addMember(paginationBar);

        return itemsViewLayout;
    }
}
