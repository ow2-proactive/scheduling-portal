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

package org.ow2.proactive_grid_cloud_portal.scheduler.client.model;

import java.util.ArrayList;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.PaginationListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.shared.PaginatedItemType;
import org.ow2.proactive_grid_cloud_portal.scheduler.shared.SchedulerConfig;

/**
 * A model for the pagination of items.
 * @author activeeon team
 *
 */
public class PaginationModel {

    /**
     * The current displayed page.
     */
    private int currentPage = 0;

    /**
     * The type of item to be paginated.
     */
    private PaginatedItemType itemType;

    /**
     * Listeners for pagination events.
     */
    protected ArrayList<PaginationListener> paginationListeners;


    public PaginationModel(PaginatedItemType itemType){
        this.itemType = itemType;
        this.paginationListeners = new ArrayList<PaginationListener>();
    }


    /**
     * Gets the current displayed page.
     * @return the current displayed page.
     */
    public int getPage() {
        return this.currentPage;
    }


    /**
     * Change the current page
     * 
     * @param page new page number
     */
    public void setPage(int page) {
        this.currentPage = page;
        for(PaginationListener listener: this.paginationListeners){
            listener.pageChanged();
        }
    }


    /**
     * Gets the size of a page.
     * @return the size of a page.
     */
    public int getPageSize() {
        return SchedulerConfig.get().getPageSize(this.itemType);
    }


    public PaginatedItemType getItemType() {
        return itemType;
    }


    public void addPaginationListener(PaginationListener listener){
        this.paginationListeners.add(listener);
    }


    /**
     * Gets the offset of the items to be retrieved for the current page.
     * @return the offset of the items to be retrieved for the current page.
     */
    public int getOffset(){
        return (this.currentPage * this.getPageSize());
    }


    /**
     * Gets the index of the last item to be retrieved for the current page.
     * @return the index of the last item to be retrieved for the current page.
     */
    public int getRange(){
        return this.getPageSize() * (this.currentPage + 1);
    }

}
