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
package org.ow2.proactive_grid_cloud_portal.scheduler.client.model;

import org.ow2.proactive_grid_cloud_portal.scheduler.shared.PaginatedItemType;


/**
 * A model for the pagination of items.
 * @author activeeon team
 *
 */
public class TasksPaginationModel extends PaginationModel {

    /**
     * The current displayed page.
     */
    private int currentPage = 0;

    /**
     * The number of the last page.
     */
    private int maxPage = 0;

    public TasksPaginationModel() {
        super(PaginatedItemType.TASK);
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
        doActionOnListeners(listener -> listener.pageChanged());
    }

    /**
     * Gets the offset of the items to be retrieved for the current page.
     * @return the offset of the items to be retrieved for the current page.
     */
    public int getOffset() {
        return (this.currentPage * this.getPageSize());
    }

    /**
     * Gets the number of the last page.
     * @return the number of the last page.
     */
    public int getMaxPage() {
        return maxPage;
    }

    /**
     * Sets the total number of items without pagination.
     * @param totalItems the total number of items without pagination.
     */
    public void setTotalItems(long totalItems) {
        this.totalItems = totalItems;
        int pageSize = this.getPageSize();
        this.maxPage = ((int) this.totalItems / pageSize) - 1;
        if (this.totalItems % pageSize != 0) {
            this.maxPage++;
        }
        doActionOnListeners(listener -> listener.totalItemChanged());
    }
}
