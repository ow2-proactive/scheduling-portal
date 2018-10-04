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

import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.PaginationListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.shared.PaginatedItemType;


/**
 * A model for the pagination of items.
 * @author activeeon team
 *
 */
public class JobsPaginationModel extends PaginationModel {

    /**
     * The current displayed page.
     */
    private int currentPage = 0;

    private int maxPage = 0;

    /**
     * The first displayed job cursor.
     */
    private String currentStartCursor;

    /**
     * The first displayed job cursor.
     */
    private String startCursor;

    /**
     * The first displayed job cursor.
     */
    private String currentEndCursor;

    /**
     * The last displayed job cursor.
     */
    private String endCursor;

    /**
     * If a previous page exists
     */
    private boolean hasPreviousPage;

    /**
     * If a following page exists
     */
    private boolean hasNextPage;

    /**
     * If the first or last objects are to be fetched
     */
    private boolean first;

    /**
     * Constructor
     */
    public JobsPaginationModel() {
        super(PaginatedItemType.JOB);
    }

    public String getCurrentStartCursor() {
        return currentStartCursor;
    }

    public void setCurrentStartCursor(String currentStartCursor) {
        this.currentStartCursor = currentStartCursor;
    }

    public String getStartCursor() {
        return startCursor;
    }

    /**
     * Set start, end cursors and first status for jobs fetch request
     * @param startCursor start cursor
     * @param endCursor end cursor
     * @param first first or last items
     */
    public void setFetchData(String startCursor, String endCursor, boolean first) {
        this.startCursor = startCursor;
        this.endCursor = endCursor;
        this.first = first;
        for (PaginationListener listener : this.paginationListeners) {
            listener.pageChanged();
        }
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

    public void setPage(int page) {
        this.currentPage = page;
        doActionOnListeners(listener -> listener.pageChanged());
    }

    /**
     * Gets the current displayed page.
     * @return the current displayed page.
     */
    public int getPage() {
        return this.currentPage;
    }

    public String getCurrentEndCursor() {
        return currentEndCursor;
    }

    public void setCurrentEndCursor(String currentEndCursor) {
        this.currentEndCursor = currentEndCursor;
    }

    public String getEndCursor() {
        return endCursor;
    }

    public boolean hasPreviousPage() {
        return hasPreviousPage;
    }

    public void setHasPreviousPage(boolean hasPreviousPage) {
        this.hasPreviousPage = hasPreviousPage;
    }

    public boolean hasNextPage() {
        return hasNextPage;
    }

    public void setHasNextPage(boolean hasNextPage) {
        this.hasNextPage = hasNextPage;
    }

    public boolean isFirst() {
        return first;
    }
}
