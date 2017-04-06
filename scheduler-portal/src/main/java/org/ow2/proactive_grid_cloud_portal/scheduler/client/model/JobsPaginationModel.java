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


/**
 * A model for the pagination of items.
 * @author activeeon team
 *
 */
public class JobsPaginationModel extends PaginationModel {

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

    public void setStartAndEndCursor(String startCursor, String endCursor) {
        this.startCursor = startCursor;
        this.endCursor = endCursor;
        for (PaginationListener listener : this.paginationListeners) {
            listener.pageChanged();
        }
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

    public void setFirst(boolean first) {
        this.first = first;
    }

    /**
     * Add a listener for pagination events.
     * @param listener the listener.
     */
    public void addPaginationListener(PaginationListener listener) {
        this.paginationListeners.add(listener);
    }
}
