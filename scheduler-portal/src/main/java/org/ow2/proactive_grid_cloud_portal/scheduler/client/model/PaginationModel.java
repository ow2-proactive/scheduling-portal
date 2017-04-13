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

import java.util.ArrayList;
import java.util.function.Consumer;

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
     * The total number of items to be displayed without pagination.
     */
    protected long totalItems = 0;

    /**
     * Listeners for pagination events.
     */
    protected final ArrayList<PaginationListener> paginationListeners;

    /**
     * The type of item to be paginated.
     */
    protected PaginatedItemType itemType;

    /**
     * Constructor
     */
    public PaginationModel(PaginatedItemType itemType) {
        this.itemType = itemType;
        this.paginationListeners = new ArrayList<PaginationListener>();
    }

    /**
     * Add a listener for pagination events.
     * @param listener the listener.
     */
    public void addPaginationListener(PaginationListener listener) {
        this.paginationListeners.add(listener);
    }

    /**
     * Get the total number of items without pagination.
     * @return the total number of items without pagination.
     */
    public long getTotalItems() {
        return totalItems;
    }

    /**
     * Apply an action on all listeners
     */
    protected void doActionOnListeners(Consumer<PaginationListener> listenerAction) {
        this.paginationListeners.stream().forEach(listenerAction);
    }

    /**
     * Sets the total number of items without pagination.
     * @param totalItems the total number of items without pagination.
     */
    public void setTotalItems(long totalItems) {
        this.totalItems = totalItems;
    }

    /**
     * Gets the size of a page.
     * @return the size of a page.
     */
    public int getPageSize() {
        return SchedulerConfig.get().getPageSize(this.itemType);
    }

    /**
     * Gets the type of items to be paginated (TASK or JOB)
     * @return the type of items to be paginated (TASK or JOB)
     */
    public PaginatedItemType getItemType() {
        return itemType;
    }
}
