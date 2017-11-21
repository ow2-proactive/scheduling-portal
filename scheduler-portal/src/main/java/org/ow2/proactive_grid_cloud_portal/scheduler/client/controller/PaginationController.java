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
package org.ow2.proactive_grid_cloud_portal.scheduler.client.controller;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.PaginationModel;

import com.smartgwt.client.widgets.layout.Layout;


/**
 * The controller for the item pagination logic.
 * @author the activeeon team
 *
 */
public abstract class PaginationController<T extends PaginationModel> {

    protected T model;

    /**
     * Builds a controller for the pagination logic.
     */
    public PaginationController(T model) {
        this.model = model;
    }

    /**
     * Fetch the first items list page
     */
    public abstract void firstPage();

    /**
     * Fetch the last items list page
     */
    public abstract void lastPage();

    /**
     * Fetch the next items list page
     */
    public abstract void nextPage();

    /**
     * Fetch the previous items list page
     */
    public abstract void previousPage();

    /**
     * Fetch the items for the current page.
     */
    public abstract void fetch(boolean silentFetch);

    /**
     * Build the view
     * @return the built Layout
     */
    public abstract Layout buildView();

    /**
     * Returns true if there is item before the current list of items, false otherwise.
     * @return true if there is item before the current list of items, false otherwise.
     */
    public abstract boolean hasPrevious();

    /**
     * Returns true if there is item after the current list of items, false otherwise.
     * @return true if there is item after the current list of items, false otherwise.
     */
    public abstract boolean hasNext();

    public void refresh() {
        this.fetch(true);
    }

    public T getModel() {
        return model;
    }
}
