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

import java.io.Serializable;


@SuppressWarnings("serial")
public abstract class ItemGrid implements Serializable, Comparable<ItemGrid> {

    protected long id;

    protected String name;

    /**
     * The constructor that has no arguments required by the Serializable interface
     */
    public ItemGrid() {
    }

    public ItemGrid(long id, String name) {
        this.id = id;
        this.name = name;
    }

    /**
     * Setter of the job ID.
     * @param id the new ID that will be set.
     */
    public void setId(long id) {
        this.id = id;
    }

    /**
     * Getter of the job Id.
     * @return the Id of the job.
     */
    public long getId() {
        return id;
    }

    /**
     * Setter of the job name.
     * @param name the name of the job.
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * Getter for the name of the job.
     * @return the username.
     */
    public String getName() {
        return name;
    }

    public int compareTo(ItemGrid item) {
        return ((Long) this.id).compareTo(item.getId());
    }

    @Override
    public int hashCode() {
        return (int) this.id;
    }

    @Override
    public boolean equals(Object o) {
        if (!(o instanceof ItemGrid))
            return false;

        return this.id == ((ItemGrid) o).getId();
    }
}
