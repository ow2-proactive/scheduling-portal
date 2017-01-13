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
package org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid;

/**
 * A class that groups columns and datasource settings for all columns of a grid.
 * @author the activeeon team.
 *
 */
public class GridColumns {

    /**
     * The name of the column
     */
    protected final String name;

    /**
     * The title of the column, as shown in the grid header.
     */
    protected final String title;

    /**
     * The width of the column in the grid.
     */
    protected final int width;

    /**
     * True if this column is bound to a data in the datasource, false otherwise.
     */
    protected final boolean hasData;

    /**
     * True if this column is part of the primary key.
     */
    protected final boolean isPrimaryKey;

    public GridColumns(String name, String title, int width, boolean hasData, boolean isPrimaryKey) {
        this.name = name;
        this.title = title;
        this.width = width;
        this.hasData = hasData;
        this.isPrimaryKey = isPrimaryKey;
    }

    public String getName() {
        return name;
    }

    public String getTitle() {
        return title;
    }

    public int getWidth() {
        return width;
    }

    public boolean hasData() {
        return hasData;
    }

    public boolean isPrimaryKey() {
        return isPrimaryKey;
    }

}
