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

import com.smartgwt.client.data.Record;


/**
 * A factory that allows to get the list of columns, and build record according to these columns.
 * @author The activeeon team.
 *
 * @param <I> the type of the item used to build records for the columns.
 */
public interface ColumnsFactory<I> {

    /**
     * Gets the list of columns.
     * @return the list of columns.
     */
    GridColumns[] getColumns();

    /**
     * Builds a record from a given item according to the columns provided by this factory.
     * @param item the item used to build a new record.
     * @param record the new record to be built with new attributes.
     */
    void buildRecord(I item, Record record);

}
