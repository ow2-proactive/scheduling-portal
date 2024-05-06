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
package org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.jobs;

import java.util.Map;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.util.JobColumnsUtil;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.ColumnsFactory;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.GridColumns;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.ui.Anchor;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.util.StringUtil;


/**
 * A factory that give columns and records for jobs.
 */
public class KeyValueColumnsFactory implements ColumnsFactory<Map.Entry<String, String>> {

    public static final GridColumns KEY_ATTR = new GridColumns("key", "Key", 120, true, true);

    public static final GridColumns VALUE_ATTR = new GridColumns("value", "Value", 120, true, false);

    private static final GridColumns[] COLUMNS = new GridColumns[] { KEY_ATTR, VALUE_ATTR };

    @Override
    public GridColumns[] getColumns() {
        return COLUMNS;
    }

    @Override
    public void buildRecord(Map.Entry<String, String> item, Record record) {
        record.setAttribute(KEY_ATTR.getName(), item.getKey());
        if (JobColumnsUtil.START_AT.equals(item.getKey()))
            record.setAttribute(VALUE_ATTR.getName(), JobColumnsUtil.getFormattedDateString(item.getValue()));
        else if (JobColumnsUtil.DOCUMENTATION.equals(item.getKey()))
            record.setAttribute(VALUE_ATTR.getName(),
                                new Anchor(item.getValue(),
                                           GWT.getHostPageBaseURL().replace("/scheduler/", "/doc/" + item.getValue()),
                                           "_blank"));
        else
            record.setAttribute(VALUE_ATTR.getName(), StringUtil.asHTML(item.getValue()));
    }

}
