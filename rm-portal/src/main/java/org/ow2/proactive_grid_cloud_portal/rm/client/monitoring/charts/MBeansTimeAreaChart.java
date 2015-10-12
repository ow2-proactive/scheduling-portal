/*
 * ################################################################
 *
 * ProActive Parallel Suite(TM): The Java(TM) library for
 *    Parallel, Distributed, Multi-Core Computing for
 *    Enterprise Grids & Clouds
 *
 * Copyright (C) 1997-2015 INRIA/University of
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
 * ################################################################
 * $$PROACTIVE_INITIAL_DEV$$
 */
package org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.charts;

import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import org.ow2.proactive_grid_cloud_portal.common.client.Model;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMController;

import com.google.gwt.visualization.client.DataTable;
import com.google.gwt.visualization.client.LegendPosition;
import com.google.gwt.visualization.client.AbstractDataTable.ColumnType;
import com.google.gwt.visualization.client.visualizations.corechart.AreaChart;
import com.google.gwt.visualization.client.visualizations.corechart.CoreChart;
import com.google.gwt.visualization.client.visualizations.corechart.Options;

import java.util.LinkedHashMap;


/**
 * Chart that retrieves information from several MBeans and shows time on Y axis.
 */
public abstract class MBeansTimeAreaChart extends MBeansChart {

    private boolean initializing = true;

    public MBeansTimeAreaChart(RMController controller, String jmxServerUrl, String mbean, String attribute,
            String title) {
        this(controller, jmxServerUrl, mbean, new String[] { attribute }, title);
    }

    public MBeansTimeAreaChart(RMController controller, String jmxServerUrl, String mbean,
            String[] attributes, String title) {
        super(controller, jmxServerUrl, mbean, attributes, title);

        loadOpts.setLegend(LegendPosition.RIGHT);
        loadTable.addColumn(ColumnType.STRING);
        // fake column to draw the chart properly
        // with mbeans we don't know how many columns we will have until receive first results
        loadTable.addColumn(ColumnType.NUMBER);
    }

    public boolean initColumns() {
        if (initializing) {
            // removing fake column
            loadTable.removeColumn(1);
            initializing = false;
            return true;
        }

        return false;
    }

    @Override
    public CoreChart createChart(DataTable data, Options opts) {
        return new AreaChart(data, opts);
    }

}
