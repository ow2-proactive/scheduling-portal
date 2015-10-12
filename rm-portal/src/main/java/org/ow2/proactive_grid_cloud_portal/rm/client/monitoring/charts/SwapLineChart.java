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

import java.util.Date;

import org.ow2.proactive_grid_cloud_portal.rm.client.RMController;

import com.google.gwt.i18n.client.DateTimeFormat;
import com.google.gwt.i18n.client.DateTimeFormat.PredefinedFormat;
import com.google.gwt.json.client.JSONArray;
import com.google.gwt.visualization.client.DataTable;
import com.google.gwt.visualization.client.LegendPosition;
import com.google.gwt.visualization.client.visualizations.corechart.AxisOptions;
import com.google.gwt.visualization.client.visualizations.corechart.CoreChart;
import com.google.gwt.visualization.client.visualizations.corechart.LineChart;
import com.google.gwt.visualization.client.visualizations.corechart.Options;


/**
 * Shows the swap consumption.
 */
public class SwapLineChart extends MBeanTimeAreaChart {

    public SwapLineChart(RMController controller, String jmxServerUrl) {
        super(controller, jmxServerUrl, "sigar:Type=Swap", new String[] { "Used", "Free", "Total" }, "Swap");
        AxisOptions vAxis = AxisOptions.create();
        vAxis.set("format", "# Mb");
        loadOpts.setVAxisOptions(vAxis);
        loadOpts.setLegend(LegendPosition.RIGHT);
        loadOpts.setColors("#fcaf3e", "#35a849", "#3a668d");
    }

    @Override
    public double formatValue(double value) {
        return (long) (value / (1024 * 1024));
    }

    @Override
    public void processResult(String result) {
        JSONArray array = controller.parseJSON(result).isArray();
        if (array != null) {
            String timeStamp = DateTimeFormat.getFormat(PredefinedFormat.HOUR24_MINUTE).format(
                    new Date(System.currentTimeMillis()));
            addRow();

            loadTable.setValue(loadTable.getNumberOfRows() - 1, 0, timeStamp);

            // getting primitive values of all attributes
            for (int i = 0; i < attrs.length; i++) {
                double value = array.get(i).isObject().get("value").isNumber().doubleValue();
                loadTable.setValue(loadTable.getNumberOfRows() - 1, i + 1, formatValue(value));
            }

            loadChart.draw(loadTable, loadOpts);
        }
    }

    @Override
    public CoreChart createChart(DataTable data, Options opts) {
        return new LineChart(data, opts);
    }
}
