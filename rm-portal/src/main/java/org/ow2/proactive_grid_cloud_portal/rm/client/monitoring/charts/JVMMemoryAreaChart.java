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
package org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.charts;

import java.util.Date;

import org.ow2.proactive_grid_cloud_portal.rm.client.RMController;

import com.google.gwt.i18n.client.DateTimeFormat;
import com.google.gwt.i18n.client.DateTimeFormat.PredefinedFormat;
import com.google.gwt.json.client.JSONArray;
import com.google.gwt.visualization.client.visualizations.corechart.AxisOptions;


/**
 * Shows the JVM memory consumption.
 */
public class JVMMemoryAreaChart extends MBeanTimeAreaChart {

    public JVMMemoryAreaChart(RMController controller, String jmxServerUrl) {
        super(controller, jmxServerUrl, "java.lang:type=Memory", "HeapMemoryUsage", "Heap Memory Usage");
        AxisOptions vAxis = AxisOptions.create();
        vAxis.set("format", "# Mb");
        loadOpts.setVAxisOptions(vAxis);
    }

    @Override
    public double formatValue(double value) {
        return (long) (value / (1024 * 1024));
    }

    @Override
    public void processResult(String result) {
        //        JSONArray array = controller.parseJSON(result).isArray();
        //        if (array != null) {
        //            String timeStamp = DateTimeFormat.getFormat(PredefinedFormat.HOUR24_MINUTE)
        //                                             .format(new Date(System.currentTimeMillis()));
        //            double value = array.get(0).isObject().get("value").isObject().get("used").isNumber().doubleValue();
        //
        //            addRow();
        //            loadTable.setValue(loadTable.getNumberOfRows() - 1, 0, timeStamp);
        //            loadTable.setValue(loadTable.getNumberOfRows() - 1, 1, formatValue(value));
        //            chart.draw(loadTable, loadOpts);
        //        }
    }
}
