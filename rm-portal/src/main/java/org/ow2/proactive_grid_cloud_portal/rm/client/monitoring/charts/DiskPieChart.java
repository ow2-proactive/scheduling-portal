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

import org.ow2.proactive_grid_cloud_portal.rm.client.RMController;

import com.google.gwt.json.client.JSONObject;
import com.google.gwt.regexp.shared.MatchResult;
import com.google.gwt.regexp.shared.RegExp;
import com.google.gwt.visualization.client.AbstractDataTable.ColumnType;
import com.google.gwt.visualization.client.DataTable;
import com.google.gwt.visualization.client.LegendPosition;
import com.google.gwt.visualization.client.visualizations.corechart.CoreChart;
import com.google.gwt.visualization.client.visualizations.corechart.Options;
import com.google.gwt.visualization.client.visualizations.corechart.PieChart;


/**
 * Shows the disks total space in MB.
 */
public class DiskPieChart extends MBeansChart {

    public DiskPieChart(RMController controller, String jmxServerUrl) {
        super(controller, jmxServerUrl, "sigar:Type=FileSystem,Name=*", new String[] { "Total" }, "File System, Mb");

        loadOpts.setLegend(LegendPosition.RIGHT);
        loadTable.addColumn(ColumnType.STRING, "Type");
        loadTable.addColumn(ColumnType.NUMBER, "Mb");
    }

    @Override
    public void processResult(String result) {

        JSONObject object = controller.parseJSON(result).isObject();
        if (object != null) {

            loadTable.removeRows(0, loadTable.getNumberOfRows());
            for (String key : object.keySet()) {
                addRow();

                double value = object.get(key).isArray().get(0).isObject().get("value").isNumber().doubleValue();
                long inMB = (long) (value / 1024);
                loadTable.setValue(loadTable.getNumberOfRows() - 1, 0, beautifyName(key));
                loadTable.setValue(loadTable.getNumberOfRows() - 1, 1, inMB);
            }

            loadChart.draw(loadTable, loadOpts);
        }
    }

    private String beautifyName(String mbeanName) {
        // sigar:Name=lo,Type=NetInterface
        String patternStr = "sigar:Name=(.*),Type=FileSystem";
        RegExp pattern = RegExp.compile(patternStr);
        MatchResult matcher = pattern.exec(mbeanName);
        return matcher.getGroup(1);
    }

    @Override
    public CoreChart createChart(DataTable data, Options opts) {
        return new PieChart(data, opts);
    }
}
