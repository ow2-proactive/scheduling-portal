/*
 * ################################################################
 *
 * ProActive Parallel Suite(TM): The Java(TM) library for
 *    Parallel, Distributed, Multi-Core Computing for
 *    Enterprise Grids & Clouds
 *
 * Copyright (C) 1997-2011 INRIA/University of
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
import com.google.gwt.json.client.JSONParser;
import com.google.gwt.visualization.client.visualizations.corechart.AxisOptions;


/**
 * Shows the JVM CPU consumption.
 */
public class JVMCpuUsageAreaChart extends MBeanTimeAreaChart {

    long prevCpu = 0;
    long prevTime = 0;

    public JVMCpuUsageAreaChart(RMController controller, String jmxServerUrl) {
        super(controller, jmxServerUrl, "java.lang:type=OperatingSystem", "ProcessCpuTime", "Cpu Usage");

        AxisOptions vAxis = AxisOptions.create();
        vAxis.set("format", "#%");
        loadOpts.setVAxisOptions(vAxis);
    }

    @Override
    public void processResult(String result) {
        JSONArray array = controller.parseJSON(result).isArray();
        if (array != null) {
            long curTime = System.currentTimeMillis();
            long curCpu = (long) array.get(0).isObject().get("value").isNumber().doubleValue();

            if (prevTime > 0) {
                addRow();

                String formattedTime = DateTimeFormat.getFormat(PredefinedFormat.HOUR24_MINUTE).format(
                        new Date(curTime));
                double cpuPercent = (curCpu - prevCpu) / (10000 * (curTime - prevTime));
                // as it will be formatted to percents divide by 100
                cpuPercent /= 100;
                loadTable.setValue(loadTable.getNumberOfRows() - 1, 0, formattedTime);
                loadTable.setValue(loadTable.getNumberOfRows() - 1, 1, cpuPercent);
            }

            prevCpu = curCpu;
            prevTime = curTime;

            loadChart.draw(loadTable, loadOpts);
        }
    }

}
