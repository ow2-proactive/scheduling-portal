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

import java.util.function.Function;

import org.ow2.proactive_grid_cloud_portal.rm.client.RMController;
import org.pepstock.charba.client.AbstractChart;
import org.pepstock.charba.client.LineChart;


/**
 * Shows the swap consumption.
 */
public class SwapLineChart extends MBeanTimeAreaChart {

    public SwapLineChart(RMController controller, String jmxServerUrl) {
        super(controller, jmxServerUrl, "sigar:Type=Swap", new String[] { "Used", "Free", "Total" }, "Swap");

        setYAxesTicksSuffix(" Mb");

        setDatasourceNames("Used", "Free", "Total");
        setColors("#fcaf3e", "#35a849", "#3a668d");
        setTooltipItemHandler(new Function<String, String>() {
            @Override
            public String apply(String s) {
                return s + " Mb";
            }
        });
        setTooltipItemHandler(new Function<String, String>() {
            @Override
            public String apply(String s) {
                int indexOfSpace = s.lastIndexOf(" ");
                String firstHalf = s.substring(0, indexOfSpace);
                String number = s.substring(indexOfSpace + 1);

                number = MBeanChart.keepNDigitsAfterComma(number, 0);

                Long valueInKb = (Long.parseLong(number) * 1024);

                number = valueInKb.toString();

                number = MBeanChart.addUnitDependsOnSize(number, VOLUME_UNITS);

                return firstHalf + " " + number;
            }
        });
    }

    @Override
    public double formatValue(double value) {
        return (long) (value / (1024 * 1024));
    }

    @Override
    public AbstractChart createChart() {
        return new LineChart();
    }
}
