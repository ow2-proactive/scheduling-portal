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
import java.util.function.Function;

import org.ow2.proactive_grid_cloud_portal.rm.client.RMController;
import org.pepstock.charba.client.AbstractChart;
import org.pepstock.charba.client.LineChart;

import com.google.gwt.i18n.client.DateTimeFormat;
import com.google.gwt.i18n.client.DateTimeFormat.PredefinedFormat;
import com.google.gwt.json.client.JSONObject;
import com.google.gwt.json.client.JSONValue;
import com.google.gwt.regexp.shared.MatchResult;
import com.google.gwt.regexp.shared.RegExp;


/**
 * Shows the CPU usage per core.
 */
public class CpusUsageLineChart extends MBeansTimeAreaChart {

    public CpusUsageLineChart(RMController controller, String jmxServerUrl) {
        super(controller, jmxServerUrl, "sigar:Type=CpuCoreUsage,Name=*", "Combined", "Load History");

        setYAxesTicksSuffix("%");

        setAreaChart(false);
        chartContainer.setHeight("300px");

        setTooltipItemHandler(new Function<String, String>() {
            @Override
            public String apply(String s) {
                return MBeanChart.parseAndKeepOnlyNDigits(s, 2) + "%";
            }
        });
    }

    @Override
    public double formatValue(double value) {
        return super.formatValue(value * 100);
    }

    @Override
    public void processResult(String result) {
        JSONObject object = controller.parseJSON(result).isObject();
        if (object != null) {

            String timeStamp = DateTimeFormat.getFormat(PredefinedFormat.HOUR24_MINUTE)
                                             .format(new Date(System.currentTimeMillis()));

            addXLabel(timeStamp);

            //            boolean initColumns = super.initColumns();
            int colIndex = 0;
            for (String key : object.keySet()) {

                //                if (initColumns) {
                //                    loadTable.addColumn(ColumnType.NUMBER, beautifyName(key));
                //                }

                double value = 0;
                JSONValue jsonVal = object.get(key).isArray().get(0).isObject().get("value");
                if (jsonVal != null && jsonVal.isNumber() != null) {
                    value = jsonVal.isNumber().doubleValue();
                }
                addPointToDataset(colIndex++, value);
            }

            chart.update();
        }
    }

    private String beautifyName(String mbeanName) {
        // sigar:Name=lo,Type=NetInterface
        String patternStr = "sigar:Name=(.*),Type=CpuCore";
        RegExp pattern = RegExp.compile(patternStr);
        MatchResult matcher = pattern.exec(mbeanName);
        return matcher.getGroup(1);
    }

    @Override
    public AbstractChart createChart() {
        return new LineChart();
    }
}
