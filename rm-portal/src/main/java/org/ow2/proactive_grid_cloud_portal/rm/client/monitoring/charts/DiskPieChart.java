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

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

import org.ow2.proactive_grid_cloud_portal.rm.client.RMController;
import org.pepstock.charba.client.AbstractChart;
import org.pepstock.charba.client.PieChart;
import org.pepstock.charba.client.data.PieDataset;

import com.google.gwt.json.client.JSONObject;
import com.google.gwt.regexp.shared.MatchResult;
import com.google.gwt.regexp.shared.RegExp;


/**
 * Shows the disks total space in MB.
 */
public class DiskPieChart extends MBeansChart {

    private int total = 0;

    public DiskPieChart(RMController controller, String jmxServerUrl) {
        super(controller, jmxServerUrl, "sigar:Type=FileSystem,Name=*", new String[] { "Total" }, "File System, Mb");
        setYAxesTicksSuffix(" Mb");
        setTooltipItemHandler(new Function<String, String>() {
            @Override
            public String apply(String s) {
                int indexOfSpace = s.lastIndexOf(" ");
                String firstHalf = s.substring(0, indexOfSpace);
                String secondHalf = s.substring(indexOfSpace + 1);

                if (secondHalf.contains(".")) {
                    secondHalf = secondHalf.substring(0, secondHalf.indexOf("."));
                }

                long valueInMb = Long.parseLong(secondHalf);

                long percentage = (long) (((double) valueInMb / total) * 100);

                secondHalf = putCommasEveryThreeDigits(secondHalf);

                return firstHalf + " " + secondHalf + " Mb (" + percentage + "%)";
            }
        });
    }

    public static String putCommasEveryThreeDigits(String number) {
        StringBuilder sb = new StringBuilder();
        sb.append(number.charAt(0));
        for (int i = 1; i < number.length(); ++i) {

            // when nubmer of digits left to read is dividable by 3
            if ((number.length() - i) % 3 == 0) {
                sb.append(",");
            }

            sb.append(number.charAt(i));
        }
        return sb.toString();
    }

    @Override
    public void processResult(String result) {
        JSONObject object = controller.parseJSON(result).isObject();
        if (object != null) {

            PieDataset dataset = (PieDataset) chart.newDataset();
            dataset.setBackgroundColor(getColors());
            List<String> labels = new ArrayList<>();
            List<Double> values = new ArrayList<>();
            total = 0;
            for (String key : object.keySet()) {

                double value = object.get(key).isArray().get(0).isObject().get("value").isNumber().doubleValue();
                long inMB = (long) (value / 1024);

                total += inMB;

                labels.add(beautifyName(key));

                values.add((double) inMB);
            }

            dataset.setData(values);

            chart.getData().setLabels(labels.toArray(new String[0]));

            chart.getData().setDatasets(dataset);

            chart.update();
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
    public AbstractChart createChart() {
        return new PieChart();
    }
}
