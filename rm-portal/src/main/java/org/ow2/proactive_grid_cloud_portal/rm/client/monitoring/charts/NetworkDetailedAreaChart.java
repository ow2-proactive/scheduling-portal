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
import com.google.gwt.json.client.JSONObject;
import com.google.gwt.json.client.JSONValue;
import com.google.gwt.visualization.client.LegendPosition;
import com.google.gwt.visualization.client.visualizations.corechart.AxisOptions;


/**
 * Shows full detailes about all network interfaces on host and current network load charts.
 */
public class NetworkDetailedAreaChart extends MBeanTimeAreaChart {

    // store last tx bytes & time stamp
    private long[] history;

    private long[] time;

    public NetworkDetailedAreaChart(RMController controller, String jmxServerUrl, String bean) {
        super(controller, jmxServerUrl, bean, new String[] { "RxBytes", "TxBytes" }, "");

        history = new long[2];
        time = new long[2];

        AxisOptions vAxis = AxisOptions.create();
        vAxis.set("format", "#.# Kb/s");
        loadOpts.setVAxisOptions(vAxis);
        loadOpts.setLegend(LegendPosition.RIGHT);
        loadTable.setColumnLabel(1, "RX");
        loadTable.setColumnLabel(2, "TX");
    }

    @Override
    public void processResult(String result) {
        //        JSONArray array = controller.parseJSON(result).isArray();
        //        if (array != null) {
        //            String timeStamp = DateTimeFormat.getFormat(PredefinedFormat.HOUR24_MINUTE)
        //                                             .format(new Date(System.currentTimeMillis()));
        //            addRow();
        //
        //            loadTable.setValue(loadTable.getNumberOfRows() - 1, 0, timeStamp);
        //
        //             getting primitive values of all attributes
        //            for (int i = 0; i < attrs.length; i++) {
        //                double value = array.get(i).isObject().get("value").isNumber().doubleValue();
        //                long t = System.currentTimeMillis();
        //                if (history[i] > 0) {
        //                    double bytePerMilliSec = (value - history[i]) / (t - time[i]);
        //                    double mbPerSec = bytePerMilliSec * 1000 / 1024;
        //                    loadTable.setValue(loadTable.getNumberOfRows() - 1, i + 1, (long) mbPerSec);
        //                } else {
        //                    loadTable.setValue(loadTable.getNumberOfRows() - 1, i + 1, 0);
        //                }
        //
        //                history[i] = (long) value;
        //                time[i] = t;
        //            }
        //
        //            chart.draw(loadTable, loadOpts);
        //        }
    }

    //    @Override
    public void processHistoryResult1(String result) {

        // removing internal escaping
        result = result.replace("\\\"", "\"");
        result = result.replace("\"{", "{");
        result = result.replace("}\"", "}");

        JSONValue resultVal = controller.parseJSON(result);
        JSONObject json = resultVal.isObject();

        if (json == null) {
            return;
        }

        loadTable.removeRows(0, loadTable.getNumberOfRows());
        long now = new Date().getTime() / 1000;
        long dur = timeRange.getDuration();
        int size = getJsonInternalSize(json);
        long step = dur / size;

        for (int i = 1; i < size; i++) {

            double[] slice = getJsonSlice(json, i);

            if (i == 1) {
                time = new long[slice.length];
                history = new long[slice.length];
            }

            long t = now - dur + step * (i - 1);
            String timeStamp = DateTimeFormat.getFormat(PredefinedFormat.HOUR24_MINUTE).format(new Date(t * 1000));

            loadTable.addRow();
            loadTable.setValue(i - 1, 0, timeStamp);

            for (int j = 0; j < slice.length; j++) {
                long value = (long) slice[j];

                if (i > 1) {
                    double bytePerSec = (value - history[j]) / (t - time[j]);
                    double kbPerSec = bytePerSec / 1024;

                    if (kbPerSec < 0) {
                        // rx counter is reset
                        kbPerSec = 0;
                    }

                    loadTable.setValue(i - 1, j + 1, (long) kbPerSec);
                }

                history[j] = value;
                time[j] = t;
            }
        }

        //        chart.draw(loadTable, loadOpts);
    }

}
