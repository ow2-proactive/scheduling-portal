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
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;

import org.ow2.proactive_grid_cloud_portal.rm.client.RMController;

import com.google.gwt.i18n.client.DateTimeFormat;
import com.google.gwt.i18n.client.DateTimeFormat.PredefinedFormat;
import com.google.gwt.json.client.JSONObject;
import com.google.gwt.json.client.JSONValue;
import com.google.gwt.regexp.shared.MatchResult;
import com.google.gwt.regexp.shared.RegExp;
import com.google.gwt.visualization.client.AbstractDataTable.ColumnType;
import com.google.gwt.visualization.client.visualizations.corechart.AxisOptions;


/**
 * Shows all network interfaces on host.
 */
public class NetworkAreaChart extends MBeansTimeAreaChart {

    // store last tx bytes & time stamp
    private long time[];

    private long txBytes[];

    public NetworkAreaChart(RMController controller, String jmxServerUrl) {
        super(controller, jmxServerUrl, "sigar:Type=NetInterface,Name=*", "RxBytes", "Network");

        AxisOptions vAxis = AxisOptions.create();
        vAxis.set("format", "#.# Kb/s");
        loadOpts.setVAxisOptions(vAxis);
    }

    @Override
    public void processResult(String result) {

        // Result:{"sigar:Name=lo,Type=NetInterface":[{"name":"TxBytes","value":147762795896}],"sigar:Name=eth0,Type=NetInterface":[{"name":"TxBytes","value":249539647369}]}
        //        JSONObject object = controller.parseJSON(result).isObject();
        //        if (object != null) {
        //
        //            String timeStamp = DateTimeFormat.getFormat(PredefinedFormat.HOUR24_MINUTE)
        //                                             .format(new Date(System.currentTimeMillis()));
        //
        //            addRow();
        //            loadTable.setValue(loadTable.getNumberOfRows() - 1, 0, timeStamp);
        //
        //            boolean initColumns = super.initColumns();
        //
        //            if (initColumns) {
        //                time = new long[object.size()];
        //                txBytes = new long[object.size()];
        //            }
        //
        //            int colIndex = 1;
        //            for (String key : object.keySet()) {
        //
        //                if (initColumns) {
        //                    loadTable.addColumn(ColumnType.NUMBER, beautifyName(key));
        //                }
        //
        //                long value = Long.parseLong(object.get(key).isArray().get(0).isObject().get("value").toString());
        //                long t = System.currentTimeMillis();
        //                if (txBytes[colIndex - 1] > 0) {
        //                    double bytePerMilliSec = (value - txBytes[colIndex - 1]) / (t - time[colIndex - 1]);
        //                    double mbPerSec = bytePerMilliSec * 1000 / 1024;
        //                    loadTable.setValue(loadTable.getNumberOfRows() - 1, colIndex, (long) mbPerSec);
        //                }
        //
        //                txBytes[colIndex - 1] = value;
        //                time[colIndex - 1] = t;
        //
        //                colIndex++;
        //            }
        //
        //            loadChart.draw(loadTable, loadOpts);
        //        }
    }

    public void processHistoryResult(String result) {

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
                txBytes = new long[slice.length];
            }

            long t = now - dur + step * (i - 1);
            String timeStamp = DateTimeFormat.getFormat(PredefinedFormat.HOUR24_MINUTE).format(new Date(t * 1000));

            loadTable.addRow();
            loadTable.setValue(i - 1, 0, timeStamp);

            for (int j = 0; j < slice.length; j++) {
                long value = (long) slice[j];

                if (i > 1) {
                    double bytePerSec = (value - txBytes[j]) / (t - time[j]);
                    double kbPerSec = bytePerSec / 1024;

                    if (kbPerSec < 0) {
                        // rx counter is reset
                        kbPerSec = 0;
                    }

                    loadTable.setValue(i - 1, j + 1, (long) kbPerSec);
                }

                txBytes[j] = value;
                time[j] = t;
            }
        }

        //        loadChart.draw(loadTable, loadOpts);
    }

    private String beautifyName(String mbeanName) {
        // sigar:Name=lo,Type=NetInterface
        String patternStr = "sigar:Name=(.*),Type=NetInterface";
        RegExp pattern = RegExp.compile(patternStr);
        MatchResult matcher = pattern.exec(mbeanName);
        return matcher.getGroup(1);
    }
}
