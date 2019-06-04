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
import java.util.Date;
import java.util.List;

import org.ow2.proactive_grid_cloud_portal.rm.client.RMController;
import org.pepstock.charba.client.data.Dataset;
import org.pepstock.charba.client.data.LineDataset;
import org.pepstock.charba.client.enums.Fill;
import org.pepstock.charba.client.enums.Position;

import com.google.gwt.i18n.client.DateTimeFormat;
import com.google.gwt.i18n.client.DateTimeFormat.PredefinedFormat;
import com.google.gwt.json.client.JSONObject;
import com.google.gwt.json.client.JSONValue;
import com.google.gwt.regexp.shared.MatchResult;
import com.google.gwt.regexp.shared.RegExp;


/**
 * Shows all network interfaces on host.
 */
public class NetworkAreaChart extends MBeansTimeAreaChart {

    // store last tx bytes & time stamp
    private long time[];

    private long txBytes[];

    public NetworkAreaChart(RMController controller, String jmxServerUrl) {
        super(controller, jmxServerUrl, "sigar:Type=NetInterface,Name=*", "RxBytes", "Network");

        setYAxesTicksSuffix(" Kb/s");
    }

    @Override
    public void processResult(String result) {

        //         Result:{"sigar:Name=lo,Type=NetInterface":[{"name":"TxBytes","value":147762795896}],"sigar:Name=eth0,Type=NetInterface":[{"name":"TxBytes","value":249539647369}]}
        JSONObject object = controller.parseJSON(result).isObject();
        if (object != null) {

            String timeStamp = DateTimeFormat.getFormat(PredefinedFormat.HOUR24_MINUTE)
                                             .format(new Date(System.currentTimeMillis()));

            addLabel(timeStamp);

            boolean initColumns = super.initColumns();

            if (initColumns) {
                time = new long[object.size()];
                txBytes = new long[object.size()];
            }

            int colIndex = 1;
            for (String key : object.keySet()) {

//                if (initColumns) {
//                    loadTable.addColumn(ColumnType.NUMBER, beautifyName(key));
//                }

                long value = Long.parseLong(object.get(key).isArray().get(0).isObject().get("value").toString());
                long t = System.currentTimeMillis();
                if (txBytes[colIndex - 1] > 0) {
                    double bytePerMilliSec = (value - txBytes[colIndex - 1]) / (t - time[colIndex - 1]);
                    double mbPerSec = bytePerMilliSec * 1000 / 1024;
                    addPointToDataset(colIndex, (long) mbPerSec);
                }

                txBytes[colIndex - 1] = value;
                time[colIndex - 1] = t;

                colIndex++;
            }

            chart.update();
        }
    }

    @Override
    public void processHistoryResult(String result) {
        result = removingInternalEscaping(result);

        JSONValue resultVal = controller.parseJSON(result);
        JSONObject json = resultVal.isObject();

        if (json == null) {
            return;
        }

//        loadTable.removeRows(0, loadTable.getNumberOfRows());

        long now = new Date().getTime() / 1000;
        long dur = timeRange.getDuration();
        int size = getJsonInternalSize(json);
        long step = dur / size;

        final int length = getJsonSlice(json, 0).length;

        List<Dataset> datasets = new ArrayList<>();
        List<Double>[] dpss = new List[length];

        String[] colors = new String[] { "#fcaf3e", "#3a668d", "#35a849", "#fcaf3e", "#24c1ff", "#1e4ed7", "#ef2929",
                                         "#000000" };
        for (int i = 0; i < length; ++i) {
            LineDataset dataset = (LineDataset) chart.newDataset();
            if (i < colors.length) {
                dataset.setBorderColor(colors[i]);
            }
            dataset.setFill(Fill.FALSE);
            dataset.setLabel(String.valueOf(i));
            datasets.add(dataset);
            dpss[i] = new ArrayList<>();
        }
        List<String> labels = new ArrayList<>();

        for (int i = 0; i < size; i++) {

            double[] slice = getJsonSlice(json, i);

            if (i == 1) {
                time = new long[slice.length];
                txBytes = new long[slice.length];
            }

            long t = now - dur + step * i;
            DateTimeFormat.PredefinedFormat format = timeRange.getFormat();
            String timeStamp = DateTimeFormat.getFormat(format).format(new Date(t * 1000));

            labels.add(timeStamp);

            for (int sliceIndex = 0; sliceIndex < slice.length; sliceIndex++) {

                long value = (long) slice[sliceIndex];

                if (i > 1) {
                    double bytePerSec = (value - txBytes[sliceIndex]) / (t - time[sliceIndex]);
                    double kbPerSec = bytePerSec / 1024;

                    if (kbPerSec < 0) {
                        // rx counter is reset
                        kbPerSec = 0;
                    }

                    dpss[sliceIndex].add(kbPerSec);
                }

                txBytes[sliceIndex] = value;
                time[sliceIndex] = t;

                //                dpss[sliceIndex].add(formatValue(slice[sliceIndex]));
            }
        }

        chart.getData().setLabels(labels.toArray(new String[0]));

        chart.getOptions().getLegend().setPosition(Position.RIGHT);
        for (int i = 0; i < length; ++i) {
            datasets.get(i).setData(dpss[i]);
        }

        chart.getData().setDatasets(datasets.toArray(new Dataset[0]));

        chart.update();

    }

    private String beautifyName(String mbeanName) {
        // sigar:Name=lo,Type=NetInterface
        String patternStr = "sigar:Name=(.*),Type=NetInterface";
        RegExp pattern = RegExp.compile(patternStr);
        MatchResult matcher = pattern.exec(mbeanName);
        return matcher.getGroup(1);
    }
}
