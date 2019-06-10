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
package org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.views;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

import org.ow2.proactive_grid_cloud_portal.common.client.json.JSONUtils;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LogModel;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LoginModel;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMController;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMServiceAsync;
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.charts.DiskPieChart;
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.charts.MBeanChart;
import org.pepstock.charba.client.PieChart;
import org.pepstock.charba.client.data.PieDataset;

import com.google.gwt.json.client.JSONArray;
import com.google.gwt.json.client.JSONObject;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.viewer.DetailViewer;
import com.smartgwt.client.widgets.viewer.DetailViewerField;
import com.smartgwt.client.widgets.viewer.DetailViewerRecord;


/**
 * File System tab in host monitoring.
 */
public class FileSystemView extends VLayout {

    public FileSystemView(final RMController controller, String url) {
        setWidth100();

        final List<String> attrs = new ArrayList<String>();

        attrs.add("DevName");
        attrs.add("DirName");
        attrs.add("Files");
        attrs.add("Options");
        attrs.add("SysTypeName");
        attrs.add("Free");
        attrs.add("Used");
        attrs.add("Total");

        final RMServiceAsync rm = controller.getRMService();
        final long t = System.currentTimeMillis();

        final LoginModel loginModel = LoginModel.getInstance();

        // loading runtime info
        rm.getNodeMBeansInfo(loginModel.getSessionId(),
                             url,
                             "sigar:Type=FileSystem,Name=*",
                             attrs,
                             new AsyncCallback<String>() {
                                 public void onSuccess(String result) {
                                     if (!loginModel.isLoggedIn())
                                         return;

                                     LogModel.getInstance().logMessage("Fetched Runtime info in " +
                                                                       (System.currentTimeMillis() - t) + "ms");

                                     JSONObject object = controller.parseJSON(result).isObject();
                                     if (object != null) {

                                         for (String disk : object.keySet()) {

                                             HLayout diskLayout = new HLayout();

                                             DetailViewer details = new DetailViewer();
                                             DetailViewerRecord dv = new DetailViewerRecord();
                                             DetailViewerField[] fields = new DetailViewerField[attrs.size()];
                                             for (int i = 0; i < fields.length; i++) {
                                                 fields[i] = new DetailViewerField(attrs.get(i));
                                             }
                                             details.setFields(fields);
                                             details.setData(new DetailViewerRecord[] { dv });
                                             details.setWidth("50%");

                                             PieChart pie = new PieChart();
                                             PieDataset dataset = pie.newDataset();
                                             dataset.setBackgroundColor("#fcaf3e",
                                                                        "#3a668d",
                                                                        "#35a849",
                                                                        "#fcaf3e",
                                                                        "#24c1ff",
                                                                        "#1e4ed7",
                                                                        "#ef2929",
                                                                        "#000000");
                                             List<String> labels = new ArrayList<>();
                                             List<Double> values = new ArrayList<>();
                                             JSONArray properties = object.get(disk).isArray();
                                             long total = 0;
                                             for (int i = 0; i < properties.size(); i++) {
                                                 String name = properties.get(i)
                                                                         .isObject()
                                                                         .get("name")
                                                                         .isString()
                                                                         .stringValue();
                                                 String value = properties.get(i).isObject().get("value").toString();
                                                 if (name.equals("Free") || name.equals("Used")) {

                                                     double doubleValue = properties.get(i)
                                                                                    .isObject()
                                                                                    .get("value")
                                                                                    .isNumber()
                                                                                    .doubleValue();

                                                     labels.add(name);
                                                     values.add(doubleValue);
                                                     total += doubleValue;
                                                 }

                                                 if (name.equals("Free") || name.equals("Used") ||
                                                     name.equals("Total")) {
                                                     final int inKb = Integer.parseInt(value);
                                                     int inMb = inKb / 1024;
                                                     if (inMb > 1024) {
                                                         value = (inMb / 1024) + " Gb";
                                                     } else {
                                                         value = inMb + " Mb";
                                                     }
                                                 }
                                                 dv.setAttribute(name, value);
                                             }

                                             pie.getData().setLabels(labels.toArray(new String[0]));

                                             dataset.setData(values);
                                             pie.getData().setDatasets(dataset);

                                             pie.setWidth("50%");
                                             pie.getOptions().getLegend().setDisplay(false);
                                             final long ttotal = total;

                                             MBeanChart.setTooltipItemHandler(pie, new Function<String, String>() {
                                                 @Override
                                                 public String apply(String s) {
                                                     int indexOfSpace = s.lastIndexOf(" ");
                                                     String firstHalf = s.substring(0, indexOfSpace);
                                                     String secondHalf = s.substring(indexOfSpace + 1);

                                                     if (secondHalf.contains(".")) {
                                                         secondHalf = secondHalf.substring(0, secondHalf.indexOf("."));
                                                     }

                                                     long valueInKb = Long.parseLong(secondHalf);

                                                     long percentage = (long) (((double) valueInKb / ttotal) * 100);

                                                     secondHalf = DiskPieChart.putCommasEveryThreeDigits(secondHalf);

                                                     return firstHalf + " " + secondHalf + " Kb (" + percentage + "%)";
                                                 }
                                             });

                                             pie.update();

                                             diskLayout.addMember(details);
                                             diskLayout.addMember(pie);
                                             addMember(diskLayout);
                                         }

                                     }
                                 }

                                 public void onFailure(Throwable caught) {
                                     if (JSONUtils.getJsonErrorCode(caught) == 401) {
                                         LogModel.getInstance()
                                                 .logMessage("You have been disconnected from the server.");
                                     }
                                 }
                             });
    }
}
