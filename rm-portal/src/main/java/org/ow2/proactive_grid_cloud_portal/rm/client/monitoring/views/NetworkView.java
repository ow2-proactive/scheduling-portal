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
import java.util.LinkedList;
import java.util.List;

import org.ow2.proactive_grid_cloud_portal.common.client.Model;
import org.ow2.proactive_grid_cloud_portal.common.client.json.JSONUtils;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LogModel;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LoginModel;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMController;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMModel;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMServiceAsync;
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.Reloadable;
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.charts.MBeanChart;
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.charts.MBeanDetailedView;
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.charts.NetworkDetailedAreaChart;

import com.google.gwt.json.client.JSONArray;
import com.google.gwt.json.client.JSONObject;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.viewer.DetailViewer;
import com.smartgwt.client.widgets.viewer.DetailViewerField;
import com.smartgwt.client.widgets.viewer.DetailViewerRecord;


/**
 * Network tab in host monitoring.
 */
public class NetworkView extends VLayout implements Reloadable {

    private Runnable onFinish;

    private ReloadableChain chain;

    private final List<MBeanChart> networkCharts;

    public NetworkView(final RMController controller, final String url) {
        setWidth100();

        final List<String> attrs = new ArrayList<String>();

        attrs.add("DefaultGateway");
        attrs.add("DomainName");
        attrs.add("FQDN");
        attrs.add("HostName");
        attrs.add("PrimaryDns");
        attrs.add("SecondaryDns");

        Label label = new Label("<nobr style='font-weight:bold;'>Basic configuration<nobr>");
        label.setHeight(50);
        MBeanDetailedView summary = new MBeanDetailedView();
        summary.load(controller, url, "sigar:Type=NetInfo", attrs);
        addMember(label);
        addMember(summary);

        label = new Label("<nobr style='font-weight:bold;'>Interfaces<nobr>");
        label.setHeight(50);
        addMember(label);

        final List<String> interfacesAttrs = new ArrayList<String>();
        networkCharts = new LinkedList<>();

        interfacesAttrs.add("Name");
        interfacesAttrs.add("Address");
        interfacesAttrs.add("Broadcast");
        interfacesAttrs.add("Destination");
        interfacesAttrs.add("Description");
        interfacesAttrs.add("Netmask");
        interfacesAttrs.add("Type");
        interfacesAttrs.add("Hwaddr");
        interfacesAttrs.add("Flags");
        interfacesAttrs.add("Metric");

        final RMServiceAsync rm = controller.getRMService();
        final RMModel model = controller.getModel();
        final long t = System.currentTimeMillis();
        final List<Reloadable> charts = new LinkedList<Reloadable>();

        final LoginModel loginModel = LoginModel.getInstance();

        // loading runtime info
        rm.getNodeMBeansInfo(loginModel.getSessionId(),
                             url,
                             "sigar:Type=NetInterface,Name=*",
                             interfacesAttrs,
                             new AsyncCallback<String>() {
                                 public void onSuccess(String result) {
                                     if (!loginModel.isLoggedIn())
                                         return;

                                     LogModel.getInstance().logMessage("Fetched Runtime info in " +
                                                                       (System.currentTimeMillis() - t) + "ms");

                                     JSONObject object = controller.parseJSON(result).isObject();
                                     if (object != null) {
                                         for (String network : object.keySet()) {
                                             DetailViewer details = new DetailViewer();
                                             DetailViewerRecord dv = new DetailViewerRecord();
                                             DetailViewerField[] fields = new DetailViewerField[interfacesAttrs.size()];
                                             for (int i = 0; i < fields.length; i++) {
                                                 fields[i] = new DetailViewerField(interfacesAttrs.get(i));
                                             }
                                             details.setFields(fields);

                                             JSONArray properties = object.get(network).isArray();

                                             for (int i = 0; i < properties.size(); i++) {
                                                 String name = properties.get(i)
                                                                         .isObject()
                                                                         .get("name")
                                                                         .isString()
                                                                         .stringValue();
                                                 String value = properties.get(i).isObject().get("value").toString();
                                                 dv.setAttribute(name, value);
                                             }
                                             details.setData(new DetailViewerRecord[] { dv });
                                             details.setWidth("45%");

                                             HLayout pane = new HLayout();
                                             pane.addMember(details);
                                             NetworkDetailedAreaChart chart = new NetworkDetailedAreaChart(controller,
                                                                                                           url,
                                                                                                           network);
                                             chart.setWidth("45%");
                                             charts.add(chart);
                                             networkCharts.add(chart);
                                             pane.addMember(chart);
                                             addMember(pane);
                                         }

                                         synchronized (NetworkView.this) {
                                             chain = new ReloadableChain(charts.toArray(new Reloadable[charts.size()]));
                                             if (onFinish != null) {
                                                 chain.onFinish(onFinish);
                                             }
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

    public void selectRange(Model.StatHistory.Range timeRange) {
        networkCharts.forEach(networkChart -> networkChart.selectRange(timeRange));
    }

    @Override
    public void reload() {
        chain.reload();
    }

    @Override
    public synchronized void onFinish(Runnable onFinish) {
        this.onFinish = onFinish;
        if (chain != null) {
            chain.onFinish(onFinish);
        }
    }
}
