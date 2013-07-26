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
package org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.views;

import org.ow2.proactive_grid_cloud_portal.rm.client.RMController;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMModel;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMServiceAsync;
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.Reloadable;
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.charts.MBeanDetailedView;
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.charts.NetworkDetailedAreaChart;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

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

        // loading runtime info
        rm.getNodeMBeansInfo(model.getSessionId(), url, "sigar:Type=NetInterface,Name=*", interfacesAttrs,
                new AsyncCallback<String>() {
                    public void onSuccess(String result) {
                        if (!model.isLoggedIn())
                            return;

                        model
                                .logMessage("Fetched Runtime info in " + (System.currentTimeMillis() - t) +
                                    "ms");

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
                                    String name = properties.get(i).isObject().get("name").isString()
                                            .stringValue();
                                    String value = properties.get(i).isObject().get("value").toString();
                                    dv.setAttribute(name, value);
                                }
                                details.setData(new DetailViewerRecord[] { dv });
                                details.setWidth("45%");

                                HLayout pane = new HLayout();
                                pane.addMember(details);
                                NetworkDetailedAreaChart chart = new NetworkDetailedAreaChart(controller,
                                    url, network);
                                chart.setWidth("45%");
                                charts.add(chart);
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
                        if (RMController.getJsonErrorCode(caught) == 401) {
                            model.logMessage("You have been disconnected from the server.");
                        }
                    }
                });

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
