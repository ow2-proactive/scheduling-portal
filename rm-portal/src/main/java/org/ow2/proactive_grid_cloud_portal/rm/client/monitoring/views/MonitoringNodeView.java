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

import com.google.gwt.user.client.Timer;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.tab.Tab;
import com.smartgwt.client.widgets.tab.TabSet;
import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSource.Host.Node;
import org.ow2.proactive_grid_cloud_portal.rm.client.NodeState;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMController;
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.charts.ClassesAreaChart;
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.charts.CpuUsageAreaChart;
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.charts.JVMMemoryAreaChart;
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.charts.MBeanChart;
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.charts.MBeanDetailedView;
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.charts.ThreadsAreaChart;
import org.ow2.proactive_grid_cloud_portal.rm.shared.RMConfig;

import java.util.Arrays;


/**
 * Node monitoring view.
 */
public class MonitoringNodeView extends VLayout implements AsyncCallback<String> {

    private Timer updater = null;
    private ReloadableChain chain;
    private TabSet tabs;
    private Label status;

    private RMController controller;

    public MonitoringNodeView(RMController controller) {
        this.controller = controller;
    }

    public void init(Node node) {

        if (node.getNodeState() == NodeState.BUSY || node.getNodeState() == NodeState.FREE ||
            node.getNodeState() == NodeState.LOCKED) {
            // good
        } else {
            // cannot monitor the node
            return;
        }

        setWidth100();
        String nodeUrl = node.getDefaultJMXUrl();
        if (!RMConfig.MONITORING_PROTOCOL_DEFAULT.equals(RMConfig.get().getMonitoringProtocol())) {
            nodeUrl = node.getProactiveJMXUrl();
        }

        final MBeanChart heapMemory = new JVMMemoryAreaChart(controller, nodeUrl);
        final MBeanChart threads = new ThreadsAreaChart(controller, nodeUrl);
        final MBeanChart classes = new ClassesAreaChart(controller, nodeUrl);
        final MBeanChart cpuUsage = new CpuUsageAreaChart(controller, nodeUrl);

        String[] jvmAttrs = { "ManagementSpecVersion", "Name", "SpecName", "SpecVendor", "StartTime",
                "Uptime", "VmName", "VmVendor", "VmVersion", "BootClassPath", "ClassPath", "LibraryPath" };

        MBeanDetailedView jvmDetails = new MBeanDetailedView(this);
        jvmDetails.load(controller, nodeUrl, "java.lang:type=Runtime", Arrays.asList(jvmAttrs));
        jvmDetails.setWidth100();

        chain = new ReloadableChain(heapMemory, threads, classes, cpuUsage);

        HLayout firstRow = new HLayout();
        HLayout secondRow = new HLayout();

        firstRow.addMember(heapMemory);
        firstRow.addMember(threads);
        secondRow.addMember(classes);
        secondRow.addMember(cpuUsage);

        Layout graphs = new VLayout();
        graphs.addMember(firstRow);
        graphs.addMember(secondRow);

        if (status != null) {
            removeMember(status);
        }

        status = new Label("Retreiving data");
        status.setWidth100();
        status.setAlign(Alignment.CENTER);

        Tab t1 = new Tab("Overview");
        t1.setPane(graphs);
        Tab t2 = new Tab("JVM Summary");
        t2.setPane(jvmDetails);

        tabs = new TabSet();
        tabs.setWidth100();
        tabs.setHeight100();
        tabs.setShowResizeBar(true);
        tabs.setTabs(t1, t2);
        tabs.hide();

        addMember(status);

        updater = new Timer() {
            @Override
            public void run() {
                chain.reload();
            }
        };
        updater.scheduleRepeating(RMConfig.get().getMonitoringPeriod());
        // Get two values right now to quickly display the charts
        chain.onFinish(new Runnable() {
            @Override
            public void run() {
                chain.onFinish(null);
                chain.reload();
            }
        });
        chain.reload();
    }

    public void close() {
        try {
            if (updater != null) {
                updater.cancel();
                updater = null;
            }

            if (tabs != null) {
                removeMember(tabs);
                chain.stopReloading();
                tabs.destroy();
                tabs = null;
            }
        } catch (Exception e) {
            // ignore it
        }
    }

    @Override
    public void onFailure(Throwable caught) {
        close();
        status.setContents(RMController.getJsonErrorMessage(caught));
    }

    @Override
    public void onSuccess(String result) {
        removeMember(status);
        status.destroy();
        if (tabs != null) {
            addMember(tabs);
            tabs.show();
        }
    }
}
