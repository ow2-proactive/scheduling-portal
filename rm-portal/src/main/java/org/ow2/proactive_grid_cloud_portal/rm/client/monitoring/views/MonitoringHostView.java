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

import org.ow2.proactive_grid_cloud_portal.common.client.json.JSONUtils;
import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSource.Host;
import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSource.Host.Node;
import org.ow2.proactive_grid_cloud_portal.rm.client.NodeState;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMController;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMImages;
import org.ow2.proactive_grid_cloud_portal.rm.shared.RMConfig;

import com.google.gwt.user.client.Timer;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.TabBarControls;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.tab.Tab;
import com.smartgwt.client.widgets.tab.TabSet;
import com.smartgwt.client.widgets.tab.events.TabSelectedEvent;
import com.smartgwt.client.widgets.tab.events.TabSelectedHandler;


/**
 * Host monitoring view.
 */
public class MonitoringHostView extends VLayout implements AsyncCallback<String> {

    private Timer updater = null;

    private ReloadableChain chain;

    private Overview overview;

    private CpuView cpuView;

    private MemoryView memoryView;

    private FileSystemView fsView;

    private NetworkView networkView;

    private ProcessesView processesView;

    private Node node;

    private TabSet tabs;

    private Label status;

    private RMController controller;

    public MonitoringHostView(RMController controller) {
        this.controller = controller;
    }

    public void init(Host host) {

        // selecting the node that will be used as an entry point to the host
        for (Node n : host.getNodes().values()) {
            if (n.getNodeState() == NodeState.BUSY || n.getNodeState() == NodeState.FREE ||
                n.getNodeState() == NodeState.LOCKED) {
                node = n;
                break;
            }
        }

        if (node == null) {
            return;
        }

        String hostMonitoringUrl = node.getDefaultJMXUrl();
        if (!RMConfig.MONITORING_PROTOCOL_DEFAULT.equals(RMConfig.get().getMonitoringProtocol())) {
            hostMonitoringUrl = node.getProactiveJMXUrl();
        }

        setWidth100();

        overview = new Overview(controller, hostMonitoringUrl, this);
        cpuView = new CpuView(controller, hostMonitoringUrl);
        memoryView = new MemoryView(controller, hostMonitoringUrl);
        fsView = new FileSystemView(controller, hostMonitoringUrl);
        networkView = new NetworkView(controller, hostMonitoringUrl);

        chain = new ReloadableChain(overview, cpuView, memoryView, networkView);

        // to not add to to automatic reloadable views (use a dedicated button for this)
        processesView = new ProcessesView(controller, hostMonitoringUrl);

        if (status != null) {
            removeMember(status);
        }

        status = new Label("Retreiving data");
        status.setWidth100();
        status.setAlign(Alignment.CENTER);

        Tab t1 = new Tab("Overview");
        t1.setPane(overview);
        Tab t2 = new Tab("CPU");
        t2.setPane(cpuView);
        Tab t3 = new Tab("Memory");
        t3.setPane(memoryView);
        Tab t4 = new Tab("File System");
        t4.setPane(fsView);
        Tab t5 = new Tab("Network");
        t5.setPane(networkView);
        final Tab t6 = new Tab("Processes");
        t6.setPane(processesView);

        tabs = new TabSet();
        tabs.setWidth100();
        tabs.setShowResizeBar(true);

        final IButton refresh = new IButton();
        refresh.setIcon(RMImages.instance.refresh().getSafeUri().asString());
        refresh.setIconAlign("center");
        refresh.setWidth(25);
        refresh.setTooltip("Refresh processes");
        refresh.hide();

        tabs.addTabSelectedHandler(new TabSelectedHandler() {
            public void onTabSelected(TabSelectedEvent event) {
                if (event.getTab() == t6) {
                    refresh.show();
                } else {
                    refresh.hide();
                }
            }
        });

        tabs.setTabBarControls(TabBarControls.TAB_SCROLLER, TabBarControls.TAB_PICKER, refresh);

        refresh.addClickHandler(new ClickHandler() {
            @Override
            public void onClick(ClickEvent event) {
                processesView.reload();
            }
        });

        tabs.setTabs(t1, t2, t3, t4, t5, t6);
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
        status.setContents("Monitoring is unavailable on this host. <br>" +
                           "Either monitoring was disabled with the disableMonitoring option or an error occurred. <br>" +
                           "More information are available inside the node logs.");
    }

    @Override
    public void onSuccess(String result) {
        removeMember(status);
        if (tabs != null) {
            addMember(tabs);
            tabs.show();
        }
    }
}
