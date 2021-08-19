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
import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSource;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMController;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMImages;
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.Reloadable;
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
 * Source monitoring view.
 */
public class MonitoringSourceView extends VLayout implements AsyncCallback<String> {

    public static final String MBEAN_NAME_PREFIX = "ProActiveResourceManager:name=IaasMonitoring";

    public static final String NO_MONITORING_INFO_EXCEPTION_STRING = "javax.management.InstanceNotFoundException";

    public static final String ACCESS_DENIED_EXCEPTION_STRING = "javax.management.MBeanPermission";

    private Timer updater = null;

    private ReloadableChain chain;

    private SourceOverview sourceOverview;

    private SourceHostsView sourceHosts;

    private TabSet tabs;

    private Label status;

    private RMController controller;

    public MonitoringSourceView(RMController controller) {
        this.controller = controller;
    }

    public void init(NodeSource ns) {

        // Set up JMX url to contact the RM
        RMConfig config = RMConfig.get();
        String rmJmxPrefix = config.getRMJmxPrefix();
        int port = config.getRMJmxPort();
        String hostname = config.getRMJmxHostname();
        String connectorServerName = config.getRMJmxServerName();

        String rmJmxUrl = rmJmxPrefix + hostname + ":" + port + "/" + connectorServerName;

        setWidth100();

        // Set up tabs
        sourceOverview = new SourceOverview(controller, rmJmxUrl, ns.getSourceName(), this);
        sourceHosts = new SourceHostsView(controller, rmJmxUrl, ns.getSourceName(), this);

        if (status != null) {
            removeMember(status);
        }

        status = new Label("Retreiving data");
        status.setWidth100();
        status.setAlign(Alignment.CENTER);

        chain = new ReloadableChain(sourceOverview, sourceHosts);

        final Tab overviewTab = new Tab("Overview");
        overviewTab.setPane(sourceOverview);
        final Tab detailsTab = new Tab("Hosts & VMs");
        detailsTab.setPane(sourceHosts);

        tabs = new TabSet();
        tabs.setWidth100();
        tabs.setShowResizeBar(true);

        // Refresh button 
        final IButton refresh = new IButton();
        refresh.setIcon(RMImages.instance.refresh().getSafeUri().asString());
        refresh.setIconAlign("center");
        refresh.setWidth(25);
        refresh.setTooltip("Refresh");
        refresh.hide();

        tabs.addTabSelectedHandler(new TabSelectedHandler() {
            public void onTabSelected(TabSelectedEvent event) {
                if (event.getTab() == overviewTab) {
                    refresh.show();
                } else if (event.getTab() == detailsTab) {
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
                Object pane = tabs.getSelectedTab().getPane();
                Reloadable reloadable = (Reloadable) pane;
                reloadable.reload();
            }
        });

        // All tabs must also implement Reloadable.
        tabs.setTabs(overviewTab, detailsTab);
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
        MonitoringNodeView.previousSelectedNode = null;
    }

    public void close() {
        try {
            if (updater != null) {
                updater.cancel();
                updater = null;
            }

            if (tabs != null) {
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
        String errorMessage = JSONUtils.getJsonErrorMessage(caught);
        if (errorMessage.contains("HTTP 500")) {
            errorMessage = "Monitoring is not available on this node source";
        }
        status.setContents(errorMessage);
        controller.getRmPage().setMonitoringTabPageDisabled(true);
    }

    @Override
    public void onSuccess(String result) {
        if (status != null) {
            removeMember(status);
            status = null;
        }
        if (tabs != null) {
            addMember(tabs);
            tabs.show();
        }
        controller.getRmPage().setMonitoringTabPageDisabled(false);
    }
}
