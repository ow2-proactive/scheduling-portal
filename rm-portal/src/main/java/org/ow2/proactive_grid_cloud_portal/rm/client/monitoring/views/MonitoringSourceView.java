/*
 * ################################################################
 *
 * ProActive Parallel Suite(TM): The Java(TM) library for
 *    Parallel, Distributed, Multi-Core Computing for
 *    Enterprise Grids & Clouds
 *
 * Copyright (C) 1997-2015 INRIA/University of
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
        status.setContents(JSONUtils.getJsonErrorMessage(caught));
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
    }
}
