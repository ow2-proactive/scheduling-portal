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

import java.util.LinkedList;
import java.util.List;

import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSource.Host;
import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSource.Host.Node;
import org.ow2.proactive_grid_cloud_portal.rm.client.NodeState;
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

/**
 * Host monitoring view.
 */
public class MonitoringHostView extends VLayout implements AsyncCallback<String> {
	
	private Timer updater = null;
	private List<Reloadable> reloadables = new LinkedList<Reloadable>();
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
		for (Node n: host.getNodes().values()) {
			if (n.getNodeState() == NodeState.BUSY || 
				n.getNodeState() == NodeState.FREE || 
				n.getNodeState() == NodeState.LOCKED) {
				node = n;
				break;
			}
		}

		if (node == null) {
			return;
		}
		
		String hostMonitoringUrl = node.getDefaultJMXUrl();
		if (!RMConfig.MONITORING_PERIOD_DEFAULT.equals(RMConfig.get().getMonitoringProtocol())) {
			hostMonitoringUrl = node.getProactiveJMXUrl();
		}
		
		setWidth100();

		overview = new Overview(controller, hostMonitoringUrl, this);
		reloadables.add(overview);
		
		cpuView = new CpuView(controller, hostMonitoringUrl);
		reloadables.add(cpuView);
		
		memoryView = new MemoryView(controller, hostMonitoringUrl);
		reloadables.add(memoryView);

		fsView = new FileSystemView(controller, hostMonitoringUrl);
		
		networkView = new NetworkView(controller, hostMonitoringUrl);
		reloadables.add(networkView);
		
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
		refresh.setTooltip("Refresh processes list");
		
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
				for (Reloadable reloadable: reloadables) {
					reloadable.reload();
				}					
			}
		};
		updater.schedule(1);
		updater.scheduleRepeating(RMConfig.get().getMonitoringPeriod());			
	}
		
	public void close() {
		try {
			if (updater != null) {
				updater.cancel();
				updater = null;
			}
			
			if (tabs != null) {
				removeMember(tabs);
				reloadables.clear();
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
		addMember(tabs);
		tabs.show();
	}
}
