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
package org.ow2.proactive_grid_cloud_portal.rm.client;

import static org.ow2.proactive_grid_cloud_portal.rm.client.RMListeners.*;

import java.util.Map;

import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSource.Host;
import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSource.Host.Node;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMListeners.NodeSelectedListener;
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.NodeLabel;
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.views.MonitoringHostView;
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.views.MonitoringNodeView;
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.views.MonitoringSourceView;

import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;


/**
 * Displays a monitoring information about the currently selected node or host
 *
 */
public class MonitoringView implements NodesListener, NodeSelectedListener {

    private Label label = null;

    private MonitoringNodeView nodeMonitoring = null;

    private MonitoringHostView hostMonitoring = null;

    private MonitoringSourceView sourceMonitoring = null;

    private VLayout nodeCanvas = null;

    private Label nodeLabel = null;

    private Layout hostCanvas = null;

    private Label hostLabel = null;

    private Layout sourceCanvas = null;

    private Label sourceLabel = null;

    private RMController controller;

    private Node selectedNode;

    MonitoringView(RMController controller) {
        this.controller = controller;

        RMEventDispatcher eventDispatcher = controller.getEventDispatcher();
        eventDispatcher.addNodesListener(this);
        eventDispatcher.addNodeSelectedListener(this);
    }

    Canvas build() {
        VLayout vl = new VLayout();
        vl.setOverflow(Overflow.AUTO);

        this.label = new Label("No node selected");
        this.label.setWidth100();
        this.label.setAlign(Alignment.CENTER);

        this.nodeMonitoring = new MonitoringNodeView(controller);
        this.nodeMonitoring.setOverflow(Overflow.AUTO);
        this.nodeMonitoring.setWidth100();

        this.nodeCanvas = new VLayout();
        this.nodeCanvas.setWidth100();
        this.nodeCanvas.setHeight100();
        this.nodeLabel = new Label();
        this.nodeLabel.setIcon(RMImages.instance.node_add_16().getSafeUri().asString());
        this.nodeLabel.setHeight(16);
        this.nodeCanvas.addMember(nodeLabel);
        this.nodeCanvas.addMember(nodeMonitoring);
        this.nodeCanvas.hide();

        this.sourceMonitoring = new MonitoringSourceView(controller);
        this.sourceMonitoring.setOverflow(Overflow.AUTO);
        this.sourceMonitoring.setWidth100();

        this.sourceCanvas = new VLayout();
        this.sourceCanvas.setWidth100();
        this.sourceCanvas.setHeight100();
        this.sourceLabel = new Label();
        this.sourceLabel.setIcon(RMImages.instance.nodesource_16().getSafeUri().asString());
        this.sourceLabel.setHeight(16);
        this.sourceCanvas.addMember(sourceLabel);
        this.sourceCanvas.addMember(sourceMonitoring);
        this.sourceCanvas.hide();

        this.hostMonitoring = new MonitoringHostView(controller);
        this.hostMonitoring.setOverflow(Overflow.AUTO);
        this.hostMonitoring.setWidth100();

        this.hostCanvas = new VLayout();
        this.hostCanvas.setWidth100();
        this.hostCanvas.setHeight100();
        this.hostLabel = new Label();
        this.hostLabel.setHeight(16);
        this.hostLabel.setIcon(RMImages.instance.host_16().getSafeUri().asString());
        this.hostCanvas.addMember(hostLabel);
        this.hostCanvas.addMember(hostMonitoring);
        this.hostCanvas.hide();

        vl.setMembers(label, nodeCanvas, hostCanvas, sourceCanvas);
        return vl;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.ow2.proactive_grid_cloud_portal.rm.client.Listeners.NodeSelectedListener#nodeUnselected()
     */
    public void nodeUnselected() {
        this.selectedNode = null;
        this.label.setContents("No node selected");
        this.label.setAlign(Alignment.CENTER);
        this.label.show();
        this.nodeCanvas.hide();
        this.hostCanvas.hide();
        this.sourceCanvas.hide();
        this.nodeMonitoring.close();
        this.hostMonitoring.close();
        this.sourceMonitoring.close();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.ow2.proactive_grid_cloud_portal.rm.client.Listeners.NodeSelectedListener#nodeSelected(org
     * .ow2.proactive_grid_cloud_portal.rm.client.NodeSource.Host.Node)
     */
    public void nodeSelected(Node node) {
        this.selectedNode = node;
        this.nodeLabel.setIcon(node.getIcon());

        this.label.hide();
        this.hostCanvas.hide();
        this.sourceCanvas.hide();

        this.nodeLabel.setContents("<h3>" + node.getNodeUrl() + "</h3>");
        this.hostMonitoring.close();
        this.sourceMonitoring.close();
        this.nodeMonitoring.close();
        this.nodeMonitoring.init(node);
        this.nodeCanvas.show();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.ow2.proactive_grid_cloud_portal.rm.client.Listeners.NodeSelectedListener#
     * nodeSourceSelected(org.ow2.proactive_grid_cloud_portal.rm.client.NodeSource)
     */
    public void nodeSourceSelected(NodeSource ns) {
        this.selectedNode = null;
        this.label.hide();
        this.nodeCanvas.hide();
        this.hostCanvas.hide();

        this.sourceLabel.setContents("<h3>" + ns.getSourceName() + "</h3>");
        this.nodeMonitoring.close();
        this.hostMonitoring.close();
        this.sourceMonitoring.close();
        this.sourceMonitoring.init(ns);
        this.sourceCanvas.show();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.ow2.proactive_grid_cloud_portal.rm.client.Listeners.NodeSelectedListener#hostSelected(org
     * .ow2.proactive_grid_cloud_portal.rm.client.NodeSource.Host)
     */
    public void hostSelected(Host h) {
        this.selectedNode = null;
        this.label.hide();
        this.nodeCanvas.hide();
        this.sourceCanvas.hide();

        this.hostLabel.setContents("<h3>" + h.getHostName() + "</h3>");
        this.nodeMonitoring.close();
        this.sourceMonitoring.close();
        this.hostMonitoring.close();
        this.hostMonitoring.init(h);
        this.hostCanvas.show();
    }

    @Override
    public void nodesUpdated(Map<String, NodeSource> nodes) {
        NodeLabel.update(nodes, nodeLabel, selectedNode);
    }

}
