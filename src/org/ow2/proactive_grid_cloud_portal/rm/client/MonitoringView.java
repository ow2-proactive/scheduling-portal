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
package org.ow2.proactive_grid_cloud_portal.rm.client;

import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSource.Host;
import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSource.Host.Node;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMListeners.NodeSelectedListener;
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.views.MonitoringHostView;
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.views.MonitoringNodeView;

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
public class MonitoringView implements NodeSelectedListener {

    private Label label = null;

    private MonitoringNodeView nodeMonitoring = null;
    private MonitoringHostView hostMonitoring = null;
    private VLayout nodeCanvas = null;
    private Label nodeLabel = null;

    private Layout hostCanvas = null;
    private Label hostLabel = null;

    private RMController controller;

    MonitoringView(RMController controller) {
        this.controller = controller;
        controller.getEventDispatcher().addNodeSelectedListener(this);
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

        vl.setMembers(label, nodeCanvas, hostCanvas);
        return vl;
    }

    /*
     * (non-Javadoc)
     * @see org.ow2.proactive_grid_cloud_portal.rm.client.Listeners.NodeSelectedListener#nodeUnselected()
     */
    public void nodeUnselected() {
        this.label.setContents("No node selected");
        this.label.setAlign(Alignment.CENTER);
        this.label.show();
        this.nodeCanvas.hide();
        this.hostCanvas.hide();
        this.nodeMonitoring.close();
        this.hostMonitoring.close();
    }

    /*
     * (non-Javadoc)
     * @see org.ow2.proactive_grid_cloud_portal.rm.client.Listeners.NodeSelectedListener#nodeSelected(org.ow2.proactive_grid_cloud_portal.rm.client.NodeSource.Host.Node)
     */
    public void nodeSelected(Node node) {

        this.nodeLabel.setIcon(node.getNodeState().getIcon());

        this.label.hide();
        this.hostCanvas.hide();

        this.nodeLabel.setContents("<h3>" + node.getNodeUrl() + "</h3>");
        this.hostMonitoring.close();
        this.nodeMonitoring.close();
        this.nodeMonitoring.init(node);
        this.nodeCanvas.show();
    }

    /*
     * (non-Javadoc)
     * @see org.ow2.proactive_grid_cloud_portal.rm.client.Listeners.NodeSelectedListener#nodeSourceSelected(org.ow2.proactive_grid_cloud_portal.rm.client.NodeSource)
     */
    public void nodeSourceSelected(NodeSource ns) {
        nodeUnselected();
    }

    /*
     * (non-Javadoc)
     * @see org.ow2.proactive_grid_cloud_portal.rm.client.Listeners.NodeSelectedListener#hostSelected(org.ow2.proactive_grid_cloud_portal.rm.client.NodeSource.Host)
     */
    public void hostSelected(Host h) {
        this.label.hide();
        this.nodeCanvas.hide();

        this.hostLabel.setContents("<h3>" + h.getHostName() + "</h3>");
        this.nodeMonitoring.close();
        this.hostMonitoring.close();
        this.hostMonitoring.init(h);
        this.hostCanvas.show();
    }
}
