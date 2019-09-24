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

import java.util.Map;

import org.ow2.proactive_grid_cloud_portal.common.client.JSUtil;
import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSource.Host;
import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSource.Host.Node;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMListeners.NodeSelectedListener;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMListeners.NodesListener;

import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.viewer.DetailViewer;
import com.smartgwt.client.widgets.viewer.DetailViewerField;
import com.smartgwt.client.widgets.viewer.DetailViewerRecord;


/**
 * Displays detailed info about the currently selected node
 *
 * @author mschnoor
 *
 */
public class InfoView implements NodeSelectedListener, NodesListener {

    private Label label = null;

    private DetailViewer nodeDetails = null;

    private Layout nodeCanvas = null;

    private Label nodeLabel = null;

    private DetailViewer nsDetails = null;

    private Layout nsCanvas = null;

    private Label nodeSourceLabel;

    private DetailViewer hostDetails = null;

    private Layout hostCanvas = null;

    private Label hostLabel = null;

    private Node selNode = null;

    private Host selHost = null;

    private NodeSource selNS = null;

    InfoView(RMController controller) {
        controller.getEventDispatcher().addNodeSelectedListener(this);
        controller.getEventDispatcher().addNodesListener(this);
    }

    Canvas build() {
        VLayout vl = new VLayout();
        vl.setOverflow(Overflow.AUTO);

        this.label = new Label("No node selected");
        this.label.setWidth100();
        this.label.setAlign(Alignment.CENTER);

        this.nodeDetails = new DetailViewer();
        this.nodeDetails.setWidth100();
        this.nodeDetails.setHeight100();
        this.nodeDetails.setCanSelectText(true);
        DetailViewerField d1 = new DetailViewerField("nodeUrl", "URL");
        DetailViewerField d2 = new DetailViewerField("nodeState", "Status");
        DetailViewerField d3 = new DetailViewerField("nodeProvider", "Provider");
        DetailViewerField d4 = new DetailViewerField("nodeOwner", "Used by");
        DetailViewerField d5 = new DetailViewerField("sourceName", "Source");
        DetailViewerField d6 = new DetailViewerField("hostName", "Host");
        DetailViewerField d7 = new DetailViewerField("vmName", "JVM");
        DetailViewerField d8 = new DetailViewerField("description", "Description");
        DetailViewerField d9 = new DetailViewerField("usage", "Usage");
        DetailViewerField d10 = new DetailViewerField("access", "Usage restrictions");
        DetailViewerField d11 = new DetailViewerField("tokens", "Tokens");
        this.nodeDetails.setFields(d1, d5, d6, d2, d3, d4, d7, d8, d9, d10, d11);
        this.nodeCanvas = new VLayout();
        this.nodeCanvas.setWidth100();
        this.nodeLabel = new Label("<h3>Node</h3>");
        this.nodeLabel.setIcon(RMImages.instance.node_add_16().getSafeUri().asString());
        this.nodeLabel.setHeight(16);
        this.nodeCanvas.addMember(this.nodeLabel);
        this.nodeCanvas.addMember(this.nodeDetails);
        this.nodeCanvas.hide();

        this.nsDetails = new DetailViewer();
        this.nsDetails.setWidth100();
        this.nsDetails.setHeight100();
        this.nsDetails.setCanSelectText(true);
        DetailViewerField n1 = new DetailViewerField("sourceName", "Node Source");
        DetailViewerField n2 = new DetailViewerField("description", "Description");
        DetailViewerField n3 = new DetailViewerField("nodeProvider", "Owner");
        DetailViewerField n4 = new DetailViewerField("hosts", "Hosts");
        DetailViewerField n5 = new DetailViewerField("nodes", "Nodes");
        this.nsDetails.setFields(n1, n2, n3, n4, n5);
        this.nsCanvas = new VLayout();
        this.nsCanvas.setWidth100();
        this.nsCanvas.setHeight100();
        this.nodeSourceLabel = new Label("<h3>Node Source</h3>");
        this.nodeSourceLabel.setHeight(16);
        this.nodeSourceLabel.setIcon(RMImages.instance.nodesource_deployed().getSafeUri().asString());
        this.nsCanvas.addMember(this.nodeSourceLabel);
        this.nsCanvas.addMember(this.nsDetails);
        this.nsCanvas.hide();

        this.hostDetails = new DetailViewer();
        this.hostDetails.setWidth100();
        this.hostDetails.setHeight100();
        this.hostDetails.setCanSelectText(true);
        DetailViewerField h1 = new DetailViewerField("hostName", "Host");
        DetailViewerField h2 = new DetailViewerField("nodes", "Nodes");
        this.hostDetails.setFields(h1, h2);
        this.hostCanvas = new VLayout();
        this.hostCanvas.setWidth100();
        this.hostLabel = new Label("<h3>Host</h3>");
        hostLabel.setHeight(16);
        hostLabel.setIcon(RMImages.instance.host_16().getSafeUri().asString());
        this.hostCanvas.addMember(hostLabel);
        this.hostCanvas.addMember(this.hostDetails);
        this.hostCanvas.hide();

        vl.setMembers(label, nodeCanvas, nsCanvas, hostCanvas);
        return vl;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.ow2.proactive_grid_cloud_portal.rm.client.Listeners.NodeSelectedListener#nodeUnselected()
     */
    public void nodeUnselected() {
        this.label.setContents("No node selected");
        this.label.setAlign(Alignment.CENTER);
        this.label.show();
        this.nodeCanvas.hide();
        this.nsCanvas.hide();
        this.hostCanvas.hide();
        this.selNode = null;
    }

    public void nodesUpdated(Map<String, NodeSource> nodes) {
        if (selNode != null) {
            for (NodeSource ns : nodes.values()) {
                if (selNode.getSourceName().equals(ns.getSourceName())) {

                    Node depl = ns.getDeploying().get(selNode.getNodeUrl());
                    if (depl != null) {
                        nodeSelected(depl);
                        return;
                    }

                    for (Host h : ns.getHosts().values()) {
                        if (selNode.getHostName().equals(h.getHostName())) {

                            for (Node n : h.getNodes().values()) {
                                if (n.getNodeUrl().equals(selNode.getNodeUrl())) {
                                    nodeSelected(n);
                                    return;
                                }
                            }

                        }

                    }
                }
            }
        } else if (selHost != null) {
            for (NodeSource ns : nodes.values()) {
                if (ns.getSourceName().equals(selHost.getSourceName())) {

                    for (Host h : ns.getHosts().values()) {
                        if (selHost.getHostName().equals(h.getHostName())) {
                            hostSelected(h);
                            return;
                        }
                    }

                }
            }
        } else if (selNS != null) {
            for (NodeSource ns : nodes.values()) {
                if (ns.getSourceName().equals(selNS.getSourceName())) {
                    nodeSourceSelected(ns);
                    return;
                }
            }
        }

        nodeUnselected();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.ow2.proactive_grid_cloud_portal.rm.client.Listeners.NodeSelectedListener#nodeSelected(org
     * .ow2.proactive_grid_cloud_portal.rm.client.NodeSource.Host.Node)
     */
    public void nodeSelected(Node node) {
        DetailViewerRecord dv = new DetailViewerRecord();

        dv.setAttribute("nodeUrl", node.getNodeUrl());
        dv.setAttribute("nodeState", node.getNodeState().toString() + " since " + JSUtil.getTime(node.getTimeStamp()));
        dv.setAttribute("nodeProvider", node.getNodeProvider());
        dv.setAttribute("nodeOwner", node.getNodeOwner());
        dv.setAttribute("hostName", node.getHostName());
        dv.setAttribute("sourceName", node.getSourceName());
        dv.setAttribute("vmName", node.getVmName());
        dv.setAttribute("description", "<pre>" + node.getDescription() + "</pre>");
        dv.setAttribute("usage", niceUsageString(node.getUsageInfo()));
        dv.setAttribute("access", node.getUserAccessTypeLocal());
        dv.setAttribute("tokens", "<pre>" + String.join("\n", node.getTokens()) + "</pre>");

        this.nodeDetails.setData(new DetailViewerRecord[] { dv });
        this.nodeLabel.setIcon(node.getIcon());

        this.label.hide();
        this.nsCanvas.hide();
        this.hostCanvas.hide();
        this.nodeCanvas.show();

        this.selNode = node;
        this.selHost = null;
        this.selNS = null;
    }

    private String niceUsageString(Map<String, String> usage) {
        if (!usage.isEmpty()) {
            return "<pre>" + "Job id: " + usage.get("JOB_ID") + "\n" + "Job name: " + usage.get("JOB_NAME") + "\n" +
                   "Task id: " + usage.get("TASK_ID") + "\n" + "Task name: " + usage.get("TASK_NAME") + "\n" + "</pre>";
        } else {
            return "<pre>(Free)</pre>";
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.ow2.proactive_grid_cloud_portal.rm.client.Listeners.NodeSelectedListener#
     * nodeSourceSelected(org.ow2.proactive_grid_cloud_portal.rm.client.NodeSource)
     */
    public void nodeSourceSelected(NodeSource ns) {
        DetailViewerRecord dv = new DetailViewerRecord();

        int numNodes = 0;
        for (Host h : ns.getHosts().values())
            numNodes += h.getNodes().size();

        dv.setAttribute("sourceName", ns.getSourceName());
        dv.setAttribute("description", ns.getSourceDescription());
        dv.setAttribute("nodeProvider", ns.getNodeSourceAdmin());
        dv.setAttribute("nodes", numNodes);
        dv.setAttribute("hosts", ns.getHosts().size());

        this.nsDetails.setData(new DetailViewerRecord[] { dv });
        this.nodeSourceLabel.setIcon(ns.getIcon());

        this.label.hide();
        this.nodeCanvas.hide();
        this.hostCanvas.hide();
        this.nsCanvas.show();

        this.selHost = null;
        this.selNS = ns;
        this.selNode = null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.ow2.proactive_grid_cloud_portal.rm.client.Listeners.NodeSelectedListener#hostSelected(org
     * .ow2.proactive_grid_cloud_portal.rm.client.NodeSource.Host)
     */
    public void hostSelected(Host h) {
        DetailViewerRecord dv = new DetailViewerRecord();

        dv.setAttribute("hostName", h.getHostName());
        dv.setAttribute("nodes", h.getNodes().size());

        this.hostDetails.setData(new DetailViewerRecord[] { dv });
        if (h.isVirtual()) {
            this.hostLabel.setIcon(RMImages.instance.host_virtual_16().getSafeUri().asString());
        } else {
            this.hostLabel.setIcon(RMImages.instance.host_16().getSafeUri().asString());
        }

        this.label.hide();
        this.nodeCanvas.hide();
        this.hostCanvas.show();
        this.nsCanvas.hide();

        this.selHost = h;
        this.selNS = null;
        this.selNode = null;
    }
}
