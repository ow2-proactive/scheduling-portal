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
package org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.views.compact;

import java.util.List;
import java.util.stream.Collectors;

import org.ow2.proactive_grid_cloud_portal.common.client.model.LogModel;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LoginModel;
import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSource;
import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSource.Host;
import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSource.Host.Node;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMController;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMEventDispatcher;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMListeners.NodeSelectedListener;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMListeners.NodesListener;

import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.WidgetCanvas;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;


/**
 * Display all current nodes as a single small icon per node
 * so that the state of a large infrastructure can be monitored easily.
 *
 * @author mschnoor
 *
 */
public class CompactView implements NodesListener, NodeSelectedListener {

    private static Layout globalHover = null;

    private RMController controller;

    private Layout root;

    private boolean _borderSwitch;

    private boolean _doNotScroll;

    /* displays nodes as a compact grid */
    private CompactFlowPanel compactPanel;

    /* canvas for compactPanel (used to remove compactPanel from Layout) */
    private WidgetCanvas campactPanelCanvas;

    /* displays only nodes used by current user as a compact grid */
    private CompactFlowPanelOwn myNodesCompactPanel;

    /* canvas for myNodesCompactPanel (used to remove myNodesCompactPanel from Layout) */
    private WidgetCanvas myNodesCompactPanelCanvas;

    public CompactView(RMController controller) {
        this.controller = controller;
        RMEventDispatcher eventDispatcher = controller.getEventDispatcher();
        eventDispatcher.addNodesListener(this);
        eventDispatcher.addNodeSelectedListener(this);
    }

    public Canvas build() {
        this.root = new HLayout();
        root.setWidth100();
        root.setHeight100();
        root.setOverflow(Overflow.AUTO);
        return root;
    }

    /**
     * Switches which canvas should be displayed in root
     * @param value true to show only canvas with my nodes, false to show all nodes
     */
    public void setViewMyNodes(boolean value) {
        if (value) {
            root.removeMember(campactPanelCanvas);
            root.addMember(myNodesCompactPanelCanvas);
        } else {
            root.removeMember(myNodesCompactPanelCanvas);
            root.addMember(campactPanelCanvas);
        }

    }

    @Override
    public void nodeUnselected() {
        if (compactPanel.getCurrentSelectedTile() != null) {
            compactPanel.getCurrentSelectedTile().setSelectedTile(false);
            compactPanel.setCurrentSelectedTile(null);
        }
        if (myNodesCompactPanel.getCurrentSelectedTile() != null) {
            myNodesCompactPanel.getCurrentSelectedTile().setSelectedTile(false);
            myNodesCompactPanel.setCurrentSelectedTile(null);
        }

    }

    @Override
    public void nodeSourceSelected(NodeSource ns) {
        compactPanel.indexOf(ns).ifPresent(integer -> changeSelection(compactPanel, integer));
        myNodesCompactPanel.indexOf(ns).ifPresent(integer -> changeSelection(myNodesCompactPanel, integer));
    }

    @Override
    public void hostSelected(Host h) {
        compactPanel.indexOf(h).ifPresent(integer -> changeSelection(compactPanel, integer));
        myNodesCompactPanel.indexOf(h).ifPresent(integer -> changeSelection(myNodesCompactPanel, integer));
    }

    @Override
    public void nodeSelected(Node node) {
        compactPanel.indexOf(node).ifPresent(integer -> changeSelection(compactPanel, integer));
        myNodesCompactPanel.indexOf(node).ifPresent(integer -> changeSelection(myNodesCompactPanel, integer));
    }

    private void changeSelection(CompactFlowPanel flow, int id) {
        if (id < 0) {
            return;
        }

        Tile tile = (Tile) flow.getWidget(id);

        if (flow.getCurrentSelectedTile() != null) {
            flow.getCurrentSelectedTile().setSelectedTile(false);
        }
        tile.setSelectedTile(true);
        flow.setCurrentSelectedTile(tile);

        // attempt to scroll at the right position
        if (!_doNotScroll) {
            int numPerLine = flow.getElement().getClientWidth() / 20;
            if (numPerLine > 0) {
                int height = 20 * (id / numPerLine);
                root.scrollTo(0, height);
            }
        }
        _doNotScroll = false;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void updateByDelta(List<NodeSource> nodeSources, List<Node> nodes) {
        LogModel.getInstance().logMessage("CompactView updateByDelta");
        /* first call : create the components */
        if (this.compactPanel == null) {
            initializePanel();
        }
        updateCompactPanel(nodeSources, nodes);

        updateMyNodesCompactPanel(nodeSources, nodes);
    }

    private void updateCompactPanel(List<NodeSource> nodeSources, List<Node> nodes) {
        processNodeSources(compactPanel, nodeSources);
        processNodes(compactPanel, nodes);
    }

    private void updateMyNodesCompactPanel(List<NodeSource> nodeSources, List<Node> nodes) {
        processNodeSources(myNodesCompactPanel, nodeSources);

        String username = LoginModel.getInstance().getLogin();
        processNodes(myNodesCompactPanel,
                     nodes.stream().filter(node -> usedBy(node, username)).collect(Collectors.toList()));
        removeNodesWhichIsNotUsedAnymore(nodes, username);

    }

    /**
     * if myNodesCompactPanel already have some nodes, but they do not belong to us anymore, then delete them
     * @param nodes delta of nodes
     * @param username current username
     */
    private void removeNodesWhichIsNotUsedAnymore(List<Node> nodes, String username) {
        nodes.stream().filter(node -> !usedBy(node, username)).forEach(node -> myNodesCompactPanel.remove(node));
    }

    private boolean usedBy(Node n, String username) {
        return username != null && username.equals(n.getNodeOwner());
    }

    private void processNodeSources(CompactFlowPanel flow, List<NodeSource> nodeSources) {
        for (NodeSource nodeSource : nodeSources) {
            if (nodeSource.isRemoved()) {
                removeNodeSource(flow, nodeSource);
            } else {
                addNodeSourceIfNotExists(flow, nodeSource);
                changeNodeSourceStatusIfChanged(flow, nodeSource);
            }
        }
    }

    private void processNodes(CompactFlowPanel flow, List<Node> nodes) {
        for (Node node : nodes) {
            if (node.isRemoved()) {
                removeNode(flow, node);
            } else {
                addNodeIfNotExists(flow, node);
                changeNodeStatusIfChanged(flow, node);
            }
        }
    }

    private void removeNode(CompactFlowPanel flow, Node node) {
        if (flow.isNodeSourceDrawn(node.getSourceName())) {
            flow.remove(node);
        }
    }

    private void changeNodeStatusIfChanged(CompactFlowPanel flow, Node node) {
        if (node.isChanged()) {
            flow.redrawNode(node);
        }
    }

    private void addNodeIfNotExists(CompactFlowPanel flow, Node node) {
        if (!flow.isNodeDrawn(node)) {
            Tile nodeTile = new Tile(this, flow, node);
            Tile hostTile = new Tile(this, flow, new Host(node.getHostName(), node.getSourceName()));
            if (node.isVirtual()) {
                hostTile.getHost().setVirtual(true);
            }
            flow.drawNode(nodeTile, hostTile);
        }
    }

    private void changeNodeSourceStatusIfChanged(CompactFlowPanel flow, NodeSource nodeSource) {
        if (nodeSource.isChanged()) {
            flow.redrawNodeSource(nodeSource);
        }
    }

    private void addNodeSourceIfNotExists(CompactFlowPanel flow, NodeSource nodeSource) {
        if (!flow.isNodeSourceDrawn(nodeSource.getSourceName())) {
            Tile nsTile = new Tile(this, flow, nodeSource);
            flow.drawNodeSource(nsTile);
        }
    }

    private void removeNodeSource(CompactFlowPanel flow, NodeSource nodeSource) {
        if (flow.isNodeSourceDrawn(nodeSource.getSourceName())) {
            flow.remove(nodeSource);
        }
    }

    private void initializePanel() {
        compactPanel = new CompactFlowPanel();
        myNodesCompactPanel = new CompactFlowPanelOwn();

        campactPanelCanvas = new WidgetCanvas(compactPanel);
        myNodesCompactPanelCanvas = new WidgetCanvas(myNodesCompactPanel);

        root.addMember(campactPanelCanvas);

        addResizeHandler(compactPanel);

        addResizeHandler(myNodesCompactPanel);
    }

    private void addResizeHandler(CompactFlowPanel panel) {
        root.addResizedHandler(event -> {
            panel.setPixelSize(root.getWidth() - root.getScrollbarSize(), root.getHeight() - root.getScrollbarSize());
            // lazy hack to force this.compactPanel to -really- relayout
            root.setBorder(_borderSwitch ? "1px solid white" : "0px");
            _borderSwitch = !_borderSwitch;
        });
    }

    public boolean isDoNotScroll() {
        return _doNotScroll;
    }

    public void setDoNotScroll(boolean _doNotScroll) {
        this._doNotScroll = _doNotScroll;
    }

    public static Layout getGlobalHover() {
        return globalHover;
    }

    public static void setGlobalHover(Layout globalHover) {
        CompactView.globalHover = globalHover;
    }

    public RMController getController() {
        return controller;
    }
}
