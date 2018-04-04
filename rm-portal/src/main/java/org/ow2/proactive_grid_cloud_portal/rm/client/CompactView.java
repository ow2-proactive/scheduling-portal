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

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.ow2.proactive_grid_cloud_portal.common.client.JSUtil;
import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSource.Host;
import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSource.Host.Node;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMListeners.NodeSelectedListener;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMListeners.NodesListener;
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.views.CompactFlowPanel;

import com.google.gwt.dom.client.Style.BorderStyle;
import com.google.gwt.dom.client.Style.Unit;
import com.google.gwt.event.dom.client.ContextMenuEvent;
import com.google.gwt.user.client.DOM;
import com.google.gwt.user.client.Event;
import com.google.gwt.user.client.ui.Image;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;


/**
 * Display all current nodes as a single small icon per node
 * so that the state of a large infrastructure can be monitored easily
 * 
 * 
 * @author mschnoor
 *
 */
public class CompactView implements NodesListener, NodeSelectedListener {

    private RMController controller;

    private Layout root;

    /* displays nodes as a compact grid */
    private CompactFlowPanel flow;

    private boolean _borderSwitch;

    private boolean _doNotScroll;

    private static Layout globalHover = null;

    /*
     * unique names of the nodesources/hosts/nodes currently held by the FlowLayout
     * helps figuring out the position of each element since FlowLayout doesnt allow it
     */
    /* nodes as they were last time #nodesUpdated was called */
    private Map<String, NodeSource> model = new HashMap<>();

    /* currently selected tile */
    private Tile curSelTile = null;

    /*
     * if view only my nodes is available (to see only nodes currently used by
     * the user logged)
     */
    private boolean onlyMyNodes = false;

    CompactView(RMController controller) {
        this.controller = controller;
        RMEventDispatcher eventDispatcher = controller.getEventDispatcher();
        eventDispatcher.addNodesListener(this);
        eventDispatcher.addNodeSelectedListener(this);
        this.model = new HashMap<>();
    }

    Canvas build() {
        this.root = new HLayout();
        root.setWidth100();
        root.setHeight100();
        root.setOverflow(Overflow.AUTO);
        return root;
    }

    void setViewMyNodes(boolean value) {
        this.onlyMyNodes = value;
    }

    @Override
    public void nodeUnselected() {
        if (this.curSelTile != null) {
            this.curSelTile.setSelectedTile(false);
            this.curSelTile = null;
        }
    }

    @Override
    public void nodeSelected(Node node) {
        changeSelection(flow.indexOfNode(node));
    }

    @Override
    public void nodeSourceSelected(NodeSource ns) {
        changeSelection(flow.indexOfNodeSource(ns));
    }

    @Override
    public void hostSelected(Host h) {
        changeSelection(flow.indexOfHost(h));
    }

    private void changeSelection(int id) {
        if (id < 0)
            return;

        Tile nt = (Tile) this.flow.getWidget(id);

        if (this.curSelTile != null) {
            this.curSelTile.setSelectedTile(false);
        }
        nt.setSelectedTile(true);
        this.curSelTile = nt;

        // attempt to scroll at the right position
        if (!_doNotScroll) {
            int numPerLine = flow.getElement().getClientWidth() / 20;
            if (numPerLine > 0) {
                int height = 20 * (id / numPerLine);
                this.root.scrollTo(0, height);
            }
        }
        _doNotScroll = false;
    }

    private boolean usedBy(NodeSource ns, String username) {
        for (String hostid : ns.getHosts().keySet()) {
            Host host = ns.getHosts().get(hostid);
            if (usedBy(host, username))
                return true;
        }
        return false;
    }

    private boolean usedBy(Host h, String username) {
        for (String nodeid : h.getNodes().keySet()) {
            Node node = h.getNodes().get(nodeid);
            if (usedBy(node, username))
                return true;
        }
        return false;
    }

    private boolean usedBy(Node n, String username) {
        return username != null ? username.equals(n.getNodeOwner()) : false;
    }

    @Override
    public void updateByDelta(List<NodeSource> nodeSources, List<Node> nodes) {
        /* first call : create the components */
        if (this.flow == null) {
            initializePanel();
        }
        processNodeSources(nodeSources);

        processNodes(nodes);

    }

    private void processNodes(List<Node> nodes) {
        for (Node node : nodes) {
            if (node.isRemoved()) {
                if (isNodeDrawn(node) && isNodeSourceDrawn(node.getSourceName())) {
                    final NodeSource nodeSource = model.get(node.getSourceName());
                    if (node.isDeployingNode()) {
                        nodeSource.getDeploying().remove(node.getNodeUrl());
                        flow.removeTile(node);
                    } else {
                        final Host host = nodeSource.getHosts().get(node.getHostName());
                        host.getNodes().remove(node.getNodeUrl());
                        flow.removeTile(node);
                        if (host.getNodes().isEmpty()) { // remove dangling host
                            nodeSource.getHosts().remove(host.getHostName());
                            flow.removeTile(host);
                        }
                    }
                }
            } else {
                if (!isNodeDrawn(node)) {
                    // there SHOULD be nodeSource for sure, otherwise it is server bug
                    final NodeSource existingNodeSource = model.get(node.getSourceName());

                    if (node.isDeployingNode()) {

                        existingNodeSource.getDeploying().put(node.getNodeUrl(), node);

                        drawDeployingNode(node);

                    } else {

                        Host host = existingNodeSource.getHosts().get(node.getHostName());
                        if (host == null) { // if this node is the first node of the host, thus create host
                            host = new Host(node.getHostName(), node.getSourceName());
                            if (node.isVirtual()) {
                                host.setVirtual(true);
                            }
                            existingNodeSource.getHosts().put(host.getHostName(), host);
                            drawHost(existingNodeSource, host);
                        }

                        host.getNodes().put(node.getNodeUrl(), node);

                        drawNode(node, host);

                    }
                }

                if (node.isChanged()) {
                    redrawNode(node);
                }
            }
        }
    }

    private void drawHost(NodeSource nodeSource, Host host) {
        Tile hostTile = new Tile(host);
        flow.drawHost(host, hostTile);

    }

    private void redrawNode(Node node) {
        flow.redrawNode(node);
    }

    private void drawNode(Node node, Host host) {
        Tile nodeTile = new Tile(node);
        flow.drawNode(node, nodeTile);
    }

    private void drawDeployingNode(Node node) {
        Tile nodeTile = new Tile(node);
        flow.drawDeployingNode(node, nodeTile);
    }

    private void processNodeSources(List<NodeSource> nodeSources) {
        for (NodeSource nodeSource : nodeSources) {
            if (nodeSource.isRemoved()) {
                if (isNodeSourceDrawn(nodeSource)) {
                    final NodeSource modelNodeSource = model.get(nodeSource.getSourceName());
                    flow.removeAllTiles(modelNodeSource);
                    model.remove(nodeSource.getSourceName());
                }
            } else {
                if (!isNodeSourceDrawn(nodeSource)) {
                    model.put(nodeSource.getSourceName(), nodeSource);
                    drawNodeSource(nodeSource);
                }

                if (nodeSource.isChanged()) {
                    redrawNodeSource(nodeSource);
                }
            }
        }
    }

    private void redrawNodeSource(NodeSource nodeSource) {
        flow.redrawNodeSource(nodeSource);
    }

    private void drawNodeSource(NodeSource nodeSource) {
        Tile nsTile = new Tile(nodeSource);
        flow.drawNodeSource(nodeSource, nsTile);
    }

    private void initializePanel() {
        this.flow = new CompactFlowPanel();

        this.model = new HashMap<>();

        this.root.addMember(this.flow);
        this.root.addResizedHandler(event -> {
            int w = root.getWidth();
            int h = root.getHeight();
            flow.setPixelSize(w - root.getScrollbarSize(), h - root.getScrollbarSize());

            // lazy hack to force this.flow to -really- relayout
            root.setBorder(_borderSwitch ? "1px solid white" : "0px");
            _borderSwitch = !_borderSwitch;
        });
    }

    private boolean isNodeDrawn(Node node) {
        return flow.isNodeDrawn(node).isPresent();
    }

    private boolean isNodeSourceDrawn(NodeSource nodeSource) {
        return isNodeSourceDrawn(nodeSource.getSourceName());
    }

    private boolean isNodeSourceDrawn(String sourceName) {
        return flow.isNodeSourceDrawn(sourceName);
    }

    public class Tile extends Image {

        private Node node;

        private Host host;

        private NodeSource nodesource;

        private Layout hover;

        private Label hoverLabel;

        private boolean dirty = true;

        @Override
        public void onBrowserEvent(Event event) {
            switch (DOM.eventGetType(event)) {
                case Event.ONCONTEXTMENU:

                    String lockItemImageResource = RMImages.instance.node_add_16_locked().getSafeUri().asString();
                    String unlockItemImageResource = RMImages.instance.node_add_16().getSafeUri().asString();
                    String deployItemImageResource = RMImages.instance.nodesource_deployed_16().getSafeUri().asString();
                    String undeployItemImageResource = RMImages.instance.nodesource_undeployed_16()
                                                                        .getSafeUri()
                                                                        .asString();

                    if (node != null) {
                        controller.selectNode(node);
                        lockItemImageResource = node.getIconLocked();
                        unlockItemImageResource = node.getIconUnlocked();
                    } else if (host != null) {
                        controller.selectHost(host);
                    } else if (nodesource != null) {
                        controller.selectNodeSource(nodesource);
                    }

                    Menu menu = new Menu();
                    menu.setShowShadow(true);
                    menu.setShadowDepth(10);

                    MenuItem removeItem = new MenuItem("Remove",
                                                       RMImages.instance.node_remove_16().getSafeUri().asString());
                    removeItem.addClickHandler(event15 -> controller.removeNodes());

                    MenuItem lockItem = new MenuItem("Lock", lockItemImageResource);
                    lockItem.addClickHandler(event14 -> controller.lockNodes());

                    MenuItem unlockItem = new MenuItem("Unlock", unlockItemImageResource);
                    unlockItem.addClickHandler(event13 -> controller.unlockNodes());

                    MenuItem deployItem = new MenuItem("Deploy", deployItemImageResource);
                    deployItem.addClickHandler(event12 -> controller.deployNodeSource());

                    MenuItem undeployItem = new MenuItem("Undeploy", undeployItemImageResource);
                    undeployItem.addClickHandler(event1 -> controller.undeployNodeSource());

                    if (node != null) {
                        if (node.isLocked()) {
                            lockItem.setEnabled(false);
                            unlockItem.setEnabled(true);
                        } else {
                            lockItem.setEnabled(true);
                            unlockItem.setEnabled(false);
                        }
                    }

                    if (nodesource != null) {
                        switch (nodesource.getNodeSourceStatus()) {
                            case NODES_DEPLOYED:
                                deployItem.setEnabled(false);
                                undeployItem.setEnabled(true);
                                break;
                            case NODES_UNDEPLOYED:
                                deployItem.setEnabled(true);
                                undeployItem.setEnabled(false);
                                break;
                            default:
                                disableNodeSourceDeploymentItems(deployItem, undeployItem);
                        }
                    } else {
                        disableNodeSourceDeploymentItems(deployItem, undeployItem);
                    }

                    menu.setItems(deployItem, undeployItem, lockItem, unlockItem, removeItem);

                    menu.moveTo(event.getClientX(), event.getClientY());
                    menu.show();

                    event.stopPropagation();
                    event.preventDefault();
                    break;
                default:
                    super.onBrowserEvent(event);
                    break;
            }
        }

        private void disableNodeSourceDeploymentItems(MenuItem deployItem, MenuItem undeployItem) {
            deployItem.setEnabled(false);
            undeployItem.setEnabled(false);
        }

        private void init() {
            setSize("16px", "16px");
            getElement().getStyle().setBorderColor("white");
            getElement().getStyle().setBorderStyle(BorderStyle.SOLID);
            getElement().getStyle().setBorderWidth(2.0, Unit.PX);

            this.hover = new Layout();
            this.hover.setBackgroundColor("white");
            this.hover.setBorder("1px solid gray");
            this.hover.setWidth(150);
            this.hover.setHeight(60);
            this.hover.setPadding(5);
            this.hoverLabel = new Label();
            this.hover.addMember(this.hoverLabel);

            this.addDomHandler(event -> {
                event.preventDefault();
                event.stopPropagation();
            }, ContextMenuEvent.getType());

            this.addMouseOverHandler(event -> {
                if (dirty) {
                    if (globalHover != null)
                        globalHover.hide();
                    dirty = false;
                    if (node != null) {
                        setHoverNodeLabel();
                    } else if (host != null) {
                        hoverLabel.setContents("<strong>Host</strong><br>" + host.getHostName());
                    } else if (nodesource != null) {
                        hoverLabel.setContents("<strong>NodeSource</strong><br>" + nodesource.getSourceName());
                    }
                    hover.moveTo(event.getClientX() - 155, event.getClientY() - 65);
                    hover.show();
                    globalHover = hover;
                }
            });
            this.addMouseOutHandler(event -> {
                dirty = true;
                hover.hide();
                globalHover = null;
            });

            this.addClickHandler(event -> {
                _doNotScroll = true;
                if (node != null) {
                    controller.selectNode(node);
                } else if (host != null) {
                    controller.selectHost(host);
                } else if (nodesource != null) {
                    controller.selectNodeSource(nodesource);
                }
            });
        }

        Tile(Node node) {
            super(node.getIcon());
            this.node = node;
            init();
        }

        Tile(Host host) {
            super(host.isVirtual() ? RMImages.instance.host_virtual_16().getSafeUri().asString()
                                   : RMImages.instance.host_16().getSafeUri().asString());
            this.host = host;
            init();
        }

        public Tile(NodeSource ns) {
            super(ns.getIcon());
            this.nodesource = ns;
            init();
        }

        public void refresh(Node n) {
            this.node = n;
            this.setUrl(n.getIcon());
            this.setHoverNodeLabel();
        }

        public void refresh(NodeSource ns) {
            this.nodesource = ns;
            this.setUrl(ns.getIcon());
        }

        private void setHoverNodeLabel() {
            String str = "Provider: " + node.getNodeProvider() + "<br>";
            if (node.getNodeOwner().trim().length() > 0) {
                str += "Used by: " + node.getNodeOwner() + "<br>";
            }
            str += node.getNodeState().toString() + " since " + JSUtil.getTime(node.getTimeStamp());
            this.hoverLabel.setContents(str);
        }

        void setSelectedTile(boolean selected) {
            if (selected) {
                this.getElement().getStyle().setBackgroundColor("#7caaf7");
                this.getElement().getStyle().setBorderColor("#d9e4f6");
                this.highlightGroup(true);
            } else {
                this.getElement().getStyle().setBackgroundColor("white");
                this.getElement().getStyle().setBorderColor("white");
                this.highlightGroup(false);
            }
        }

        private void highlightGroup(boolean selected) {
            // highlight every node following this one in the flow layout,
            // until we hit one from another host/ns
            if (host != null || nodesource != null) {
                int id;
                if (host == null) {
                    id = CompactView.this.flow.indexOfNodeSource(nodesource);
                } else {
                    id = CompactView.this.flow.indexOfHost(host);
                }

                while (true) {
                    id++;
                    if (flow.getWidgetCount() == id)
                        break;

                    Tile nt = (Tile) CompactView.this.flow.getWidget(id);
                    // reaching the end
                    if (nt == null) {
                        break;
                    }
                    // reaching next host/ms
                    if ((this.host != null && nt.host != null) || nt.nodesource != null) {
                        break;
                    }
                    if (selected) {
                        nt.getElement().getStyle().setBackgroundColor("#d9e4f6");
                        nt.getElement().getStyle().setBorderColor("#d9e4f6");
                    } else {
                        nt.getElement().getStyle().setBackgroundColor("white");
                        nt.getElement().getStyle().setBorderColor("white");
                    }
                }
            }
        }

        @Override
        public boolean equals(Object o) {
            Tile t = (Tile) o;
            if (this.node != null && t.node != null) {
                return this.node.getNodeUrl().equals(t.node.getNodeUrl());
            } else if (this.host != null && t.host != null) {
                return this.host.getId().equals(t.host.getId());
            } else if (this.nodesource != null && t.nodesource != null) {
                return this.nodesource.getSourceName().equals(t.nodesource.getSourceName());
            }
            return false;
        }

    }

}
