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
import java.util.Optional;
import java.util.stream.Collectors;

import com.smartgwt.client.widgets.WidgetCanvas;
import org.ow2.proactive_grid_cloud_portal.common.client.JSUtil;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LogModel;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LoginModel;
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
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.views.CompactFlowPanelOwn;


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

    /* displays own nodes as a compact grid */
    private CompactFlowPanelOwn ownFlow;

    private WidgetCanvas flowCanvas = null;

    private WidgetCanvas ownFlowCanvas = null;

    private boolean _borderSwitch;

    private boolean _doNotScroll;

    private static Layout globalHover = null;


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
    }

    Canvas build() {
        this.root = new HLayout();
        root.setWidth100();
        root.setHeight100();
        root.setOverflow(Overflow.AUTO);
        return root;
    }

    void setViewMyNodes(boolean value) {
        onlyMyNodes = value;
        if(onlyMyNodes){
            root.removeMember(flowCanvas);
            root.addMember(ownFlowCanvas);
        } else {
            root.removeMember(ownFlowCanvas);
            root.addMember(flowCanvas);
        }

    }

    @Override
    public void nodeUnselected() {
        if (flow.getCurSelTile()!= null) {
            flow.getCurSelTile().setSelectedTile(false);
            flow.setCurSelTile(null);
        }
        if (ownFlow.getCurSelTile()!= null) {
            ownFlow.getCurSelTile().setSelectedTile(false);
            ownFlow.setCurSelTile(null);
        }

    }

    @Override
    public void nodeSourceSelected(NodeSource ns) {
        flow.indexOf(ns).ifPresent(integer -> changeSelection(flow, integer));
        ownFlow.indexOf(ns).ifPresent(integer -> changeSelection(ownFlow, integer));
    }

    @Override
    public void hostSelected(Host h) {
        flow.indexOf(h).ifPresent(integer -> changeSelection(flow, integer));
        ownFlow.indexOf(h).ifPresent(integer -> changeSelection(ownFlow, integer));
    }

    @Override
    public void nodeSelected(Node node) {
        flow.indexOf(node).ifPresent(integer -> changeSelection(flow, integer));
        ownFlow.indexOf(node).ifPresent(integer -> changeSelection(ownFlow, integer));
    }

    private void changeSelection(CompactFlowPanel flow, int id) {
        if (id < 0) {
            return;
        }

        Tile nt = (Tile) flow.getWidget(id);

        if (flow.getCurSelTile() != null) {
            flow.getCurSelTile().setSelectedTile(false);
        }
        nt.setSelectedTile(true);
        flow.setCurSelTile(nt);

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

    private boolean usedBy(Node n, String username) {
        return username != null && username.equals(n.getNodeOwner());
    }

    @Override
    public void updateByDelta(List<NodeSource> nodeSources, List<Node> nodes) {
        /* first call : create the components */
        if (this.flow == null) {
            initializePanel();
        }
        processNodeSources(flow, nodeSources);

        processNodes(flow, nodes);

        processNodeSources(ownFlow, nodeSources);

        processNodes(ownFlow, filterOwnNodes(nodes));

//         if ownFlow already have some nodes, but they do not became to us anymore, then delete them
        filterOtherNodes(nodes).stream()
                .filter(node -> ownFlow.isNodeDrawn(node))
                .forEach(node -> ownFlow.remove(node));


    }

    private void processNodes(CompactFlowPanel flow, List<Node> nodes) {
        for (Node node : nodes) {
            if (node.isRemoved()) {
                if (flow.isNodeDrawn(node)
                        && flow.isNodeSourceDrawn(node.getSourceName())) {
                    flow.remove(node);
                }
            } else {
                if (!flow.isNodeDrawn(node)) {
                    Tile nodeTile = new Tile(node);
                    Tile hostTile = new Tile(new Host(node.getHostName(), node.getSourceName()));
                    if(node.isVirtual()){
                        hostTile.getHost().setVirtual(true);
                    }
                    flow.drawNode(nodeTile, hostTile);
                }

                if (node.isChanged()) {
                    flow.redrawNode(node);
                }
            }
        }
    }


    private void processNodeSources(CompactFlowPanel flow, List<NodeSource> nodeSources) {
        for (NodeSource nodeSource : nodeSources) {
            if (nodeSource.isRemoved()) {
                if (flow.isNodeSourceDrawn(nodeSource.getSourceName())) {
                    flow.remove(nodeSource);
                }
            } else {
                if (!flow.isNodeSourceDrawn(nodeSource.getSourceName())) {
                    Tile nsTile = new Tile(nodeSource);
                    flow.drawNodeSource(nsTile);
                }

                if (nodeSource.isChanged()) {
                    flow.redrawNodeSource(nodeSource);
                }
            }
        }
    }



    private void initializePanel() {
        flow = new CompactFlowPanel();
        ownFlow = new CompactFlowPanelOwn();

        flowCanvas = new WidgetCanvas(flow);
        ownFlowCanvas = new WidgetCanvas(ownFlow);

        root.addMember(flowCanvas);


        root.addResizedHandler(event -> {
            int w = root.getWidth();
            int h = root.getHeight();
            flow.setPixelSize(w - root.getScrollbarSize(), h - root.getScrollbarSize());


            // lazy hack to force this.flow to -really- relayout
            root.setBorder(_borderSwitch ? "1px solid white" : "0px");
            _borderSwitch = !_borderSwitch;
        });

        root.addResizedHandler(event -> {
            int w = root.getWidth();
            int h = root.getHeight();
            ownFlow.setPixelSize(w - root.getScrollbarSize(), h - root.getScrollbarSize());


            // lazy hack to force this.flow to -really- relayout
            root.setBorder(_borderSwitch ? "1px solid white" : "0px");
            _borderSwitch = !_borderSwitch;
        });
    }


    private List<Node> filterOwnNodes(List<Node> nodes){
        String username = LoginModel.getInstance().getLogin();

        return nodes.stream()
                .filter(node -> usedBy(node, username))
                .collect(Collectors.toList());
    }

    private List<Node> filterOtherNodes(List<Node> nodes){
        String username = LoginModel.getInstance().getLogin();

        return nodes.stream()
                .filter(node -> !usedBy(node, username))
                .collect(Collectors.toList());
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

        Tile(NodeSource ns) {
            super(ns.getIcon());
            this.nodesource = ns;
            init();
        }

        public Node getNode() {
            return node;
        }

        public Host getHost() {
            return host;
        }

        public NodeSource getNodesource() {
            return nodesource;
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
                    id = CompactView.this.flow.indexOf(nodesource).get();
                } else {
                    id = CompactView.this.flow.indexOf(host).get();
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
