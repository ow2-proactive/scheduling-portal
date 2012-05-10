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

import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.Map.Entry;

import org.ow2.proactive_grid_cloud_portal.common.client.JSUtil;
import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSource.Host;
import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSource.Host.Node;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMListeners.NodeSelectedListener;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMListeners.NodesListener;

import com.google.gwt.dom.client.Style.BorderStyle;
import com.google.gwt.dom.client.Style.Unit;
import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.dom.client.ContextMenuEvent;
import com.google.gwt.event.dom.client.ContextMenuHandler;
import com.google.gwt.event.dom.client.MouseOutEvent;
import com.google.gwt.event.dom.client.MouseOutHandler;
import com.google.gwt.event.dom.client.MouseOverEvent;
import com.google.gwt.event.dom.client.MouseOverHandler;
import com.google.gwt.user.client.DOM;
import com.google.gwt.user.client.Event;
import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.Image;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.events.ResizedEvent;
import com.smartgwt.client.widgets.events.ResizedHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;


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
    private FlowPanel flow;
    boolean _borderSwitch;
    boolean _doNotScroll;
    private static Layout globalHover = null;

    /* unique names of the nodesources/hosts/nodes currently held by the FlowLayout 
     * helps figuring out the position of each element since FlowLayout doesnt allow it */
    private LinkedList<String> curTiles = null;
    /* nodes as they were last time #nodesUpdated was called */
    private Map<String, NodeSource> oldNodes = null;
    /* currently selected tile */
    private NodeTile curSelTile = null;

    CompactView(RMController controller) {
        this.controller = controller;
        controller.getEventDispatcher().addNodesListener(this);
        controller.getEventDispatcher().addNodeSelectedListener(this);
        this.oldNodes = new HashMap<String, NodeSource>();
    }

    Canvas build() {
        this.root = new HLayout();
        root.setWidth100();
        root.setHeight100();
        root.setOverflow(Overflow.AUTO);
        return root;
    }

    @Override
    public void nodeSelected(Node node) {
        changeSelection(node.getNodeUrl());
    }

    @Override
    public void nodeUnselected() {
        if (this.curSelTile != null) {
            this.curSelTile.setSelectedTile(false);
            this.curSelTile = null;
        }
    }

    @Override
    public void nodeSourceSelected(NodeSource ns) {
        changeSelection(ns.getSourceName());
    }

    @Override
    public void hostSelected(Host h) {
        changeSelection(h.getId());
    }

    private void changeSelection(String name) {
        int id = this.curTiles.indexOf(name);
        NodeTile nt = (NodeTile) this.flow.getWidget(id);

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

    @Override
    public void nodesUpdated(Map<String, NodeSource> nodes) {
        /* first call : create the components */
        if (this.flow == null) {
            this.flow = new FlowPanel();
            this.flow.setWidth("100%");
            // removes the vertical space between lines
            this.flow.getElement().getStyle().setProperty("lineHeight", "0");
            this.curTiles = new LinkedList<String>();

            // add every tile at once, else it will attempt to render the page
            // each time we add a new tile
            for (NodeSource ns : nodes.values()) {
                flow.add(new NodeTile(ns));
                curTiles.add(ns.getSourceName());
                for (Node n : ns.getDeploying().values()) {
                    flow.add(new NodeTile(n));
                    curTiles.add(n.getNodeUrl());
                }
                for (Host hh : ns.getHosts().values()) {
                    flow.add(new NodeTile(hh));
                    curTiles.add(hh.getId());
                    for (Node n : hh.getNodes().values()) {
                        flow.add(new NodeTile(n));
                        curTiles.add(n.getNodeUrl());
                    }
                }
            }
            this.root.addMember(this.flow);
            this.root.addResizedHandler(new ResizedHandler() {
                @Override
                public void onResized(ResizedEvent event) {
                    int w = root.getWidth();
                    int h = root.getHeight();
                    flow.setPixelSize(w - root.getScrollbarSize(), h - root.getScrollbarSize());

                    // lazy hack to force this.flow to -really- relayout
                    root.setBorder(_borderSwitch ? "1px solid white" : "0px");
                    _borderSwitch = !_borderSwitch;
                }
            });
        } else {
            /* for each new nodesource */
            for (NodeSource ns : nodes.values()) {
                String nsName = ns.getSourceName();

                NodeSource oldNs = (oldNodes != null) ? oldNodes.get(nsName) : null;
                /* new nodesource : adding at the end */
                if (oldNs == null) {
                    // WARN at first I was inserting new NS at the head
                    // but it was added at the end without any warning!
                    NodeTile nsTile = new NodeTile(ns);
                    int i = curTiles.size();
                    flow.insert(nsTile, i);
                    this.curTiles.add(i, nsName);
                }

                /* deploying nodes : not in a host yet */
                for (Node n : ns.getDeploying().values()) {
                    String nodeUrl = n.getNodeUrl();

                    Node oldNode = (oldNs != null) ? oldNs.getDeploying().get(nodeUrl) : null;
                    /* new deploying node */
                    if (oldNode == null) {
                        NodeTile nodeTile = new NodeTile(n);
                        int i = curTiles.indexOf(nsName) + 1;
                        flow.insert(nodeTile, i);
                        this.curTiles.add(i, nodeUrl);
                    } else {
                        if (!oldNode.getNodeState().equals(n.getNodeState())) {
                            int i = this.curTiles.indexOf(nodeUrl);
                            NodeTile nt = ((NodeTile) this.flow.getWidget(i));
                            nt.refresh(n);
                        }
                    }
                }

                /* hosts */
                for (Host h : ns.getHosts().values()) {
                    String hostName = h.getHostName();

                    Host oldHost = (oldNs != null) ? oldNs.getHosts().get(hostName) : null;
                    /* new host */
                    if (oldHost == null) {
                        NodeTile hostTile = new NodeTile(h);
                        int i = curTiles.indexOf(nsName) + 1 + ns.getDeploying().size();
                        flow.insert(hostTile, i);
                        this.curTiles.add(i, h.getId());
                    }
                    /* nodes */
                    for (Node n : h.getNodes().values()) {
                        String nodeUrl = n.getNodeUrl();

                        Node oldNode = (oldHost != null) ? oldHost.getNodes().get(nodeUrl) : null;
                        /* new node */
                        if (oldNode == null) {
                            NodeTile nodeTile = new NodeTile(n);
                            int i = curTiles.indexOf(h.getId()) + 1;
                            flow.insert(nodeTile, i);
                            this.curTiles.add(i, nodeUrl);
                        }
                        /* update old node status */
                        else {
                            if (!oldNode.getNodeState().equals(n.getNodeState())) {
                                int i = this.curTiles.indexOf(nodeUrl);
                                NodeTile nt = ((NodeTile) this.flow.getWidget(i));
                                nt.refresh(n);
                            }
                        }
                    }
                }
            }

            /* now remove the difference between the nodes from this method call,
             * and the ones from the previous call */
            for (Entry<String, NodeSource> oldNs : this.oldNodes.entrySet()) {
                /* Keep NS */
                NodeSource newNs = nodes.get(oldNs.getKey());
                if (newNs != null) {
                    for (Entry<String, Node> oldDepl : oldNs.getValue().getDeploying().entrySet()) {
                        /* Keep deploying Node */
                        if (newNs.getDeploying().containsKey(oldDepl.getKey())) {

                        }
                        /* Deploying Node to be removed */
                        else if (curTiles.contains(oldDepl.getKey())) {
                            removeTile(oldDepl.getKey());
                        }
                    }

                    for (Entry<String, Host> oldHost : oldNs.getValue().getHosts().entrySet()) {
                        /* Keep host */
                        Host newHost = newNs.getHosts().get(oldHost.getKey());
                        if (newHost != null && newHost.getNodes().size() > 0) {
                            for (Entry<String, Node> oldNode : oldHost.getValue().getNodes().entrySet()) {
                                /* Keep node */
                                if (newHost.getNodes().containsKey(oldNode.getKey())) {
                                }
                                /* Node to be removed */
                                else if (curTiles.contains(oldNode.getKey())) {
                                    removeTile(oldNode.getKey());
                                }
                            }

                        }
                        /* Host to be removed */
                        else if (curTiles.contains(oldHost.getValue().getId())) {
                            removeTile(oldHost.getValue().getId());
                            for (String n : oldHost.getValue().getNodes().keySet()) {
                                removeTile(n);
                            }
                        }
                    }

                }
                /* remove NS */
                else {
                    removeTile(oldNs.getKey());
                    for (Node n : oldNs.getValue().getDeploying().values()) {
                        removeTile(n.getNodeUrl());
                    }
                    for (Host h : oldNs.getValue().getHosts().values()) {
                        removeTile(h.getId());
                        for (String n : h.getNodes().keySet()) {
                            removeTile(n);
                        }
                    }
                }
            }
        }
        this.oldNodes = nodes;
    }

    private void removeTile(String name) {
        int index = this.curTiles.indexOf(name);
        if (index >= 0) {
            this.curTiles.remove(index);
            this.flow.remove(index);
        }
    }

    private class NodeTile extends Image {
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
                    if (node != null) {
                        controller.selectNode(node);
                    } else if (host != null) {
                        controller.selectHost(host);
                    } else if (nodesource != null) {
                        controller.selectNodeSource(nodesource);
                    }

                    Menu menu = new Menu();
                    menu.setShowShadow(true);
                    menu.setShadowDepth(10);

                    MenuItem removeItem = new MenuItem("Remove", RMImages.instance.node_remove_16()
                            .getSafeUri().asString());
                    removeItem.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
                        @Override
                        public void onClick(MenuItemClickEvent event) {
                            controller.removeNodes();
                        }
                    });

                    MenuItem lockItem = new MenuItem("Lock", RMImages.instance.node_locked_16().getSafeUri()
                            .asString());
                    lockItem.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
                        @Override
                        public void onClick(MenuItemClickEvent event) {
                            controller.lockNodes();
                        }
                    });

                    MenuItem unlockItem = new MenuItem("Unlock", RMImages.instance.node_free_16()
                            .getSafeUri().asString());
                    unlockItem.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
                        @Override
                        public void onClick(MenuItemClickEvent event) {
                            controller.unlockNodes();
                        }
                    });

                    menu.setItems(lockItem, unlockItem, removeItem);
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

            this.addDomHandler(new ContextMenuHandler() {
                @Override
                public void onContextMenu(ContextMenuEvent event) {
                    event.preventDefault();
                    event.stopPropagation();
                }
            }, ContextMenuEvent.getType());

            this.addMouseOverHandler(new MouseOverHandler() {
                @Override
                public void onMouseOver(MouseOverEvent event) {
                    if (dirty) {
                        if (globalHover != null)
                            globalHover.hide();
                        dirty = false;
                        if (node != null) {
                            setHoverNodeLabel();
                        } else if (host != null) {
                            hoverLabel.setContents("<strong>Host</strong><br>" + host.getHostName());
                        } else if (nodesource != null) {
                            hoverLabel.setContents("<strong>NodeSource</strong><br>" +
                                nodesource.getSourceName());
                        }
                        hover.moveTo(event.getClientX() - 155, event.getClientY() - 65);
                        hover.show();
                        globalHover = hover;
                    }
                }
            });
            this.addMouseOutHandler(new MouseOutHandler() {
                @Override
                public void onMouseOut(MouseOutEvent event) {
                    dirty = true;
                    hover.hide();
                    globalHover = null;
                }
            });

            this.addClickHandler(new ClickHandler() {
                @Override
                public void onClick(ClickEvent event) {
                    _doNotScroll = true;
                    if (node != null) {
                        controller.selectNode(node);
                    } else if (host != null) {
                        controller.selectHost(host);
                    } else if (nodesource != null) {
                        controller.selectNodeSource(nodesource);
                    }
                }
            });
        }

        public NodeTile(Node node) {
            super(node.getNodeState().getIcon());
            this.node = node;
            init();
        }

        public NodeTile(Host host) {
            super(host.isVirtual() ? RMImages.instance.host_virtual_16().getSafeUri().asString()
                    : RMImages.instance.host_16().getSafeUri().asString());
            this.host = host;
            init();
        }

        public NodeTile(NodeSource ns) {
            super(RMImages.instance.nodesource_16().getSafeUri().asString());
            this.nodesource = ns;
            init();
        }

        public void refresh(Node n) {
            this.node = n;
            this.setUrl(n.getNodeState().getIcon());
            this.setHoverNodeLabel();
        }

        private void setHoverNodeLabel() {
            String str = "Provider: " + node.getNodeProvider() + "<br>";
            if (node.getNodeOwner().trim().length() > 0) {
                str += "Used by: " + node.getNodeOwner() + "<br>";
            }
            str += node.getNodeState().toString() + " since " + JSUtil.getTime(node.getTimeStamp());
            this.hoverLabel.setContents(str);
        }

        public void setSelectedTile(boolean selected) {
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
                int id = CompactView.this.curTiles.indexOf((host == null) ? nodesource.getSourceName() : host
                        .getId());

                while (true) {
                    id++;
                    if (flow.getWidgetCount() == id)
                        break;

                    NodeTile nt = (NodeTile) CompactView.this.flow.getWidget(id);
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
            NodeTile t = (NodeTile) o;
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
