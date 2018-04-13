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

import static org.ow2.proactive_grid_cloud_portal.rm.client.NodeSource.*;
import static org.ow2.proactive_grid_cloud_portal.rm.client.NodeSource.Host.*;

import org.ow2.proactive_grid_cloud_portal.common.client.JSUtil;
import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSource;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMImages;

import com.google.gwt.dom.client.Style;
import com.google.gwt.event.dom.client.ContextMenuEvent;
import com.google.gwt.user.client.DOM;
import com.google.gwt.user.client.Event;
import com.google.gwt.user.client.ui.Image;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.MenuItemSeparator;


public class Tile extends Image {

    private Host.Node node;

    private Host host;

    private NodeSource nodesource;

    private Layout hover;

    private Label hoverLabel;

    private boolean dirty = true;

    private CompactView compactView;

    private CompactFlowPanel panel;

    Tile(CompactView compactView, CompactFlowPanel panel, NodeSource ns) {
        super(ns.getIcon());
        this.nodesource = ns;
        this.compactView = compactView;
        this.panel = panel;
        init();
    }

    Tile(CompactView compactView, CompactFlowPanel panel, Host host) {
        super(host.isVirtual() ? RMImages.instance.host_virtual_16().getSafeUri().asString()
                               : RMImages.instance.host_16().getSafeUri().asString());
        this.host = host;
        this.compactView = compactView;
        this.panel = panel;
        init();
    }

    Tile(CompactView compactView, CompactFlowPanel panel, Host.Node node) {
        super(node.getIcon());
        this.node = node;
        this.compactView = compactView;
        this.panel = panel;
        init();
    }

    @Override
    public void onBrowserEvent(Event event) {
        switch (DOM.eventGetType(event)) {
            case Event.ONCONTEXTMENU:

                String lockItemImageResource = RMImages.instance.node_add_16_locked().getSafeUri().asString();
                String unlockItemImageResource = RMImages.instance.node_add_16().getSafeUri().asString();
                String deployItemImageResource = RMImages.instance.nodesource_deployed().getSafeUri().asString();
                String undeployItemImageResource = RMImages.instance.nodesource_undeployed().getSafeUri().asString();
                String editItemImageResource = RMImages.instance.nodesource_edit().getSafeUri().asString();

                if (node != null) {
                    compactView.getController().selectNode(node);
                    lockItemImageResource = node.getIconLocked();
                    unlockItemImageResource = node.getIconUnlocked();
                } else if (host != null) {
                    compactView.getController().selectHost(host);
                } else if (nodesource != null) {
                    compactView.getController().selectNodeSource(nodesource);
                }

                Menu menu = new Menu();
                menu.setShowShadow(true);
                menu.setShadowDepth(10);

                MenuItem removeItem = new MenuItem("Remove",
                                                   RMImages.instance.node_remove_16().getSafeUri().asString());
                removeItem.addClickHandler(event15 -> compactView.getController().removeNodes());

                MenuItem lockItem = new MenuItem("Lock", lockItemImageResource);
                lockItem.addClickHandler(event14 -> compactView.getController().lockNodes());

                MenuItem unlockItem = new MenuItem("Unlock", unlockItemImageResource);
                unlockItem.addClickHandler(event13 -> compactView.getController().unlockNodes());

                MenuItem deployItem = new MenuItem("Deploy", deployItemImageResource);
                deployItem.addClickHandler(event12 -> compactView.getController().deployNodeSource());

                MenuItem undeployItem = new MenuItem("Undeploy", undeployItemImageResource);
                undeployItem.addClickHandler(event1 -> compactView.getController().undeployNodeSource());

                MenuItem editItem = new MenuItem("Edit", editItemImageResource);
                String nodeSourceName = nodesource == null ? "" : nodesource.getSourceName();
                editItem.addClickHandler(event1 -> compactView.getController().editNodeSource(nodeSourceName));

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
                            enableItems(new MenuItem[] { undeployItem });
                            disableItems(new MenuItem[] { deployItem, editItem });
                            break;
                        case NODES_UNDEPLOYED:
                            enableItems(new MenuItem[] { deployItem, editItem });
                            disableItems(new MenuItem[] { undeployItem });
                            break;
                        default:
                            disableItems(new MenuItem[] { deployItem, undeployItem, editItem });
                    }
                } else {
                    disableItems(new MenuItem[] { deployItem, undeployItem, editItem });
                }

                menu.setItems(deployItem,
                              undeployItem,
                              editItem,
                              new MenuItemSeparator(),
                              lockItem,
                              unlockItem,
                              removeItem);

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

    private void disableItems(MenuItem[] items) {
        for (MenuItem item : items) {
            item.setEnabled(false);
        }
    }

    private void enableItems(MenuItem[] items) {
        for (MenuItem item : items) {
            item.setEnabled(true);
        }
    }

    private void init() {
        setSize("16px", "16px");
        getElement().getStyle().setBorderColor("white");
        getElement().getStyle().setBorderStyle(Style.BorderStyle.SOLID);
        getElement().getStyle().setBorderWidth(2.0, Style.Unit.PX);

        this.hover = new Layout();
        this.hover.setBackgroundColor("white");
        this.hover.setBorder("1px solid gray");
        this.hover.setWidth(150);
        this.hover.setHeight(60);
        this.hover.setPadding(5);
        this.hoverLabel = new Label();
        this.hover.addMember(this.hoverLabel);

        addDomHandler();
        addMouseOverHandler();
        addMouseOutHandler();
        addClickHandler();
    }

    private void addDomHandler() {
        this.addDomHandler(event -> {
            event.preventDefault();
            event.stopPropagation();
        }, ContextMenuEvent.getType());
    }

    private void addMouseOverHandler() {
        this.addMouseOverHandler(event -> {
            if (dirty) {
                if (CompactView.getGlobalHover() != null) {
                    CompactView.getGlobalHover().hide();
                }
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
                CompactView.setGlobalHover(hover);
            }
        });
    }

    private void addClickHandler() {
        this.addClickHandler(event -> {
            compactView.setDoNotScroll(true);
            if (node != null) {
                compactView.getController().selectNode(node);
            } else if (host != null) {
                compactView.getController().selectHost(host);
            } else if (nodesource != null) {
                compactView.getController().selectNodeSource(nodesource);
            }
        });
    }

    private void addMouseOutHandler() {
        this.addMouseOutHandler(event -> {
            dirty = true;
            hover.hide();
            CompactView.setGlobalHover(null);
        });
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
        // highlight every node following this one in the compactPanel layout,
        // until we hit one from another host/ns
        if (host != null || nodesource != null) {
            int id;
            if (host == null) {
                id = panel.indexOf(nodesource).get();
            } else {
                id = panel.indexOf(host).get();
            }

            while (true) {
                id++;
                if (panel.getWidgetCount() == id)
                    break;

                Tile tile = (Tile) panel.getWidget(id);
                // reaching the end
                if (tile == null) {
                    break;
                }
                // reaching next host/ms
                if ((this.host != null && tile.host != null) || tile.nodesource != null) {
                    break;
                }
                if (selected) {
                    tile.getElement().getStyle().setBackgroundColor("#d9e4f6");
                    tile.getElement().getStyle().setBorderColor("#d9e4f6");
                } else {
                    tile.getElement().getStyle().setBackgroundColor("white");
                    tile.getElement().getStyle().setBorderColor("white");
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

    public Node getNode() {
        return node;
    }

    public Host getHost() {
        return host;
    }

    public NodeSource getNodesource() {
        return nodesource;
    }

}
