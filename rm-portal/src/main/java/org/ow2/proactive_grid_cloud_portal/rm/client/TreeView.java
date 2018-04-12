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

import org.ow2.proactive_grid_cloud_portal.common.client.Images;
import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSource.Host;
import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSource.Host.Node;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMListeners.NodeSelectedListener;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMListeners.NodesListener;
import org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.EditDynamicParametersWindow;
import org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.EditNodeSourceWindow;

import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.TreeModelType;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.MenuItemSeparator;
import com.smartgwt.client.widgets.tree.Tree;
import com.smartgwt.client.widgets.tree.TreeGrid;
import com.smartgwt.client.widgets.tree.TreeGridField;
import com.smartgwt.client.widgets.tree.TreeNode;


/**
 * Displays current nodes in a hierarchical tree view
 * <p>
 * NodeSource > Host > Node
 *
 * @author mschnoor
 */
public class TreeView implements NodesListener, NodeSelectedListener {

    private static final String NODE_ID = "nodeId";

    private RMController controller = null;

    /**
     * tree view
     */
    private TreeGrid treeGrid = null;

    /**
     * tree data
     */
    private Tree tree = null;

    /**
     * treenodes currently held by {@link #tree}
     */
    private Map<String, TreeNode> currentNodes = null;

    /**
     * prevent event cycling
     */
    private boolean ignoreNodeSelectedEvent = false;

    private class TNode extends TreeNode {
        Node rmNode = null;

        TNode(String name, Node node) {
            super(name);
            this.rmNode = node;
            this.setAttribute(NODE_ID, node.getNodeUrl());
        }
    }

    private class THost extends TreeNode {
        Host rmHost = null;

        THost(String name, Host h) {
            super(name);
            this.rmHost = h;
            this.setAttribute(NODE_ID, h.getId());
        }
    }

    private class TNS extends TreeNode {
        NodeSource rmNS = null;

        TNS(String name, NodeSource ns) {
            super(name);
            this.rmNS = ns;
            this.setAttribute(NODE_ID, ns.getSourceName());
        }
    }

    TreeView(RMController controller) {
        this.controller = controller;
        this.controller.getEventDispatcher().addNodesListener(this);
        this.controller.getEventDispatcher().addNodeSelectedListener(this);
        this.currentNodes = new HashMap<>();
    }

    Canvas build() {
        VLayout vl = new VLayout();

        this.treeGrid = new TreeGrid();
        treeGrid.setWidth100();
        treeGrid.setHeight100();
        treeGrid.setShowHeader(false);
        treeGrid.setSelectionType(SelectionStyle.SINGLE);

        TreeGridField field = new TreeGridField("name");
        field.setCanSort(true);
        field.setSortByDisplayField(true);

        treeGrid.setFields(field);
        treeGrid.setSortField("name");

        this.tree = new Tree();
        tree.setModelType(TreeModelType.PARENT);
        tree.setNameProperty("name");
        tree.setIdField(NODE_ID);

        this.treeGrid.setData(this.tree);

        this.treeGrid.addNodeClickHandler(event -> {
            TreeNode n = event.getNode();
            if (n instanceof TNode) {
                TNode tn = (TNode) n;
                ignoreNodeSelectedEvent = true;
                TreeView.this.controller.selectNode(tn.rmNode);
            } else if (n instanceof TNS) {
                TNS tn = (TNS) n;
                ignoreNodeSelectedEvent = true;
                TreeView.this.controller.selectNodeSource(tn.rmNS);
            } else if (n instanceof THost) {
                THost tn = (THost) n;
                ignoreNodeSelectedEvent = true;
                TreeView.this.controller.selectHost(tn.rmHost);
            }
        });

        this.treeGrid.addNodeContextClickHandler(event -> {

            String lockItemImageResource = RMImages.instance.node_add_16_locked().getSafeUri().asString();
            String unlockItemImageResource = RMImages.instance.node_add_16().getSafeUri().asString();
            String deployItemImageResource = RMImages.instance.nodesource_deployed().getSafeUri().asString();
            String undeployItemImageResource = RMImages.instance.nodesource_undeployed().getSafeUri().asString();
            String editItemImageResource = RMImages.instance.nodesource_edit().getSafeUri().asString();

            NodeSource currentSelectedNodeSource = null;
            Node currentSelectedNode = null;

            final TreeNode treeNode = event.getNode();
            if (treeNode instanceof TNode) {
                TNode tn = (TNode) treeNode;
                TreeView.this.controller.selectNode(tn.rmNode);
                lockItemImageResource = tn.rmNode.getIconLocked();
                unlockItemImageResource = tn.rmNode.getIconUnlocked();
                currentSelectedNode = tn.rmNode;
            } else if (treeNode instanceof TNS) {
                TNS tn = (TNS) treeNode;
                TreeView.this.controller.selectNodeSource(tn.rmNS);
                currentSelectedNodeSource = tn.rmNS;
            } else if (treeNode instanceof THost) {
                THost tn = (THost) treeNode;
                TreeView.this.controller.selectHost(tn.rmHost);
            }

            Menu menu = new Menu();
            menu.setShowShadow(true);
            menu.setShadowDepth(10);

            MenuItem expandItem = new MenuItem("Expand all", Images.instance.expand_16().getSafeUri().asString());
            expandItem.addClickHandler(event17 -> expandAll());

            MenuItem collapseItem = new MenuItem("Collapse all", Images.instance.close_16().getSafeUri().asString());
            collapseItem.addClickHandler(event16 -> closeAll());

            MenuItem removeItem = new MenuItem("Remove", RMImages.instance.node_remove_16().getSafeUri().asString());
            removeItem.addClickHandler(event15 -> controller.removeNodes());

            MenuItem lockItem = new MenuItem("Lock", lockItemImageResource);
            lockItem.addClickHandler(event14 -> controller.lockNodes());

            MenuItem unlockItem = new MenuItem("Unlock", unlockItemImageResource);
            unlockItem.addClickHandler(event13 -> controller.unlockNodes());

            MenuItem deployItem = new MenuItem("Deploy", deployItemImageResource);
            deployItem.addClickHandler(event12 -> controller.deployNodeSource());

            MenuItem undeployItem = new MenuItem("Undeploy", undeployItemImageResource);
            undeployItem.addClickHandler(event1 -> controller.undeployNodeSource());

            MenuItem editItem = new MenuItem("Edit", editItemImageResource);
            String nodeSourceName = currentSelectedNodeSource == null ? "" : currentSelectedNodeSource.getSourceName();
            NodeSourceStatus nodeSourceStatus = currentSelectedNodeSource == null ? null
                                                                                  : currentSelectedNodeSource.getNodeSourceStatus();
            editItem.addClickHandler(event1 -> controller.editNodeSource(nodeSourceName, nodeSourceStatus));

            if (currentSelectedNode != null) {
                if (currentSelectedNode.isLocked()) {
                    lockItem.setEnabled(false);
                    unlockItem.setEnabled(true);
                } else {
                    lockItem.setEnabled(true);
                    unlockItem.setEnabled(false);
                }
            }

            if (currentSelectedNodeSource != null) {
                switch (currentSelectedNodeSource.getNodeSourceStatus()) {
                    case NODES_DEPLOYED:
                        editItem.setTitle(EditDynamicParametersWindow.WINDOW_TITLE);
                        enableItems(new MenuItem[] { undeployItem });
                        disableItems(new MenuItem[] { deployItem });
                        break;
                    case NODES_UNDEPLOYED:
                        editItem.setTitle(EditNodeSourceWindow.WINDOW_TITLE);
                        enableItems(new MenuItem[] { deployItem });
                        disableItems(new MenuItem[] { undeployItem });
                        break;
                    default:
                        disableItems(new MenuItem[] { deployItem, undeployItem, editItem });
                }
            } else {
                disableItems(new MenuItem[] { deployItem, undeployItem, editItem });
            }

            menu.setItems(expandItem,
                          collapseItem,
                          new MenuItemSeparator(),
                          deployItem,
                          undeployItem,
                          editItem,
                          new MenuItemSeparator(),
                          lockItem,
                          unlockItem,
                          removeItem);

            treeGrid.setContextMenu(menu);
        });

        vl.addMember(treeGrid);
        return vl;
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

    @Override
    public void updateByDelta(List<NodeSource> nodeSources, List<Node> nodes) {
        processNodeSources(nodeSources);

        treeGrid.refreshFields();

        processNodes(nodes);

        treeGrid.markForRedraw();
    }

    private void processNodes(List<Node> nodes) {
        if (!nodes.isEmpty()) {
            for (Node node : nodes) {
                if (node.isRemoved()) {
                    removeNode(node);
                } else {
                    addNodeIfNotExists(node);
                    changeNodeStatusIfChanged(node);
                }
            }
        }
    }

    private void changeNodeStatusIfChanged(Node node) {
        if (node.isChanged()) {
            TNode treeNode = (TNode) currentNodes.get(node.getNodeUrl());
            treeNode.setAttribute("nodeState", node.getNodeState().toString());
            treeNode.rmNode = node;
            treeNode.setIcon(node.getIcon());
        }
    }

    private void addNodeIfNotExists(Node node) {
        if (!currentNodes.containsKey(node.getNodeUrl())) { // if there is no node
            if (node.isDeployingNode()) {
                TNode nodeTreeNode = new TNode(node.getNodeUrl(), node);
                nodeTreeNode.setIcon(node.getIcon());
                tree.add(nodeTreeNode, this.currentNodes.get(node.getSourceName()));
                currentNodes.put(node.getNodeUrl(), nodeTreeNode);
            } else {
                final Host host = new Host(node.getHostName(), node.getSourceName());

                if (!currentNodes.containsKey(host.getId())) { // no host as well
                    // we should add host first
                    THost hostTreeNode = new THost(node.getHostName(), host);
                    if (node.isVirtual()) {
                        hostTreeNode.setIcon(RMImages.instance.host_virtual_16().getSafeUri().asString());
                    } else {
                        hostTreeNode.setIcon(RMImages.instance.host_16().getSafeUri().asString());
                    }

                    tree.add(hostTreeNode, currentNodes.get(node.getSourceName()));
                    currentNodes.put(host.getId(), hostTreeNode);
                }

                TNode nodeTreeNode = new TNode(node.getNodeUrl(), node);
                nodeTreeNode.setAttribute("nodeState", node.getNodeState().toString());
                nodeTreeNode.setIcon(node.getIcon());
                tree.add(nodeTreeNode, currentNodes.get(host.getId()));
                currentNodes.put(node.getNodeUrl(), nodeTreeNode);
            }
        }
    }

    private void removeNode(Node node) {
        if (currentNodes.containsKey(node.getNodeUrl())) {
            final TreeNode toRemove = currentNodes.get(node.getNodeUrl());

            final TreeNode parent = tree.getParent(toRemove);
            tree.remove(toRemove);
            currentNodes.remove(node.getNodeUrl());

            if (!node.isDeployingNode() && !tree.hasChildren(parent)) { // thus this node has a host, which might be removed
                tree.remove(parent);
                currentNodes.remove(parent.getAttribute(NODE_ID));
            }
        }

    }

    private void processNodeSources(List<NodeSource> nodeSources) {
        if (!nodeSources.isEmpty()) {
            for (NodeSource nodeSource : nodeSources) {
                if (nodeSource.isRemoved()) {
                    removeNodeSource(nodeSource);
                } else {
                    addNodeSourceIfNotExists(nodeSource);
                    changeNodeSourceStatusIfChanged(nodeSource);
                }
            }
        }
    }

    private void changeNodeSourceStatusIfChanged(NodeSource nodeSource) {
        if (nodeSource.isChanged()) {
            TNS curTreeNodeSource = (TNS) currentNodes.get(nodeSource.getSourceName());
            curTreeNodeSource.rmNS = nodeSource;
            curTreeNodeSource.setName(getNodeSourceDisplayedDescription(nodeSource, nodeSource.getSourceName()));
            curTreeNodeSource.setIcon(nodeSource.getIcon());
        }
    }

    private void addNodeSourceIfNotExists(NodeSource nodeSource) {
        if (!currentNodes.containsKey(nodeSource.getSourceName())) {
            TNS nsTreeNode = new TNS(getNodeSourceDisplayedDescription(nodeSource, nodeSource.getSourceName()),
                                     nodeSource);
            nsTreeNode.setIcon(nodeSource.getIcon());
            tree.add(nsTreeNode, this.tree.getRoot());
            currentNodes.put(nodeSource.getSourceName(), nsTreeNode);
        }

    }

    private void removeNodeSource(NodeSource nodeSource) {
        if (currentNodes.containsKey(nodeSource.getSourceName())) {
            final TreeNode treeNodeSource = currentNodes.remove(nodeSource.getSourceName());

            for (TreeNode treeNode : tree.getAllNodes(treeNodeSource)) {
                if (treeNode instanceof THost) {
                    final THost tHost = (THost) treeNode;
                    currentNodes.remove(tHost.rmHost.getId());
                } else if (treeNode instanceof TNode) {
                    final TNode tNode = (TNode) treeNode;
                    currentNodes.remove(tNode.rmNode.getNodeUrl());
                }
            }

            tree.remove(treeNodeSource);
            currentNodes.remove(nodeSource.getSourceName());
        }
    }

    private String getNodeSourceDisplayedDescription(NodeSource ns, String nsName) {
        return nsName + " <span style='color:#777;'>" + ns.getSourceDescription() + ", Owner: " +
               ns.getNodeSourceAdmin() + "</span>";
    }

    void expandAll() {
        tree.openAll();
    }

    void closeAll() {
        tree.closeAll();
    }

    private void scrollList(TreeNode tn) {
        int id = treeGrid.getRecordIndex(tn);
        if (id >= 0) {
            treeGrid.scrollToRow(id);
        }
    }

    @Override
    public void nodeSelected(Node node) {
        if (ignoreNodeSelectedEvent) {
            ignoreNodeSelectedEvent = false;
            return;
        }

        treeGrid.deselectAllRecords();
        TreeNode treeNode = currentNodes.get(node.getNodeUrl());
        treeGrid.selectRecord(treeNode, true);
        scrollList(treeNode);
    }

    @Override
    public void nodeUnselected() {
        this.treeGrid.deselectAllRecords();
    }

    @Override
    public void nodeSourceSelected(NodeSource ns) {
        if (ignoreNodeSelectedEvent) {
            ignoreNodeSelectedEvent = false;
            return;
        }

        treeGrid.deselectAllRecords();
        TreeNode treeNode = currentNodes.get(ns.getSourceName());
        treeGrid.selectRecord(treeNode, true);
        scrollList(treeNode);
    }

    @Override
    public void hostSelected(Host h) {
        if (ignoreNodeSelectedEvent) {
            ignoreNodeSelectedEvent = false;
            return;
        }

        treeGrid.deselectAllRecords();
        TreeNode treeNode = currentNodes.get(h.getId());
        treeGrid.selectRecord(treeNode, true);
        scrollList(treeNode);
    }
}
