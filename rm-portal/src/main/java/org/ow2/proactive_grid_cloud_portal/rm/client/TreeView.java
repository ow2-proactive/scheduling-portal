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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.ow2.proactive_grid_cloud_portal.common.client.Settings;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LogModel;
import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSource.Host;
import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSource.Host.Node;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMListeners.NodeSelectedListener;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMListeners.NodesListener;

import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.TreeModelType;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
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

    public static final String POLICY = ", Policy: ";

    public static final String USER_ACCESS_TYPE_ = "user access type ";

    public static final String PROVIDER_ACCESS_TYPE_ = ", provider access type ";

    public static final String INFRASTRUCTURE_FIELD = "Infrastructure";

    public static final String POLICY_FIELD = "Policy";

    public static final String ACCESS_FIELD = "Access";

    public static final String OWNER_FIELD = "Owner";

    public static final String NUMBER_OF_NODES = "#Nodes";

    public static final String NUMBER_OF_BUSY_NODES = "#Busy";

    public static final String PERCENTAGE_OF_BUSY_NODES = "%Busy";

    //specifies the variable name of the node source grid view state in the local storage
    private static final String NS_GRID_VIEW_STATE = "NSGridViewState";

    public static final String NODE_SOURCES = "Node Sources";

    private RMController controller = null;

    /**
     * tree view
     */
    private TreeGrid treeGrid = null;

    /**
     * tree data
     */
    Tree tree = null;

    /**
     * treenodes currently held by {@link #tree}
     */
    Map<String, TreeNode> currentNodes = null;

    /**
     * prevent event cycling
     */
    private boolean ignoreNodeSelectedEvent = false;

    private List<NodeSource> currentNodeSources = new LinkedList<>();

    private class TNode extends TreeNode {
        Node rmNode = null;

        TNode(String name, Node node) {
            super(name);
            super.setAttribute(NODE_SOURCES, name);
            this.rmNode = node;
            this.setAttribute(NODE_ID, node.getNodeUrl());
        }
    }

    private class THost extends TreeNode {
        Host rmHost = null;

        THost(String name, Host h) {
            super(name);
            super.setAttribute(NODE_SOURCES, name);
            this.rmHost = h;
            this.setAttribute(NODE_ID, h.getId());
        }
    }

    private class TNS extends TreeNode {
        NodeSource rmNS = null;

        TNS(String name, NodeSource ns) {
            super(name);
            NodeSourceDisplayedDescription nodeSourceDisplayedDescription = new NodeSourceDisplayedDescription(ns.getSourceDescription());
            super.setAttribute(NODE_SOURCES, name);
            super.setAttribute(INFRASTRUCTURE_FIELD, nodeSourceDisplayedDescription.getInfrastructure());
            super.setAttribute(POLICY_FIELD, nodeSourceDisplayedDescription.getPolicy());
            super.setAttribute(ACCESS_FIELD, nodeSourceDisplayedDescription.getAccess());
            super.setAttribute(OWNER_FIELD, ns.getNodeSourceAdmin());
            this.rmNS = ns;
            this.setAttribute(NODE_ID, ns.getSourceName());

            NodeSourceDisplayedNumberOfNodes nodeSourceDisplayedNumberOfNodes = new NodeSourceDisplayedNumberOfNodes(ns.getHosts()
                                                                                                                       .values());
            super.setAttribute(NUMBER_OF_NODES, nodeSourceDisplayedNumberOfNodes.getNumberOfNodes());
            super.setAttribute(NUMBER_OF_BUSY_NODES, nodeSourceDisplayedNumberOfNodes.getNumberOfBusyNodes());
            super.setAttribute(PERCENTAGE_OF_BUSY_NODES, nodeSourceDisplayedNumberOfNodes.getPercentageOfBusyNodes());

        }
    }

    private static class NodeSourceDisplayedNumberOfNodes {
        private int numberOfNodes = 0;

        private int numberOfBusyNodes = 0;

        private String percentageOfBusyNodes = "0%";

        NodeSourceDisplayedNumberOfNodes(Collection<Host> hosts) {
            List<Node> nodes = new ArrayList<>();
            hosts.forEach(host -> nodes.addAll(host.getNodes().values()));
            if (!nodes.isEmpty()) {
                numberOfNodes = nodes.size();
                numberOfBusyNodes = (int) nodes.stream()
                                               .filter(node -> node.getNodeState().equals(NodeState.BUSY))
                                               .count();
                percentageOfBusyNodes = (numberOfBusyNodes * 100 / nodes.size()) + "%";
            }
        }

        public int getNumberOfNodes() {
            return numberOfNodes;
        }

        public int getNumberOfBusyNodes() {
            return numberOfBusyNodes;
        }

        public String getPercentageOfBusyNodes() {
            return percentageOfBusyNodes;
        }
    }

    private class NodeSourceDisplayedDescription {
        private String infrastructure;

        private String policy;

        private String access;

        NodeSourceDisplayedDescription(String description) {

            infrastructure = description.split(" ")[1];
            policy = description.split(POLICY)[1];
            access = description.split(USER_ACCESS_TYPE_)[1];

            if (infrastructure.contains(",")) {
                infrastructure = infrastructure.substring(0, infrastructure.indexOf(","));
            }
            infrastructure = infrastructure.replace(INFRASTRUCTURE_FIELD, "");
            infrastructure = infrastructure.replace("Manager", "");
            infrastructure = beautifyName(infrastructure);

            policy = policy.substring(0, policy.indexOf(USER_ACCESS_TYPE_));
            policy = policy.replace(POLICY_FIELD, "");
            policy = beautifyName(policy);
            access = access.substring(0, access.indexOf(PROVIDER_ACCESS_TYPE_));
        }

        public String getInfrastructure() {
            return infrastructure;
        }

        public String getPolicy() {
            return policy;
        }

        public String getAccess() {
            return access;
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
        treeGrid.setShowHeader(true);
        treeGrid.setSelectionType(SelectionStyle.SINGLE);

        TreeGridField field = new TreeGridField(NODE_SOURCES);
        field.setCanSort(true);
        field.setSortByDisplayField(true);
        field.setWidth("25%");
        field.setAlign(Alignment.CENTER);

        TreeGridField infrastructureField = new TreeGridField(INFRASTRUCTURE_FIELD);
        infrastructureField.setCanSort(true);
        infrastructureField.setWidth("13%");
        infrastructureField.setAlign(Alignment.CENTER);
        TreeGridField policyField = new TreeGridField(POLICY_FIELD);
        policyField.setCanSort(true);
        policyField.setWidth("13%");
        policyField.setAlign(Alignment.CENTER);
        TreeGridField accessField = new TreeGridField(ACCESS_FIELD);
        accessField.setCanSort(true);
        accessField.setWidth("10%");
        accessField.setAlign(Alignment.CENTER);
        TreeGridField ownerField = new TreeGridField(OWNER_FIELD);
        ownerField.setAlign(Alignment.CENTER);
        ownerField.setCanSort(true);
        ownerField.setWidth("10%");
        TreeGridField numberOfNodesField = new TreeGridField(NUMBER_OF_NODES);
        numberOfNodesField.setAlign(Alignment.CENTER);
        numberOfNodesField.setCanSort(true);
        numberOfNodesField.setWidth("8%");
        TreeGridField numberOfBusyNodesField = new TreeGridField(NUMBER_OF_BUSY_NODES);
        numberOfBusyNodesField.setAlign(Alignment.CENTER);
        numberOfBusyNodesField.setCanSort(true);
        numberOfBusyNodesField.setWidth("8%");
        TreeGridField percentageOfBusyNodesField = new TreeGridField(PERCENTAGE_OF_BUSY_NODES);
        percentageOfBusyNodesField.setAlign(Alignment.CENTER);
        percentageOfBusyNodesField.setCanSort(true);
        percentageOfBusyNodesField.setWidth("8%");
        treeGrid.setFields(field,
                           numberOfNodesField,
                           numberOfBusyNodesField,
                           percentageOfBusyNodesField,
                           infrastructureField,
                           policyField,
                           accessField,
                           ownerField);
        treeGrid.setSortField(NODE_SOURCES);
        treeGrid.setCanDragSelectText(true);

        this.tree = new Tree();
        tree.setModelType(TreeModelType.PARENT);
        tree.setNameProperty(NODE_SOURCES);
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

            Object related = null;
            final TreeNode treeNode = event.getNode();
            if (treeNode instanceof TNode) {
                TNode tn = (TNode) treeNode;
                related = tn.rmNode;
            } else if (treeNode instanceof TNS) {
                TNS tn = (TNS) treeNode;
                related = tn.rmNS;
            } else if (treeNode instanceof THost) {
                THost tn = (THost) treeNode;
                related = tn.rmHost;
            }

            final Menu menu = ContextMenu.createContextMenuFromTreeView(controller, related, tree);

            treeGrid.setContextMenu(menu);
        });

        this.treeGrid.addFieldStateChangedHandler(fieldStateChangedEvent -> {
            //save the view state in the local storage
            Settings.get().setSetting(NS_GRID_VIEW_STATE, this.treeGrid.getViewState());
        });

        this.treeGrid.addDrawHandler(drawEvent -> {
            try {
                final String viewState = Settings.get().getSetting(NS_GRID_VIEW_STATE);
                if (viewState != null) {
                    // restore any previously saved view state for this grid
                    this.treeGrid.setViewState(viewState);
                }
            } catch (Exception e) {
                LogModel.getInstance().logImportantMessage("Failed to restore node source grid view state " + e);
            }
        });

        vl.addMember(treeGrid);
        return vl;
    }

    @Override
    public void updateByDelta(List<NodeSource> nodeSources, List<Node> nodes) {
        processNodeSources(nodeSources, nodes);

        treeGrid.refreshFields();

        processNodes(nodes);

        treeGrid.markForRedraw();
    }

    void processNodes(List<Node> nodes) {
        if (!nodes.isEmpty()) {
            for (Node node : nodes) {
                if (node.isRemoved()) {
                    removeNode(node);
                } else {
                    if (currentNodes.containsKey(node.getSourceName())) {
                        addNodeIfNotExists(node);
                        changeNodeStatusIfChanged(node);
                    }
                }
            }
        }
    }

    void changeNodeStatusIfChanged(Node node) {
        if (node.isChanged()) {
            TNode treeNode = (TNode) currentNodes.get(node.getNodeUrl());
            treeNode.setAttribute("nodeState", node.getNodeState().toString());
            treeNode.rmNode = node;
            treeNode.setIcon(node.getIcon());
            List<NodeSource> nodeSources = currentNodeSources.stream()
                                                             .filter(nodeSource -> nodeSource.getSourceName()
                                                                                             .equals(node.getSourceName()))
                                                             .collect(Collectors.toList());
            if (!nodeSources.isEmpty()) {
                addNodeToNodeSource(Collections.singletonList(node), nodeSources.get(0));
                if (nodeSources.get(0).getHosts() != null &&
                    nodeSources.get(0).getHosts().get(node.getHostName()) != null &&
                    nodeSources.get(0).getHosts().get(node.getHostName()).getNodes() != null) {
                    nodeSources.get(0).getHosts().get(node.getHostName()).getNodes().put(node.getNodeUrl(), node);
                }
                updateNodeSourceDisplayedNumberOfNodesIfChanged(nodeSources.get(0));
            }
        }
    }

    void addNodeIfNotExists(Node node) {
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

    void removeNode(Node node) {
        if (currentNodes.containsKey(node.getNodeUrl())) {
            final TreeNode toRemove = currentNodes.get(node.getNodeUrl());

            final TreeNode parent = tree.getParent(toRemove);
            tree.remove(toRemove);
            currentNodes.remove(node.getNodeUrl());
            currentNodeSources.forEach(ns -> {
                List<Host> hosts = ns.getHosts()
                                     .values()
                                     .stream()
                                     .filter(host -> host.getNodes().containsKey(node.getNodeUrl()))
                                     .collect(Collectors.toList());
                if (!hosts.isEmpty()) {
                    Host h = hosts.get(0);
                    if (h != null) {
                        h.getNodes().remove(node.getNodeUrl());
                    }
                }
            });
            List<NodeSource> filteredNodeSources = currentNodeSources.stream()
                                                                     .filter(nodeSource -> nodeSource.getSourceName()
                                                                                                     .equals(node.getSourceName()))
                                                                     .collect(Collectors.toList());
            if (!filteredNodeSources.isEmpty()) {
                if (filteredNodeSources.get(0).getHosts() != null &&
                    filteredNodeSources.get(0).getHosts().get(node.getHostName()) != null &&
                    filteredNodeSources.get(0).getHosts().get(node.getHostName()).getNodes() != null) {
                    filteredNodeSources.get(0).getHosts().get(node.getHostName()).getNodes().remove(node.getNodeUrl());
                }
                updateNodeSourceDisplayedNumberOfNodesIfChanged(filteredNodeSources.get(0));
            }
            if (!node.isDeployingNode() && !tree.hasChildren(parent)) { // thus this node has a host, which might be removed
                tree.remove(parent);
                currentNodes.remove(parent.getAttribute(NODE_ID));
            }
        }

    }

    void processNodeSources(List<NodeSource> nodeSources, List<Node> nodes) {

        addNodesToNodeSources(nodeSources, nodes);
        if (!nodeSources.isEmpty()) {
            currentNodeSources = new LinkedList<>(nodeSources);
        }
        if (!nodeSources.isEmpty()) {
            for (NodeSource nodeSource : nodeSources) {
                if (nodeSource.isRemoved()) {
                    removeNodeSource(nodeSource);
                } else {
                    addNodeSourceIfNotExists(nodeSource);
                    updateNodeSourceDescriptionIfChanged(nodeSource);
                    updateNodeSourceDisplayedNumberOfNodesIfChanged(nodeSource);
                    changeNodeSourceStatusIfChanged(nodeSource);
                }
            }
        }
    }

    private void addNodesToNodeSources(List<NodeSource> nodeSources, List<Node> nodes) {
        nodeSources.forEach(nodeSource -> {
            List<Node> filteredNodes = nodes.stream()
                                            .filter(node -> node.getSourceName().equals(nodeSource.getSourceName()))
                                            .collect(Collectors.toList());
            addNodeToNodeSource(filteredNodes, nodeSource);
        });
    }

    private void addNodeToNodeSource(List<Node> nodes, NodeSource nodeSource) {
        for (Node node : nodes) {
            // as deploying node
            if (node.isDeployingNode()) {

                nodeSource.getDeploying().put(node.getNodeUrl(), node);

            } else { // as already deployed node
                Host host = nodeSource.getHosts().get(node.getHostName());

                if (host == null) { // create host if there is no host
                    host = new Host(node.getHostName(), node.getSourceName());
                    nodeSource.getHosts().put(node.getHostName(), host);
                }

                host.getNodes().put(node.getNodeUrl(), node);

                if (node.isVirtual()) {
                    host.setVirtual(true);
                }
            }
        }
    }

    void changeNodeSourceStatusIfChanged(NodeSource nodeSource) {
        if (nodeSource.isChanged()) {
            TNS curTreeNodeSource = (TNS) currentNodes.get(nodeSource.getSourceName());
            curTreeNodeSource.rmNS = nodeSource;
            curTreeNodeSource.setName(nodeSource.getSourceName());
            curTreeNodeSource.setIcon(nodeSource.getIcon());
        }
    }

    void addNodeSourceIfNotExists(NodeSource nodeSource) {
        if (!currentNodes.containsKey(nodeSource.getSourceName())) {
            TNS nsTreeNode = new TNS(nodeSource.getSourceName(), nodeSource);
            nsTreeNode.setIcon(nodeSource.getIcon());
            tree.add(nsTreeNode, this.tree.getRoot());
            currentNodes.put(nodeSource.getSourceName(), nsTreeNode);
        }
    }

    private void updateNodeSourceDescriptionIfChanged(NodeSource nodeSource) {
        TNS currentNs = (TNS) currentNodes.get(nodeSource.getSourceName());
        NodeSourceDisplayedDescription nodeSourceDisplayedDescription = new NodeSourceDisplayedDescription(nodeSource.getSourceDescription());

        if (!currentNs.getAttribute(INFRASTRUCTURE_FIELD).equals(nodeSourceDisplayedDescription.getInfrastructure())) {
            currentNs.setAttribute(INFRASTRUCTURE_FIELD, nodeSourceDisplayedDescription.getInfrastructure());
            currentNodes.put(nodeSource.getSourceName(), currentNs);
        }
        if (!currentNs.getAttribute(POLICY_FIELD).equals(nodeSourceDisplayedDescription.getPolicy())) {
            currentNs.setAttribute(POLICY_FIELD, nodeSourceDisplayedDescription.getPolicy());
            currentNodes.put(nodeSource.getSourceName(), currentNs);
        }
        if (!currentNs.getAttribute(ACCESS_FIELD).equals(nodeSourceDisplayedDescription.getAccess())) {
            currentNs.setAttribute(ACCESS_FIELD, nodeSourceDisplayedDescription.getAccess());
            currentNodes.put(nodeSource.getSourceName(), currentNs);
        }

    }

    private void updateNodeSourceDisplayedNumberOfNodesIfChanged(NodeSource nodeSource) {
        TNS currentNs = (TNS) currentNodes.get(nodeSource.getSourceName());
        NodeSourceDisplayedNumberOfNodes nodeSourceDisplayedNumberOfNodes = new NodeSourceDisplayedNumberOfNodes(nodeSource.getHosts()
                                                                                                                           .values());
        if (!currentNs.getAttribute(NUMBER_OF_NODES)
                      .equals(String.valueOf(nodeSourceDisplayedNumberOfNodes.getNumberOfNodes()))) {
            currentNs.setAttribute(NUMBER_OF_NODES,
                                   String.valueOf(nodeSourceDisplayedNumberOfNodes.getNumberOfNodes()));
            currentNodes.put(nodeSource.getSourceName(), currentNs);
        }
        if (!currentNs.getAttribute(NUMBER_OF_BUSY_NODES)
                      .equals(String.valueOf(nodeSourceDisplayedNumberOfNodes.getNumberOfBusyNodes()))) {
            currentNs.setAttribute(NUMBER_OF_BUSY_NODES,
                                   String.valueOf(nodeSourceDisplayedNumberOfNodes.getNumberOfBusyNodes()));
            currentNodes.put(nodeSource.getSourceName(), currentNs);
        }
        if (!currentNs.getAttribute(PERCENTAGE_OF_BUSY_NODES)
                      .equals(String.valueOf(nodeSourceDisplayedNumberOfNodes.getPercentageOfBusyNodes()))) {
            currentNs.setAttribute(PERCENTAGE_OF_BUSY_NODES,
                                   String.valueOf(nodeSourceDisplayedNumberOfNodes.getPercentageOfBusyNodes()));
            currentNodes.put(nodeSource.getSourceName(), currentNs);
        }
    }

    void removeNodeSource(NodeSource nodeSource) {
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
    public void nodeUnselected() {
        this.treeGrid.deselectAllRecords();
    }

    @Override
    public void nodeSourceSelected(NodeSource ns) {
        selectTreeNode(ns.getSourceName());
    }

    @Override
    public void hostSelected(Host h) {
        selectTreeNode(h.getId());
    }

    @Override
    public void nodeSelected(Node node) {
        selectTreeNode(node.getNodeUrl());
    }

    private void selectTreeNode(String id) {
        if (ignoreNodeSelectedEvent) {
            ignoreNodeSelectedEvent = false;
            return;
        }

        treeGrid.deselectAllRecords();
        TreeNode treeNode = currentNodes.get(id);
        treeGrid.selectRecord(treeNode, true);
        scrollList(treeNode);
    }

    private String beautifyName(String name) {
        StringBuffer buffer = new StringBuffer();

        for (int i = 0; i < name.length(); i++) {
            char ch = name.charAt(i);
            if (i == 0) {
                buffer.append(Character.toUpperCase(ch));
            } else if (i > 0 && (Character.isUpperCase(ch) || Character.isDigit(ch))) {
                boolean nextCharIsUpperCase = (i < name.length() - 1) && (Character.isUpperCase(name.charAt(i + 1)) ||
                                                                          Character.isDigit(name.charAt(i + 1)));
                boolean previousCharIsLowerCase = Character.isLowerCase(name.charAt(i - 1));
                if (previousCharIsLowerCase) {
                    buffer.append(" " + ch);
                } else if (!nextCharIsUpperCase) {
                    buffer.append(" " + ch);
                } else {
                    buffer.append(ch);
                }
            } else {
                buffer.append(ch);
            }
        }

        return buffer.toString();
    }
}
