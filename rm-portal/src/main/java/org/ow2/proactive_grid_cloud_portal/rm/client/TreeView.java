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
import java.util.Arrays;
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
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.views.compact.CompactView;

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

    public static final String USER_ACCESS_TYPE_ = "userAccessType: ";

    public static final String PROVIDER_ACCESS_TYPE_ = ", providerAccessType: ";

    public static final String INFRASTRUCTURE_FIELD = "Infrastructure";

    public static final String POLICY_FIELD = "Policy";

    public static final String ACCESS_FIELD = "Access";

    public static final String OWNER_FIELD = "Owner";

    public static final String NUMBER_OF_NODES = "#Nodes";

    public static final String NUMBER_OF_DEPLOYING_NODES = "#Depl";

    public static final String NUMBER_OF_BUSY_NODES = "#Busy";

    public static final String PERCENTAGE_OF_BUSY_NODES = "%Busy";

    //specifies the variable name of the node source grid view state in the local storage
    private static final String NS_GRID_VIEW_STATE = "NSGridViewState";

    public static final String NODE_SOURCES = "Node Sources";

    private RMController controller = null;

    private final List<Node> currentNodes;

    private static boolean notEmptyNsView = false;

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
    Map<String, TreeNode> currentTreeNodes;

    /**
     * prevent event cycling
     */
    private boolean ignoreNodeSelectedEvent = false;

    private static final List<NodeSource> currentNodeSources = new LinkedList<>();

    private CompactView compactView;

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
            super.setAttribute(NUMBER_OF_DEPLOYING_NODES, ns.getDeploying().size());

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
            if (infrastructure.equals("AWSEC 2")) {
                infrastructure = "AWS EC2";
            } else if (infrastructure.equals("SSHV 2")) {
                infrastructure = "SSH V2";
            }
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
        this.currentTreeNodes = new HashMap<>();
        this.currentNodes = new LinkedList<>();
    }

    Canvas build(CompactView compactView) {
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
        TreeGridField numberOfDeplNodesField = new TreeGridField(NUMBER_OF_DEPLOYING_NODES);
        numberOfDeplNodesField.setAlign(Alignment.CENTER);
        numberOfDeplNodesField.setCanSort(true);
        numberOfDeplNodesField.setWidth("8%");
        treeGrid.setFields(field,
                           numberOfNodesField,
                           numberOfBusyNodesField,
                           percentageOfBusyNodesField,
                           numberOfDeplNodesField,
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
        this.compactView = compactView;
        treeGrid.addSortChangedHandler(sortEvent -> sortCompactView(compactView, currentNodeSources));
        vl.addMember(treeGrid);
        return vl;
    }

    public void setNotEmptyNsView(boolean value) {
        notEmptyNsView = value;
        if (value) {
            currentNodeSources.stream()
                              .filter(this::isHidden)
                              .collect(Collectors.toList())
                              .forEach(this::hideNodeSource);
            sortCompactView(compactView,
                            currentNodeSources.stream()
                                              .filter(nodeSource -> !nodeSource.getHosts().isEmpty() ||
                                                                    !nodeSource.getDeploying().isEmpty())
                                              .collect(Collectors.toList()));
        } else {
            currentNodeSources.forEach(this::showNodeSource);
            sortCompactView(compactView, currentNodeSources);
        }
    }

    public void sortCompactView(CompactView compactView, List<NodeSource> nodeSources) {
        nodeSources = nodeSources == null ? currentNodeSources : nodeSources;
        compactView.removeNodeSources(currentNodeSources);
        compactView.updateByDelta(nodeSources, currentNodes);
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
                    if (currentTreeNodes.containsKey(node.getSourceName())) {
                        addNodeIfNotExists(node);
                        changeNodeStatusIfChanged(node);
                    }
                }
            }
        }
    }

    void changeNodeStatusIfChanged(Node node) {
        if (node.isChanged()) {
            TNode treeNode = (TNode) currentTreeNodes.get(node.getNodeUrl());
            treeNode.setAttribute("nodeState", node.getNodeState().toString());
            currentNodes.remove(node);
            currentNodes.add(node);
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
            NodeSource nodeSource = getNodeSourceBySourceName(node.getSourceName());
            if (nodeSource != null && node.isDeployingNode() &&
                !nodeSource.getDeploying().containsKey(node.getNodeUrl())) {
                nodeSource.getDeploying().put(node.getNodeUrl(), node);
                updateNodeSourceDisplayedNumberOfNodesIfChanged(nodeSource);
            } else if (nodeSource != null && !node.isDeployingNode()) {
                nodeSource.getDeploying().remove(node.getNodeUrl());
                updateNodeSourceDisplayedNumberOfNodesIfChanged(nodeSource);
            }
        }
    }

    private NodeSource getNodeSourceBySourceName(String sourceName) {
        return currentNodeSources.stream()
                                 .filter(nodeSource -> nodeSource.getSourceName().equals(sourceName))
                                 .findFirst()
                                 .orElse(null);
    }

    void addNodeIfNotExists(Node node) {
        if (!currentTreeNodes.containsKey(node.getNodeUrl())) { // if there is no node
            NodeSource nodeSource = getNodeSourceBySourceName(node.getSourceName());
            if (node.isDeployingNode()) {
                TNode nodeTreeNode = new TNode(node.getNodeUrl(), node);
                nodeTreeNode.setIcon(node.getIcon());
                tree.add(nodeTreeNode, this.currentTreeNodes.get(node.getSourceName()));
                currentTreeNodes.put(node.getNodeUrl(), nodeTreeNode);
                currentNodes.remove(node);
                currentNodes.add(node);
                if (nodeSource != null && !nodeSource.getDeploying().containsKey(node.getNodeUrl())) {
                    nodeSource.getDeploying().put(node.getNodeUrl(), node);
                    updateNodeSourceDisplayedNumberOfNodesIfChanged(nodeSource);
                }
            } else {
                final Host host = new Host(node.getHostName(), node.getSourceName());

                if (!currentTreeNodes.containsKey(host.getId())) { // no host as well
                    // we should add host first
                    THost hostTreeNode = new THost(node.getHostName(), host);
                    if (node.isVirtual()) {
                        hostTreeNode.setIcon(RMImages.instance.host_virtual_16().getSafeUri().asString());
                    } else {
                        hostTreeNode.setIcon(RMImages.instance.host_16().getSafeUri().asString());
                    }

                    tree.add(hostTreeNode, currentTreeNodes.get(node.getSourceName()));
                    currentTreeNodes.put(host.getId(), hostTreeNode);
                    currentNodes.remove(node);
                    currentNodes.add(node);
                }

                TNode nodeTreeNode = new TNode(node.getNodeUrl(), node);
                nodeTreeNode.setAttribute("nodeState", node.getNodeState().toString());
                nodeTreeNode.setIcon(node.getIcon());
                tree.add(nodeTreeNode, currentTreeNodes.get(host.getId()));
                currentTreeNodes.put(node.getNodeUrl(), nodeTreeNode);
                currentNodes.remove(node);
                currentNodes.add(node);
                if (nodeSource != null) {
                    nodeSource.getDeploying().remove(node.getNodeUrl());
                    updateNodeSourceDisplayedNumberOfNodesIfChanged(nodeSource);
                }
            }
        }
    }

    void removeNode(Node node) {
        if (currentTreeNodes.containsKey(node.getNodeUrl())) {
            final TreeNode toRemove = currentTreeNodes.get(node.getNodeUrl());

            final TreeNode parent = tree.getParent(toRemove);
            tree.remove(toRemove);
            currentTreeNodes.remove(node.getNodeUrl());
            currentNodes.remove(node);
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
            NodeSource nodeSource = getNodeSourceBySourceName(node.getSourceName());
            nodeSource.getDeploying().remove(node.getNodeUrl());
            updateNodeSourceDisplayedNumberOfNodesIfChanged(nodeSource);
            if (!node.isDeployingNode() && !tree.hasChildren(parent)) { // thus this node has a host, which might be removed
                tree.remove(parent);
                currentTreeNodes.remove(parent.getAttribute(NODE_ID));
            }
        }

    }

    void processNodeSources(List<NodeSource> nodeSources, List<Node> nodes) {
        addNodesToNodeSources(nodeSources, nodes);
        if (!nodeSources.isEmpty()) {
            for (NodeSource nodeSource : nodeSources) {
                if (nodeSource.isRemoved()) {
                    removeNodeSource(nodeSource);
                } else {
                    if (nodeSource.isShutdown() || nodeSource.isDeployed()) {
                        removeNodeSource(nodeSource);
                    }
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
                if (!nodeSource.getDeploying().containsKey(node.getNodeUrl())) {
                    nodeSource.getDeploying().put(node.getNodeUrl(), node);
                }
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
                nodeSource.getDeploying().remove(node.getNodeUrl());
            }
        }
    }

    void changeNodeSourceStatusIfChanged(NodeSource nodeSource) {
        if (nodeSource.isChanged()) {
            TNS curTreeNodeSource = (TNS) currentTreeNodes.get(nodeSource.getSourceName());
            curTreeNodeSource.rmNS = nodeSource;
            curTreeNodeSource.setName(nodeSource.getSourceName());
            curTreeNodeSource.setIcon(nodeSource.getIcon());
        }
    }

    void addNodeSourceIfNotExists(NodeSource nodeSource) {
        if (!currentTreeNodes.containsKey(nodeSource.getSourceName())) {
            TNS nsTreeNode = new TNS(nodeSource.getSourceName(), nodeSource);
            nsTreeNode.setIcon(nodeSource.getIcon());
            tree.add(nsTreeNode, this.tree.getRoot());
            currentTreeNodes.put(nodeSource.getSourceName(), nsTreeNode);
            currentNodeSources.remove(nodeSource);
            currentNodeSources.add(nodeSource);
        }
    }

    private void updateNodeSourceDescriptionIfChanged(NodeSource nodeSource) {
        TNS currentNs = (TNS) currentTreeNodes.get(nodeSource.getSourceName());
        NodeSourceDisplayedDescription nodeSourceDisplayedDescription = new NodeSourceDisplayedDescription(nodeSource.getSourceDescription());

        if (!currentNs.getAttribute(INFRASTRUCTURE_FIELD).equals(nodeSourceDisplayedDescription.getInfrastructure())) {
            currentNs.setAttribute(INFRASTRUCTURE_FIELD, nodeSourceDisplayedDescription.getInfrastructure());
            currentTreeNodes.put(nodeSource.getSourceName(), currentNs);
            currentNodeSources.remove(nodeSource);
            currentNodeSources.add(nodeSource);
        }
        if (!currentNs.getAttribute(POLICY_FIELD).equals(nodeSourceDisplayedDescription.getPolicy())) {
            currentNs.setAttribute(POLICY_FIELD, nodeSourceDisplayedDescription.getPolicy());
            currentTreeNodes.put(nodeSource.getSourceName(), currentNs);
            currentNodeSources.remove(nodeSource);
            currentNodeSources.add(nodeSource);
        }
        if (!currentNs.getAttribute(ACCESS_FIELD).equals(nodeSourceDisplayedDescription.getAccess())) {
            currentNs.setAttribute(ACCESS_FIELD, nodeSourceDisplayedDescription.getAccess());
            currentTreeNodes.put(nodeSource.getSourceName(), currentNs);
            currentNodeSources.remove(nodeSource);
            currentNodeSources.add(nodeSource);
        }

    }

    private void updateNodeSourceDisplayedNumberOfNodesIfChanged(NodeSource nodeSource) {
        TNS currentNs = (TNS) currentTreeNodes.get(nodeSource.getSourceName());
        NodeSourceDisplayedNumberOfNodes nodeSourceDisplayedNumberOfNodes = new NodeSourceDisplayedNumberOfNodes(nodeSource.getHosts()
                                                                                                                           .values());
        String numberOfNodes = nodeSource.isUndeployed() ? "0"
                                                         : String.valueOf(nodeSourceDisplayedNumberOfNodes.getNumberOfNodes());
        if (!currentNs.getAttribute(NUMBER_OF_NODES).equals(numberOfNodes)) {
            currentNs.setAttribute(NUMBER_OF_NODES, numberOfNodes);
            if (nodeSourceDisplayedNumberOfNodes.getNumberOfNodes() != 0) {
                currentNs.setAttribute(NUMBER_OF_DEPLOYING_NODES, 0);
            }
            currentTreeNodes.put(nodeSource.getSourceName(), currentNs);
            currentNodeSources.remove(nodeSource);
            currentNodeSources.add(nodeSource);
        }
        if (!currentNs.getAttribute(NUMBER_OF_BUSY_NODES)
                      .equals(String.valueOf(nodeSourceDisplayedNumberOfNodes.getNumberOfBusyNodes()))) {
            currentNs.setAttribute(NUMBER_OF_BUSY_NODES,
                                   String.valueOf(nodeSourceDisplayedNumberOfNodes.getNumberOfBusyNodes()));
            currentTreeNodes.put(nodeSource.getSourceName(), currentNs);
            currentNodeSources.remove(nodeSource);
            currentNodeSources.add(nodeSource);
        }
        if (!currentNs.getAttribute(PERCENTAGE_OF_BUSY_NODES)
                      .equals(String.valueOf(nodeSourceDisplayedNumberOfNodes.getPercentageOfBusyNodes()))) {
            currentNs.setAttribute(PERCENTAGE_OF_BUSY_NODES,
                                   String.valueOf(nodeSourceDisplayedNumberOfNodes.getPercentageOfBusyNodes()));
            currentTreeNodes.put(nodeSource.getSourceName(), currentNs);
            currentNodeSources.remove(nodeSource);
            currentNodeSources.add(nodeSource);
        }
        if (!currentNs.getAttribute(NUMBER_OF_DEPLOYING_NODES)
                      .equals(String.valueOf(nodeSource.getDeploying().size())) &&
            currentNs.getAttribute(NUMBER_OF_NODES).equals(String.valueOf(0))) {
            currentNs.setAttribute(NUMBER_OF_DEPLOYING_NODES, String.valueOf(nodeSource.getDeploying().size()));
            currentTreeNodes.put(nodeSource.getSourceName(), currentNs);
            currentNodeSources.remove(nodeSource);
            currentNodeSources.add(nodeSource);
        }
        sortNotEmptyNsView(nodeSource);
    }

    private void sortNotEmptyNsView(NodeSource nodeSource) {
        if (notEmptyNsView && isHidden(nodeSource)) {
            hideNodeSource(nodeSource);
            sortCompactView(compactView,
                            currentNodeSources.stream()
                                              .filter(ns -> !ns.getHosts().isEmpty() || !ns.getDeploying().isEmpty())
                                              .collect(Collectors.toList()));
        } else if (notEmptyNsView) {
            showNodeSource(nodeSource);
            sortCompactView(compactView,
                            currentNodeSources.stream()
                                              .filter(ns -> !ns.getHosts().isEmpty() || !ns.getDeploying().isEmpty())
                                              .collect(Collectors.toList()));
        }
    }

    void removeNodeSource(NodeSource nodeSource) {
        if (currentTreeNodes.containsKey(nodeSource.getSourceName())) {
            final TreeNode treeNodeSource = currentTreeNodes.remove(nodeSource.getSourceName());
            removeCurrentNodeSource(nodeSource);

            for (TreeNode treeNode : tree.getAllNodes(treeNodeSource)) {
                if (treeNode instanceof THost) {
                    final THost tHost = (THost) treeNode;
                    currentTreeNodes.remove(tHost.rmHost.getId());
                    removeCurrentNodeSource(nodeSource);
                } else if (treeNode instanceof TNode) {
                    final TNode tNode = (TNode) treeNode;
                    currentTreeNodes.remove(tNode.rmNode.getNodeUrl());
                    removeCurrentNodeSource(nodeSource);
                }
            }

            tree.remove(treeNodeSource);
            currentTreeNodes.remove(nodeSource.getSourceName());
            removeCurrentNodeSource(nodeSource);
        }
    }

    private void hideNodeSource(NodeSource nodeSource) {
        final TreeNode treeNodeSource = currentTreeNodes.get(nodeSource.getSourceName());
        tree.remove(treeNodeSource);
    }

    void showNodeSource(NodeSource nodeSource) {
        final TreeNode treeNodeSource = currentTreeNodes.get(nodeSource.getSourceName());
        if (Arrays.stream(tree.getDescendants(tree.getRoot()))
                  .noneMatch(treeNode -> treeNode.getAttribute(NODE_SOURCES)
                                                 .equals(treeNodeSource.getAttribute(NODE_SOURCES)))) {
            tree.add(treeNodeSource, this.tree.getRoot());
        }
    }

    public static void undeploy(String nodeSourceName) {
        NodeSource nodeSource = currentNodeSources.stream()
                                                  .filter(nodeSource1 -> nodeSource1.getSourceName()
                                                                                    .equals(nodeSourceName))
                                                  .findFirst()
                                                  .orElse(null);
        if (nodeSource != null) {
            currentNodeSources.remove(nodeSource);
            nodeSource.setNodeSourceStatus(NodeSourceStatus.NODES_UNDEPLOYED);
            nodeSource.resetDeploying();
            currentNodeSources.add(nodeSource);
        }
    }

    private boolean isHidden(NodeSource nodeSource) {
        return currentTreeNodes.containsKey(nodeSource.getSourceName()) &&
               ((nodeSource.getHosts().isEmpty() && nodeSource.getDeploying().isEmpty()) || nodeSource.isUndeployed());
    }

    private void removeCurrentNodeSource(NodeSource nodeSource) {
        List<Node> removesNodes = currentNodes.stream()
                                              .filter(node -> node.getSourceName().equals(nodeSource.getSourceName()))
                                              .collect(Collectors.toList());
        currentNodes.removeAll(removesNodes);
        currentNodeSources.remove(nodeSource);
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
        TreeNode treeNode = currentTreeNodes.get(id);
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

    public TreeGrid getTreeGrid() {
        return treeGrid;
    }
}
