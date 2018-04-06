/*
 * ProActive Parallel Suite(TM):
 * The Open Source library for parallel and distributed
 * Workflows & Scheduling, Orchestration, Cloud Automation
 * and Big Data Analysis on Enterprise Grids & Clouds.
 *
 * Copyright (c) 2007 - 2018 ActiveEon
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
package org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.views;

import java.util.Comparator;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;

import org.ow2.proactive_grid_cloud_portal.rm.client.CompactView;
import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSource;

import com.google.gwt.dev.util.collect.Lists;
import com.google.gwt.user.client.ui.FlowPanel;


/**
 * It is flow panel to draw compact representation of hodesources/hosts/nodes.
 * Underlying stucture (FlowPanel) is kind of ArrayList.
 * However, we have hierarchical structure (model)
 * So to represent model as flat structure we have to be able to identify
 * index of model elements. Where index is the position of item from model in
 * FlowPanel. Plus we should be quite fast in finding indexes (in case we have 20K nodes).
 *
 * Thus, to have at the same time hierarchical structure (model) and
 * be able to find any index of this model we keep always updated tiles field
 * in HierarchyNodeSource and HierarchyHost.
 * For example: we want to update node. For this we need to find index of the tile which represents
 * this node. Initially index is zero. Then we sequentially go through nodesources,
 * hosts, and nodes. Until we find our node, we add to index value of tiles field of HierarchyNodeSource
 * and HierarchyHost, and 1 for the case of Node.
 *
 * Host tiles are ephemeral because CompactFlowPanel handle them by it self.
 * Add soon there is node but there is no dedicated host for this node, host will be created.
 * Also, host will be deleted automatically as soon as it does not have nodes anymore.
 *
 */
public class CompactFlowPanel extends FlowPanel {

    protected List<HierarchyNodeSource> model = new LinkedList<>();

    private CompactView.Tile curSelTile;

    public CompactFlowPanel() {
        super();
        this.setWidth("100%");
        // removes the vertical space between lines
        this.getElement().getStyle().setProperty("lineHeight", "0");
    }

    public CompactView.Tile getCurSelTile() {
        return curSelTile;
    }

    public void setCurSelTile(CompactView.Tile curSelTile) {
        this.curSelTile = curSelTile;
    }

    public void drawNodeSource(CompactView.Tile nsTile) {
        int index = size();
        this.insert(nsTile, index);
        model.add(new HierarchyNodeSource(nsTile.getNodesource()));
        Lists.sort(model, Comparator.comparing(a -> a.getNodeSource().getSourceName()));
    }

    protected int size() {
        return model.stream().map(HierarchyNodeSource::getTiles).reduce((a, b) -> a + b).orElse(0);
    }

    public void redrawNodeSource(NodeSource nodeSource) {
        int index = 0;
        for (HierarchyNodeSource hierarchyNodeSource : model) {
            if (hierarchyNodeSource.getNodeSource().equals(nodeSource)) {
                break;
            } else {
                index += hierarchyNodeSource.getTiles();
            }
        }

        CompactView.Tile nt = ((CompactView.Tile) this.getWidget(index));
        nt.refresh(nodeSource);
    }

    public boolean isNodeSourceDrawn(String sourceName) {
        return model.stream().anyMatch(hierarchyNodeSource -> hierarchyNodeSource.getNodeSource()
                                                                                 .getSourceName()
                                                                                 .equals(sourceName));
    }

    public boolean isNodeDrawn(NodeSource.Host.Node node) {
        return indexOf(node).isPresent();
    }

    public void redrawNode(NodeSource.Host.Node node) {
        int index = indexOf(node).get();
        CompactView.Tile nt = ((CompactView.Tile) this.getWidget(index));
        nt.refresh(node);
    }

    public void drawNode(CompactView.Tile nodeTile, CompactView.Tile hostTile) {
        if (nodeTile.getNode().isDeployingNode()) {
            drawDeployingNode(nodeTile);
        } else {
            drawNormalNode(nodeTile, hostTile);
        }
    }

    protected void drawNormalNode(CompactView.Tile nodeTile, CompactView.Tile hostTile) {
        int index = 0;
        for (HierarchyNodeSource hierarchyNodeSource : model) {
            if (hierarchyNodeSource.getNodeSource().getSourceName().equals(nodeTile.getNode().getSourceName())) {
                ++index;
                index += hierarchyNodeSource.getDeploying().size();

                int indexOfFirstHost = index;

                for (HierarchyHost hierarchyHost : hierarchyNodeSource.getHosts()) {
                    if (hierarchyHost.getHost().getHostName().equals(nodeTile.getNode().getHostName())) {
                        ++index;

                        hierarchyNodeSource.incrementTiles();
                        hierarchyHost.incrementTiles();

                        hierarchyHost.getNodes().add(0, nodeTile.getNode());

                        this.insert(nodeTile, index);
                        return;
                    } else {
                        index += hierarchyHost.getTiles();
                    }
                }

                // if we are here than there is no host for this node yet
                // thus we will add it and node after
                hierarchyNodeSource.incrementTiles();
                final HierarchyHost hierarchyHost = new HierarchyHost(hostTile.getHost());
                hierarchyNodeSource.getHosts().add(0, hierarchyHost);
                this.insert(hostTile, indexOfFirstHost);

                hierarchyNodeSource.incrementTiles();
                hierarchyHost.incrementTiles();
                hierarchyHost.getNodes().add(0, nodeTile.getNode());
                this.insert(nodeTile, indexOfFirstHost + 1);

                return;

            } else {
                index += hierarchyNodeSource.getTiles();
            }
        }
    }

    protected void drawDeployingNode(CompactView.Tile nodeTile) {
        int index = 0;
        for (HierarchyNodeSource hierarchyNodeSource : model) {
            if (hierarchyNodeSource.getNodeSource().getSourceName().equals(nodeTile.getNode().getSourceName())) {
                ++index;

                hierarchyNodeSource.incrementTiles();

                hierarchyNodeSource.getDeploying().add(0, nodeTile.getNode());

                this.insert(nodeTile, index);
                return;
            } else {
                index += hierarchyNodeSource.getTiles();
            }
        }
    }

    public void remove(NodeSource nodeSource) {
        int index = 0;
        final Iterator<HierarchyNodeSource> iterator = model.iterator();
        while (iterator.hasNext()) {
            final HierarchyNodeSource hierarchyNodeSource = iterator.next();
            if (hierarchyNodeSource.getNodeSource().equals(nodeSource)) {
                for (int i = 0; i < hierarchyNodeSource.getTiles(); ++i) {
                    remove(index);
                }
                iterator.remove();
                return;
            } else {
                index += hierarchyNodeSource.getTiles();
            }
        }
    }

    public void remove(NodeSource.Host.Node node) {
        int index = 0;
        for (HierarchyNodeSource hierarchyNodeSource : model) {
            if (hierarchyNodeSource.getNodeSource().getSourceName().equals(node.getSourceName())) {
                ++index;

                if (node.isDeployingNode()) {
                    final Iterator<NodeSource.Host.Node> iterator = hierarchyNodeSource.getDeploying().iterator();
                    while (iterator.hasNext()) {
                        final NodeSource.Host.Node existing = iterator.next();
                        if (existing.equals(node)) {
                            iterator.remove();
                            hierarchyNodeSource.decrementTiles();
                            this.remove(index);
                            return;
                        } else {
                            ++index;
                        }
                    }

                } else {
                    index += hierarchyNodeSource.getDeploying().size();
                    final Iterator<HierarchyHost> hostIterator = hierarchyNodeSource.getHosts().iterator();
                    while (hostIterator.hasNext()) {
                        final HierarchyHost hierarchyHost = hostIterator.next();
                        if (hierarchyHost.getHost().getHostName().equals(node.getHostName())) {
                            ++index;

                            final Iterator<NodeSource.Host.Node> nodeIterator = hierarchyHost.getNodes().iterator();
                            while (nodeIterator.hasNext()) {
                                final NodeSource.Host.Node existing = nodeIterator.next();
                                if (existing.equals(node)) {
                                    nodeIterator.remove();
                                    hierarchyNodeSource.decrementTiles();
                                    hierarchyHost.decrementTiles();
                                    this.remove(index);

                                    // remove dangling host
                                    if (hierarchyHost.getNodes().isEmpty()) {
                                        hostIterator.remove();
                                        hierarchyNodeSource.decrementTiles();
                                        this.remove(index - 1);
                                    }
                                    return;
                                } else {
                                    ++index;
                                }
                            }

                        } else {
                            index += hierarchyHost.getTiles();
                        }
                    }

                }

            } else {
                index += hierarchyNodeSource.getTiles();
            }
        }

    }

    public Optional<Integer> indexOf(NodeSource ns) {
        int index = 0;
        for (HierarchyNodeSource hierarchyNodeSource : model) {
            if (hierarchyNodeSource.getNodeSource().getSourceName().equals(ns.getSourceName())) {
                return Optional.of(index);
            } else {
                index += hierarchyNodeSource.getTiles();
            }
        }
        return Optional.empty();
    }

    public Optional<Integer> indexOf(NodeSource.Host host) {
        int index = 0;
        for (HierarchyNodeSource hierarchyNodeSource : model) {
            if (hierarchyNodeSource.getNodeSource().getSourceName().equals(host.getSourceName())) {
                ++index;
                index += hierarchyNodeSource.getDeploying().size();
                for (HierarchyHost hierarchyHost : hierarchyNodeSource.getHosts()) {
                    if (hierarchyHost.getHost().getHostName().equals(host.getHostName())) {
                        return Optional.of(index);
                    } else {
                        index += hierarchyHost.getTiles();
                    }
                }

            } else {
                index += hierarchyNodeSource.getTiles();
            }
        }

        return Optional.empty();
    }

    public Optional<Integer> indexOf(NodeSource.Host.Node node) {
        int index = 0;
        for (HierarchyNodeSource hierarchyNodeSource : model) {
            if (hierarchyNodeSource.getNodeSource().getSourceName().equals(node.getSourceName())) {
                ++index;
                if (node.isDeployingNode()) {
                    for (NodeSource.Host.Node existingNode : hierarchyNodeSource.getDeploying()) {
                        if (existingNode.equals(node)) {
                            return Optional.of(index);
                        } else {
                            ++index;
                        }
                    }
                } else {
                    index += hierarchyNodeSource.getDeploying().size();
                    for (HierarchyHost hierarchyHost : hierarchyNodeSource.getHosts()) {
                        if (hierarchyHost.getHost().getHostName().equals(node.getHostName())) {
                            ++index;
                            for (NodeSource.Host.Node existingNode : hierarchyHost.getNodes()) {
                                if (existingNode.equals(node)) {
                                    return Optional.of(index);
                                } else {
                                    ++index;
                                }
                            }
                            return Optional.empty();
                        } else {
                            index += hierarchyHost.getTiles();
                        }
                    }

                }
                return Optional.empty();
            } else {
                index += hierarchyNodeSource.getTiles();
            }
        }
        return Optional.empty();
    }

}

/**
 * It is a wrapper around NodeSource
 * It stores deploying nodes and hosts, in lists so their order it preserved.
 * Also it has tiles tiles which represent number of tiles which are need to represent
 * this nodesource with all its content into CompactFlowPanel
 */
class HierarchyNodeSource {
    private NodeSource nodeSource;

    private int tiles = 1;

    private List<HierarchyHost> hosts = new LinkedList<>();

    private List<NodeSource.Host.Node> deploying = new LinkedList<>();

    HierarchyNodeSource(NodeSource nodeSource) {
        this.nodeSource = nodeSource;
    }

    NodeSource getNodeSource() {
        return nodeSource;
    }

    int getTiles() {
        return tiles;
    }

    List<HierarchyHost> getHosts() {
        return hosts;
    }

    List<NodeSource.Host.Node> getDeploying() {
        return deploying;
    }

    void decrementTiles() {
        --tiles;
    }

    void incrementTiles() {
        ++tiles;
    }
}

/**
 * It is a wrapper around Host
 * It stores nodes in list so its order it preserved.
 * Also it has tiles tiles which represent number of tiles which are need to represent
 * this host with all its content into CompactFlowPanel
 */
class HierarchyHost {
    private NodeSource.Host host;

    private int tiles = 1;

    private List<NodeSource.Host.Node> nodes = new LinkedList<>();

    HierarchyHost(NodeSource.Host host) {
        this.host = host;
    }

    NodeSource.Host getHost() {
        return host;
    }

    int getTiles() {
        return tiles;
    }

    List<NodeSource.Host.Node> getNodes() {
        return nodes;
    }

    void decrementTiles() {
        --tiles;
    }

    void incrementTiles() {
        ++tiles;
    }
}
