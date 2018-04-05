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
package org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.views;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;

import org.ow2.proactive_grid_cloud_portal.common.client.model.LogModel;
import org.ow2.proactive_grid_cloud_portal.rm.client.CompactView;
import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSource;

import com.google.gwt.user.client.ui.FlowPanel;


public class CompactFlowPanel extends FlowPanel {

    private List<HierarchyNodeSource> model = new LinkedList<>();

    public CompactFlowPanel() {
        super();
        this.setWidth("100%");
        // removes the vertical space between lines
        this.getElement().getStyle().setProperty("lineHeight", "0");
    }

    public void drawNodeSource(NodeSource nodeSource, CompactView.Tile nsTile) {
        int index = size();
        this.insert(nsTile, index);
        LogModel.getInstance().logMessage("drawNodeSource " + index);
        model.add(new HierarchyNodeSource(nodeSource));
    }

    private int size() {
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

    public void redrawNode(NodeSource.Host.Node node) {
        int index = indexOf(node).get();
        CompactView.Tile nt = ((CompactView.Tile) this.getWidget(index));
        nt.refresh(node);
    }

    public void drawNode(NodeSource.Host.Node node, CompactView.Tile nodeTile) {
        if(node.isDeployingNode()){
            drawDeployingNode(node, nodeTile);
        } else {
            drawNormalNode(node, nodeTile);
        }
    }

    public void drawNormalNode(NodeSource.Host.Node node, CompactView.Tile nodeTile) {
        int index = 0;
        for (HierarchyNodeSource hierarchyNodeSource : model) {
            if (hierarchyNodeSource.getNodeSource().getSourceName().equals(node.getSourceName())) {
                ++index;
                index += hierarchyNodeSource.getDeploying().size();

                int indexOfFirstHost = index;

                for (HierarchyHost hierarchyHost : hierarchyNodeSource.getHosts()) {
                    if (hierarchyHost.getHost().getHostName().equals(node.getHostName())) {
                        ++index;

                        hierarchyNodeSource.incrementTiles();
                        hierarchyHost.incrementTiles();

                        hierarchyHost.getNodes().add(0, node);

                        this.insert(nodeTile, index);
                        LogModel.getInstance().logMessage("drawNode " + index);
                        return;
                    } else {
                        index += hierarchyHost.getTiles();
                    }
                }

                // if we are here than there is no host for this node yet
                // thus we will add it and node after



            } else {
                index += hierarchyNodeSource.getTiles();
            }
        }
    }

    public void drawDeployingNode(NodeSource.Host.Node node, CompactView.Tile nodeTile) {
        int index = 0;
        for (HierarchyNodeSource hierarchyNodeSource : model) {
            if (hierarchyNodeSource.getNodeSource().getSourceName().equals(node.getSourceName())) {
                ++index;

                hierarchyNodeSource.incrementTiles();

                hierarchyNodeSource.getDeploying().add(0, node);

                this.insert(nodeTile, index);
                return;
            } else {
                index += hierarchyNodeSource.getTiles();
            }
        }
    }

    public void drawHost(NodeSource.Host host, CompactView.Tile hostTile) {
        int index = 0;
        for (HierarchyNodeSource hierarchyNodeSource : model) {
            if (hierarchyNodeSource.getNodeSource().getSourceName().equals(host.getSourceName())) {
                ++index;
                index += hierarchyNodeSource.getDeploying().size();

                hierarchyNodeSource.incrementTiles();

                hierarchyNodeSource.getHosts().add(0, new HierarchyHost(host));

                this.insert(hostTile, index);
                LogModel.getInstance().logMessage("drawHost " + index);
                return;
            } else {
                index += hierarchyNodeSource.getTiles();
            }
        }
    }

    public void removeAllTiles(NodeSource nodeSource) {
        int index = 0;
        final Iterator<HierarchyNodeSource> iterator = model.iterator();
        while (iterator.hasNext()) {
            final HierarchyNodeSource hierarchyNodeSource = iterator.next();
            if (hierarchyNodeSource.getNodeSource().equals(nodeSource)) {
                LogModel.getInstance().logMessage("remove " + hierarchyNodeSource.getTiles() + " from " + index);
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

    public void removeNode(NodeSource.Host.Node node) {
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
                    while(hostIterator.hasNext()){
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
                                    if(hierarchyHost.getNodes().isEmpty()){
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

    public int indexOfHost(NodeSource.Host host) {
        int index = 0;
        for (HierarchyNodeSource hierarchyNodeSource : model) {
            if (hierarchyNodeSource.getNodeSource().getSourceName().equals(host.getSourceName())) {
                ++index;
                index += hierarchyNodeSource.getDeploying().size();
                for (HierarchyHost hierarchyHost : hierarchyNodeSource.getHosts()) {
                    if (hierarchyHost.getHost().getHostName().equals(host.getHostName())) {
                        return index;
                    } else {
                        index += hierarchyHost.getTiles();
                    }
                }

            } else {
                index += hierarchyNodeSource.getTiles();
            }
        }

        return -1;
    }

    public int indexOfNodeSource(NodeSource ns) {
        int index = 0;
        for (HierarchyNodeSource hierarchyNodeSource : model) {
            if (hierarchyNodeSource.getNodeSource().getSourceName().equals(ns.getSourceName())) {
                return index;
            } else {
                index += hierarchyNodeSource.getTiles();
            }
        }
        return -1;
    }

    public int indexOfNode(NodeSource.Host.Node node) {
        int index = 0;
        for (HierarchyNodeSource hierarchyNodeSource : model) {
            if (hierarchyNodeSource.getNodeSource().getSourceName().equals(node.getSourceName())) {
                ++index;
                index += hierarchyNodeSource.getDeploying().size();
                for (HierarchyHost hierarchyHost : hierarchyNodeSource.getHosts()) {
                    if (hierarchyHost.getHost().getHostName().equals(node.getHostName())) {
                        ++index;

                        for (NodeSource.Host.Node existingNode : hierarchyHost.getNodes()) {
                            if (existingNode.getNodeUrl().equals(node.getNodeUrl())) {
                                return index;
                            } else {
                                ++index;
                            }
                        }

                    } else {
                        index += hierarchyHost.getTiles();
                    }
                }

            } else {
                index += hierarchyNodeSource.getTiles();
            }
        }

        return -1;
    }
}

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
