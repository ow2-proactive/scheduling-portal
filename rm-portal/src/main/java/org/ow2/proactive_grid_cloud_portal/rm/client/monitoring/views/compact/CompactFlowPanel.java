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

import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;

import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSource;

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
 * be able to find any index of this model we keep always updated tilesNumber field
 * in HierarchyNodeSource and HierarchyHost.
 * For example: we want to update node. For this we need to find index of the tile which represents
 * this node. Initially index is zero. Then we sequentially go through nodesources,
 * hosts, and nodes. Until we find our node, we add to index value of tilesNumber field of HierarchyNodeSource
 * and HierarchyHost, and 1 for the case of Node.
 *
 * Host tilesNumber are ephemeral because CompactFlowPanel handle them by it self.
 * As soon there is node but there is no dedicated host for this node, host will be created.
 * Also, host will be deleted automatically as soon as it does not have nodes anymore.
 *
 */
public class CompactFlowPanel extends FlowPanel {

    protected List<HierarchyNodeSource> model = new LinkedList<>();

    private Tile currentSelectedTile;

    private NodeRemover nodeRemover;

    public CompactFlowPanel() {
        super();
        this.setWidth("100%");
        // removes the vertical space between lines
        this.getElement().getStyle().setProperty("lineHeight", "0");
        nodeRemover = new NodeRemover(this);
    }

    public Tile getCurrentSelectedTile() {
        return currentSelectedTile;
    }

    public void setCurrentSelectedTile(Tile currentSelectedTile) {
        this.currentSelectedTile = currentSelectedTile;
    }

    public void drawNodeSource(Tile nsTile) {
        model.add(new HierarchyNodeSource(nsTile.getNodesource()));
        model.sort(Comparator.comparing(a -> a.getNodeSource().getSourceName()));

        this.insert(nsTile, indexOf(nsTile.getNodesource()).get());
    }

    public void redrawNodeSource(NodeSource nodeSource) {
        int index = indexOf(nodeSource).get();

        Tile nodeSourceTile = ((Tile) this.getWidget(index));
        nodeSourceTile.refresh(nodeSource);
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
        Tile nt = ((Tile) this.getWidget(index));
        nt.refresh(node);
    }

    public void drawNode(Tile nodeTile, Tile hostTile) {
        if (nodeTile.getNode().isDeployingNode()) {
            drawDeployingNode(nodeTile);
        } else {
            drawNormalNode(nodeTile, hostTile);
        }
    }

    protected void drawNormalNode(Tile nodeTile, Tile hostTile) {
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
                        index += hierarchyHost.getTilesNumber();
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
                index += hierarchyNodeSource.getTilesNumber();
            }
        }
    }

    protected void drawDeployingNode(Tile nodeTile) {
        int index = 0;
        for (HierarchyNodeSource hierarchyNodeSource : model) {
            if (hierarchyNodeSource.getNodeSource().getSourceName().equals(nodeTile.getNode().getSourceName())) {
                ++index;

                hierarchyNodeSource.incrementTiles();

                hierarchyNodeSource.getDeploying().add(0, nodeTile.getNode());

                this.insert(nodeTile, index);
                return;
            } else {
                index += hierarchyNodeSource.getTilesNumber();
            }
        }
    }

    public void remove(NodeSource nodeSource) {
        nodeRemover.findAndRemove(nodeSource);
    }

    public void remove(NodeSource.Host.Node node) {
        nodeRemover.findAndRemove(node);
    }

    public Optional<Integer> indexOf(NodeSource ns) {
        return nodeRemover.find(ns);
    }

    public Optional<Integer> indexOf(NodeSource.Host host) {
        return nodeRemover.find(host);
    }

    public Optional<Integer> indexOf(NodeSource.Host.Node node) {
        return nodeRemover.find(node);
    }

    public List<HierarchyNodeSource> getModel() {
        return model;
    }

}
