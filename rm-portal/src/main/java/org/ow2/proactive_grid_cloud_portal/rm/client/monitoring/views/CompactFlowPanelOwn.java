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

import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.ow2.proactive_grid_cloud_portal.rm.client.CompactView;
import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSource;

import com.google.gwt.dev.util.collect.Lists;


/**
 * It is flow panel to draw compact representation of hodesources/hosts/nodes
 * where all nodes are nodes own by the current user.
 * In comparing to CompactFlowPanel, this class also treats nodesources as
 * ephemeral items. So whenever CompactView asks to draw nodesource, it is just stored
 * in the nodeSourceTiles. And then if at some point we have a node to draw we will
 * draw nodesource as well.
 */
public class CompactFlowPanelOwn extends CompactFlowPanel {

    private Map<String, CompactView.Tile> nodeSourceTiles = new HashMap<>();

    public CompactFlowPanelOwn() {
        super();
    }

    @Override
    public void drawNodeSource(CompactView.Tile nsTile) {
        nodeSourceTiles.put(nsTile.getNodesource().getSourceName(), nsTile);
    }

    @Override
    public void remove(NodeSource nodeSource) {
        nodeSourceTiles.remove(nodeSource.getSourceName());
        super.remove(nodeSource);
    }

    @Override
    protected void drawDeployingNode(CompactView.Tile nodeTile) {
        // own nodes are nodes that already been deployed and used by user
    }

    @Override
    public void redrawNodeSource(NodeSource nodeSource) {
        nodeSourceTiles.get(nodeSource.getSourceName()).refresh(nodeSource);
        if (isNodeSourceDrawn(nodeSource.getSourceName())) {
            super.redrawNodeSource(nodeSource);
        }
    }

    /**
     * draws a node and if necessary draws nodesource, and if necessary draws host
     * @param nodeTile
     * @param hostTile
     */
    @Override
    protected void drawNormalNode(CompactView.Tile nodeTile, CompactView.Tile hostTile) {
        int index = 0;

        if (!isNodeSourceDrawn(nodeTile.getNode().getSourceName())) {
            final CompactView.Tile nsTile = nodeSourceTiles.get(nodeTile.getNode().getSourceName());
            this.insert(nsTile, size());
            model.add(new HierarchyNodeSource(nsTile.getNodesource()));
            Lists.sort(model, Comparator.comparing(a -> a.getNodeSource().getSourceName()));
        }

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

    /**
     * removes node tile, and dangling hosts and nodesources
     * @param node
     */
    @Override
    public void remove(NodeSource.Host.Node node) {
        int index = 0;
        final Iterator<HierarchyNodeSource> nsIterator = model.iterator();
        while (nsIterator.hasNext()) {
            final HierarchyNodeSource hierarchyNodeSource = nsIterator.next();
            if (hierarchyNodeSource.getNodeSource().getSourceName().equals(node.getSourceName())) {
                ++index;

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

                                // remove dangling nodesource
                                if (hierarchyNodeSource.getHosts().isEmpty()) {
                                    nsIterator.remove();
                                    this.remove(index - 2);
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

            } else {
                index += hierarchyNodeSource.getTiles();
            }
        }
    }
}
