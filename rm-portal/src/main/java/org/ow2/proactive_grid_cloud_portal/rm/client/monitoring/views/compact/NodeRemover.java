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

import java.util.Iterator;
import java.util.Optional;

import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSource;


public class NodeRemover {

    private String nodeUrl;

    private String hostName;

    private String sourceName;

    HierarchyNodeSource hierarchyNodeSource;

    Iterator<HierarchyNodeSource> hierarchyNodeSourceIterator;

    HierarchyHost hierarchyHost;

    Iterator<HierarchyHost> hierarchyHostIterator;

    Iterator<NodeSource.Host.Node> nodeIterator;

    int index = 0;

    CompactFlowPanel compactFlowPanel;

    NodeRemover(CompactFlowPanel compactFlowPanel) {
        this.compactFlowPanel = compactFlowPanel;
    }

    /**
     * Tries to find node source with the same id (sourceName) as given
     * @param ns
     * @return index of nodesoruce if found
     */
    public Optional<Integer> find(NodeSource ns) {
        init(ns);
        if (findNodeSource()) {
            return Optional.of(index);
        } else {
            return Optional.empty();
        }
    }

    /**
     * Tries to find host with the same id (hostName) as given
     * @param host
     * @return index of host if found
     */
    public Optional<Integer> find(Host host) {
        init(host);
        if (findNodeSource() && findHost()) {
            return Optional.of(index);
        } else {
            return Optional.empty();
        }
    }

    /**
     * Tries to find node with the same id (nodeUrl) as given
     * @param node
     * @return index of node if found
     */
    public Optional<Integer> find(Node node) {
        init(node);
        if (findNodeSource() && ((node.isDeployingNode() && findDeploying()) || (findHost() && findNode()))) {
            return Optional.of(index);
        } else {
            return Optional.empty();
        }
    }

    /**
     * Tries to find and remove node source with the same id (sourceName) as given
     * @param nodeSource
     */
    void findAndRemove(NodeSource nodeSource) {
        init(nodeSource);
        if (findNodeSource()) {
            hierarchyNodeSourceIterator.remove();
            while (hierarchyNodeSource.getTilesNumber() > 0) {
                compactFlowPanel.remove(index);
                hierarchyNodeSource.decrementTiles();
            }
        }
    }

    /**
     * Tries to find and remove node with the same id (nodeUrl) as given
     * @param node
     */
    public void findAndRemove(Node node) {
        init(node);
        if (findNodeSource()) {
            if (node.isDeployingNode() && findDeploying()) {
                removeDeployingNode();
            } else {
                if (findHost() && findNode()) {
                    removeNode();

                    // remove dangling host
                    if (hierarchyHost.isDangling()) {
                        removeHost();
                    }

                    return;
                }
            }
        }
    }

    protected void removeHost() {
        hierarchyHostIterator.remove();
        hierarchyNodeSource.decrementTiles();
        compactFlowPanel.remove(index - 1);
    }

    protected void removeNode() {
        nodeIterator.remove();
        hierarchyNodeSource.decrementTiles();
        hierarchyHost.decrementTiles();
        compactFlowPanel.remove(index);
    }

    private void removeDeployingNode() {
        nodeIterator.remove();
        hierarchyNodeSource.decrementTiles();
        compactFlowPanel.remove(index);
    }

    /**
     * Tries to find node source, if find, it sets fields dedicated to node source
     * and make index point to this node source.
     * @return true if node source was found
     */
    boolean findNodeSource() {
        final Iterator<HierarchyNodeSource> nsIterator = compactFlowPanel.getModel().iterator();
        while (nsIterator.hasNext()) {
            final HierarchyNodeSource hierarchyNodeSource = nsIterator.next();
            if (hierarchyNodeSource.getNodeSource().getSourceName().equals(sourceName)) {
                this.hierarchyNodeSource = hierarchyNodeSource;
                this.hierarchyNodeSourceIterator = nsIterator;
                return true;
            } else {
                index += hierarchyNodeSource.getTilesNumber();
            }
        }
        return false;
    }

    /**
     * SHOULD be called after findNodeSource().
     * Tries to find deploying node in the node source found before.
     * @return true if deploying node was found
     */
    private boolean findDeploying() {
        ++index;
        final Iterator<NodeSource.Host.Node> nodeIterator = hierarchyNodeSource.getDeploying().iterator();
        while (nodeIterator.hasNext()) {
            final Node existing = nodeIterator.next();
            if (existing.getNodeUrl().equals(nodeUrl)) {
                this.nodeIterator = nodeIterator;
                return true;
            } else {
                ++index;
            }
        }
        return false;
    }

    /**
     * SHOULD be called after findNodeSource().
     * Tries to find host in the node source found before.
     * @return true if host was found
     */
    boolean findHost() {
        ++index;
        index += hierarchyNodeSource.getDeploying().size();
        final Iterator<HierarchyHost> hostIterator = hierarchyNodeSource.getHosts().iterator();
        while (hostIterator.hasNext()) {
            final HierarchyHost hierarchyHost = hostIterator.next();
            if (hierarchyHost.getHost().getHostName().equals(hostName)) {
                this.hierarchyHost = hierarchyHost;
                this.hierarchyHostIterator = hostIterator;
                return true;
            } else {
                index += hierarchyHost.getTilesNumber();
            }
        }
        return false;
    }

    /**
     * SHOULD be called after findHost().
     * Tries to find node in the host found before.
     * @return true if node was found
     */
    boolean findNode() {
        ++index;
        final Iterator<NodeSource.Host.Node> nodeIterator = hierarchyHost.getNodes().iterator();
        while (nodeIterator.hasNext()) {
            final Node existing = nodeIterator.next();
            if (existing.getNodeUrl().equals(nodeUrl)) {
                this.nodeIterator = nodeIterator;

                return true;
            } else {
                ++index;
            }
        }
        return false;
    }

    /**
     * Reset current state.
     */
    private void cleanState() {
        this.nodeUrl = null;
        this.hostName = null;
        this.sourceName = null;
        this.hierarchyNodeSource = null;
        this.hierarchyNodeSourceIterator = null;
        this.hierarchyHost = null;
        this.hierarchyHostIterator = null;
        this.nodeIterator = null;
        this.index = 0;
    }

    protected void init(Node node) {
        cleanState();
        this.nodeUrl = node.getNodeUrl();
        this.hostName = node.getHostName();
        this.sourceName = node.getSourceName();
    }

    protected void init(Host host) {
        cleanState();
        this.hostName = host.getHostName();
        this.sourceName = host.getSourceName();
    }

    protected void init(NodeSource ns) {
        cleanState();
        this.sourceName = ns.getSourceName();
    }

}
