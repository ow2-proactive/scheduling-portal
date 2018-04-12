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

import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSource;

import java.util.Iterator;
import java.util.Optional;

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


    public Optional<Integer> find(NodeSource.Host.Node node){
        init(node);
        if(findNodeSource() &&
                ((node.isDeployingNode() && findDeploying())
                        || (findHost() && findNode()))){
            return Optional.of(index);
        } else {
            return Optional.empty();
        }
    }

    public Optional<Integer> find(NodeSource ns) {
        init(ns);
        if(findNodeSource()){
            return Optional.of(index);
        } else {
            return Optional.empty();
        }
    }

    public Optional<Integer> find(NodeSource.Host host) {
        init(host);
        if(findNodeSource() && findHost()){
            return Optional.of(index);
        } else {
            return Optional.empty();
        }
    }


    void findAndRemove(NodeSource nodeSource){
        init(nodeSource);
        if(findNodeSource()){
            hierarchyNodeSourceIterator.remove();
            compactFlowPanel.remove(index);
        }
    }

    public void findAndRemove(NodeSource.Host.Node node) {
        init(node);
        if(findNodeSource()){
            if(node.isDeployingNode()){
                if(findDeploying()){
                    nodeIterator.remove();
                    hierarchyNodeSource.decrementTiles();
                    compactFlowPanel.remove(index);
                }
            } else {
                if(findHost() && findNode()){
                    nodeIterator.remove();
                    hierarchyNodeSource.decrementTiles();
                    hierarchyHost.decrementTiles();
                    compactFlowPanel.remove(index);

                    // remove dangling host
                    if (hierarchyHost.getNodes().isEmpty()) {
                        hierarchyHostIterator.remove();
                        hierarchyNodeSource.decrementTiles();
                        compactFlowPanel.remove(index - 1);
                    }
                    return;
                }
            }
        }
    }

    boolean findNodeSource(){
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

    private boolean findDeploying(){
        ++index;
        final Iterator<NodeSource.Host.Node> nodeIterator = hierarchyNodeSource.getDeploying().iterator();
        while (nodeIterator.hasNext()) {
            final NodeSource.Host.Node existing = nodeIterator.next();
            if (existing.getNodeUrl().equals(nodeUrl)) {
                this.nodeIterator = nodeIterator;
                return true;
            } else {
                ++index;
            }
        }
        return false;
    }

    boolean findHost(){
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

    boolean findNode(){
        ++index;
        final Iterator<NodeSource.Host.Node> nodeIterator = hierarchyHost.getNodes().iterator();
        while (nodeIterator.hasNext()) {
            final NodeSource.Host.Node existing = nodeIterator.next();
            if (existing.getNodeUrl().equals(nodeUrl)) {
                this.nodeIterator = nodeIterator;

                return true;
            } else {
                ++index;
            }
        }
        return false;
    }

    private void cleanState(){
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

    protected void init(NodeSource.Host.Node node){
        cleanState();
        this.nodeUrl = node.getNodeUrl();
        this.hostName = node.getHostName();
        this.sourceName = node.getSourceName();
    }

    protected void init(NodeSource.Host host) {
        cleanState();
        this.hostName = host.getHostName();
        this.sourceName = host.getSourceName();
    }

    protected void init(NodeSource ns) {
        cleanState();
        this.sourceName = ns.getSourceName();
    }


}
