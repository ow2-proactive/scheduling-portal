package org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.views;

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
            return Optional.of(index - 1);
        } else {
            return Optional.empty();
        }
    }

    public Optional<Integer> find(NodeSource.Host host) {
        init(host);
        if(findNodeSource() && findHost()){
            return Optional.of(index - 1);
        } else {
            return Optional.empty();
        }
    }


    void findAndRemove(NodeSource nodeSource){
        init(nodeSource);
        if(findNodeSource()){
            hierarchyNodeSourceIterator.remove();
            compactFlowPanel.remove(index - 1);
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

    protected boolean incrementIndex(){
        ++index;
        return true;
    }

    boolean findNodeSource(){
        final Iterator<HierarchyNodeSource> nsIterator = compactFlowPanel.getModel().iterator();
        while (nsIterator.hasNext()) {
            final HierarchyNodeSource hierarchyNodeSource = nsIterator.next();
            if (hierarchyNodeSource.getNodeSource().getSourceName().equals(sourceName)) {
                ++index;

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
        index += hierarchyNodeSource.getDeploying().size();
        final Iterator<HierarchyHost> hostIterator = hierarchyNodeSource.getHosts().iterator();
        while (hostIterator.hasNext()) {
            final HierarchyHost hierarchyHost = hostIterator.next();
            if (hierarchyHost.getHost().getHostName().equals(hostName)) {
                ++index;

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
        this.nodeUrl = null;
        this.hostName = host.getHostName();
        this.sourceName = host.getSourceName();
    }

    protected void init(NodeSource ns) {
        cleanState();
        this.nodeUrl = null;
        this.hostName = null;
        this.sourceName = ns.getSourceName();
    }


}
