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
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import org.ow2.proactive_grid_cloud_portal.common.client.Listeners.LogListener;
import org.ow2.proactive_grid_cloud_portal.common.client.Listeners.StatsListener;
import org.ow2.proactive_grid_cloud_portal.common.client.Model.StatHistory.Range;
import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSource.Host;
import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSource.Host.Node;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMListeners.NodeSelectedListener;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMListeners.NodesListener;

import com.google.gwt.i18n.client.DateTimeFormat;
import com.google.gwt.i18n.client.DateTimeFormat.PredefinedFormat;


/**
 * Stores local client data,
 * Notifies listeners when data changes
 * <p>
 * This privileged view of the model should only be used directly by the Controller
 * 
 * @author mschnoor
 */
public class RMModelImpl extends RMModel implements RMEventDispatcher {

    private boolean logged = false;

    private String login = null;

    private String sessionId = null;

    private HashMap<String, NodeSource> nodes = null;

    private Node selectedNode = null;

    private Host selectedHost = null;

    private NodeSource selectedNodeSource = null;

    private HashMap<String, PluginDescriptor> infrastructures = null;

    private HashMap<String, PluginDescriptor> policies = null;

    private Map<String, StatHistory> statistics = null;

    private Map<String, Range> requestedStatHistoryRange = null;

    private ArrayList<LogListener> logListeners;

    private ArrayList<NodesListener> nodesListeners;

    private ArrayList<NodeSelectedListener> nodeSelectedListeners;

    private ArrayList<StatsListener> statsListeners;

    private int numDeploying = 0;

    private int numLost = 0;

    private int numConfiguring = 0;

    private int numLocked = 0;

    private int numFree = 0;

    private int numBusy = 0;

    private int numDown = 0;

    private int numToBeRemoved = 0;

    private int maxNumBusy = 0;

    private int maxNumFree = 0;

    private int maxNumDown = 0;

    private int numPhysicalHosts = 0;

    private int numVirtualHosts = 0;

    private long maxNumberOfNodes = -1;

<<<<<<< HEAD
    private int numDeployedNodeSources = 0;

    private int numUndeployedNodeSources = 0;

    private long maxCounter = 0;
=======
    private long maxCounter = -1;
>>>>>>> Does not work

    RMModelImpl() {
        super();

        this.nodes = new HashMap<String, NodeSource>();
        this.infrastructures = new HashMap<String, PluginDescriptor>();
        this.policies = new HashMap<String, PluginDescriptor>();
        this.requestedStatHistoryRange = new HashMap<String, Range>();

        this.logListeners = new ArrayList<LogListener>();
        this.nodesListeners = new ArrayList<NodesListener>();
        this.nodeSelectedListeners = new ArrayList<NodeSelectedListener>();
        this.statsListeners = new ArrayList<StatsListener>();
        this.statistics = new HashMap<String, StatHistory>();
    }

    @Override
    public Map<String, NodeSource> getNodeSources() {
        return this.nodes;
    }

    void setNodes(HashMap<String, NodeSource> nodes) {
        this.nodes = nodes;
        for (NodesListener list : this.nodesListeners) {
            list.nodesUpdated(nodes);
        }
    }

    @Override
    public Node getSelectedNode() {
        return this.selectedNode;
    }

    @Override
    public Host getSelectedHost() {
        return this.selectedHost;
    }

    @Override
    public NodeSource getSelectedNodeSource() {
        return this.selectedNodeSource;
    }

    /*
     * select by id and not direct reference to avoid setting an old value
     */
    void setSelectedNode(String nodeUrl) {
        if (nodeUrl == null) {
            if (this.selectedNode == null) {
                return;
            }
            this.selectedNode = null;
            for (NodeSelectedListener list : this.nodeSelectedListeners) {
                list.nodeUnselected();
            }
        } else {
            if (this.selectedNode != null) {
                if (this.selectedNode.getNodeUrl().equals(nodeUrl)) {
                    return;
                }
            }
            this.selectedNode = null;
            this.selectedHost = null;
            this.selectedNodeSource = null;

            for (NodeSource ns : this.nodes.values()) {
                for (Node n : ns.getDeploying().values()) {
                    if (n.getNodeUrl().equals(nodeUrl)) {
                        this.selectedNode = n;
                    }
                }
                for (Host h : ns.getHosts().values()) {
                    for (Node n : h.getNodes().values()) {
                        if (n.getNodeUrl().equals(nodeUrl))
                            this.selectedNode = n;
                    }
                }
            }
            for (NodeSelectedListener list : this.nodeSelectedListeners) {
                list.nodeSelected(this.selectedNode);
            }
        }
    }

    /*
     * select by id and not direct reference to avoid setting an old value
     */
    void setSelectedHost(String hostId) {
        if (hostId == null) {
            if (this.selectedHost == null) {
                return;
            }
            this.selectedHost = null;
            for (NodeSelectedListener list : this.nodeSelectedListeners) {
                list.nodeUnselected();
            }
        } else {
            if (this.selectedHost != null) {
                if (this.selectedHost.getId().equals(hostId)) {
                    return;
                }
            }
            this.selectedHost = null;
            this.selectedNode = null;
            this.selectedNodeSource = null;

            for (NodeSource ns : this.nodes.values()) {
                for (Host h : ns.getHosts().values()) {
                    if (h.getId().equals(hostId)) {
                        this.selectedHost = h;
                    }
                }
            }
            for (NodeSelectedListener list : this.nodeSelectedListeners) {
                list.hostSelected(this.selectedHost);
            }
        }
    }

    /*
     * select by id and not direct reference to avoid setting an old value
     */
    void setSelectedNodeSource(String nsName) {
        if (nsName == null) {
            if (this.selectedNodeSource == null) {
                return;
            }
            this.selectedNodeSource = null;
            for (NodeSelectedListener list : this.nodeSelectedListeners) {
                list.nodeUnselected();
            }
        } else {
            if (this.selectedNodeSource != null) {
                if (this.selectedNodeSource.getSourceName().equals(nsName)) {
                    return;
                }
            }
            this.selectedNodeSource = null;
            this.selectedHost = null;
            this.selectedNode = null;
            for (NodeSource ns : this.nodes.values()) {
                if (ns.getSourceName().equals(nsName)) {
                    this.selectedNodeSource = ns;
                }
            }
            for (NodeSelectedListener list : this.nodeSelectedListeners) {
                list.nodeSourceSelected(this.selectedNodeSource);
            }
        }
    }

    @Override
    public Map<String, PluginDescriptor> getSupportedInfrastructures() {
        return this.infrastructures;
    }

    void setSupportedInfrastructures(HashMap<String, PluginDescriptor> inf) {
        this.infrastructures = inf;
    }

    @Override
    public Map<String, PluginDescriptor> getSupportedPolicies() {
        return this.policies;
    }

    void setSupportedPolicies(HashMap<String, PluginDescriptor> pol) {
        this.policies = pol;
    }

    @Override
    public StatHistory getStatHistory(String source) {
        return this.statistics.get(source);
    }

    @Override
    public Map<String, StatHistory> getStatHistory() {
        return this.statistics;
    }

    void setStatHistory(Map<String, StatHistory> values) {
        this.statistics = values;
        for (StatsListener list : this.statsListeners) {
            list.statsUpdated(values);
        }
    }

    @Override
    public Range getRequestedStatHistoryRange(String source) {
        Range r = this.requestedStatHistoryRange.get(source);
        if (r == null)
            return Range.MINUTE_10;
        return r;
    }

    void setRequestedStatHistoryRange(String source, Range r) {
        this.requestedStatHistoryRange.put(source, r);
    }

    private String getLogStamp() {
        String date = DateTimeFormat.getFormat(PredefinedFormat.TIME_LONG).format(new Date());
        return "<span style='color:gray'>" + date + "</span> ";
    }

    @Override
    public void addNodesListener(NodesListener listener) {
        this.nodesListeners.add(listener);
    }

    @Override
    public void addNodeSelectedListener(NodeSelectedListener listener) {
        this.nodeSelectedListeners.add(listener);
    }

    @Override
    public void addStatsListener(StatsListener listener) {
        this.statsListeners.add(listener);
    }

    @Override
    public int getNumDeploying() {
        return numDeploying;
    }

    @Override
    public int getNumLost() {
        return numLost;
    }

    @Override
    public int getNumConfiguring() {
        return numConfiguring;
    }

    @Override
    public int getNumFree() {
        return numFree;
    }

    @Override
    public int getMaxNumFree() {
        return Math.max(maxNumFree, numFree);
    }

    @Override
    public int getNumLocked() {
        return numLocked;
    }

    @Override
    public int getNumBusy() {
        return numBusy;
    }

    @Override
    public int getMaxNumBusy() {
        return Math.max(numBusy, maxNumBusy);
    }

    @Override
    public int getNumDown() {
        return numDown;
    }

    @Override
    public int getMaxNumDown() {
        return Math.max(numDown, maxNumDown);
    }

    @Override
    public int getNumToBeRemoved() {
        return this.numToBeRemoved;
    }

    @Override
    public int getNumPhysicalHosts() {
        return this.numPhysicalHosts;
    }

    @Override
    public int getNumVirtualHosts() {
        return this.numVirtualHosts;
    }

    @Override
    public int getNumDeployedNodeSources() {
        return this.numDeployedNodeSources;
    }

    @Override
    public int getNumUndeployedNodeSources() {
        return this.numUndeployedNodeSources;
    }

    @Override
    public int getNumNodes() {
        return numFree + numBusy + numDown;
    }

    void setNumDeploying(int numDeploying) {
        this.numDeploying = numDeploying;
    }

    void setNumLost(int numLost) {
        this.numLost = numLost;
    }

    @Override
    public long getMaxNumberOfNodes() {
        return maxNumberOfNodes;
    }

    public void setMaxNumberOfNodes(long maxNumberOfNodes) {
        this.maxNumberOfNodes = maxNumberOfNodes;
    }

    void setNumConfiguring(int numConfiguring) {
        this.numConfiguring = numConfiguring;
    }

    void setNumFree(int numFree) {
        this.numFree = numFree;
    }

    void setMaxNumFree(int numFree) {
        this.maxNumFree = numFree;
    }

    void setNumLocked(int numLocked) {
        this.numLocked = numLocked;
    }

    void setNumBusy(int numBusy) {
        this.numBusy = numBusy;
    }

    void setMaxNumBusy(int numBusy) {
        this.maxNumBusy = numBusy;
    }

    void setNumDown(int numDown) {
        this.numDown = numDown;
    }

    void setMaxNumDown(int numDown) {
        this.maxNumDown = numDown;
    }

    void setNumToBeRemoved(int numToBeRemoved) {
        this.numToBeRemoved = numToBeRemoved;
    }

    void setNumPhysicalHosts(int num) {
        this.numPhysicalHosts = num;
    }

    void setNumVirtualHosts(int num) {
        this.numVirtualHosts = num;
    }

    void setNumDeployedNodeSources(int num) {
        this.numDeployedNodeSources = num;
    }

    void setNumUndeployedNodeSources(int num) {
        this.numUndeployedNodeSources = num;
    }

    public long getMaxCounter() { return maxCounter; }

    public void setMaxCounter(long maxCounter) { this.maxCounter = maxCounter; }
}

