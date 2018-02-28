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

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSource.Host.Node;

import com.google.gwt.resources.client.ImageResource;


/**
 * Hierarchically store Nodes data as received from the REST api
 *
 * @author mschnoor
 */
public class NodeSource {

    /** nodes are grouped per host, each nodesource can hold nodes on different hosts */
    private Map<String, Host> hosts;

    /** currently _deploying_ nodes are not yet on any host */
    private Map<String, Node> deploying;

    /** Unique name of the nodesource */
    private String sourceName;

    /** describes the infrastructure and policy used */
    private String sourceDescription;

    /** login of the user that created the NS */
    private String nodeSourceAdmin;

    /** if the node source is not deployed, it has no node */
    private NodeSourceStatus nodeSourceStatus;

    private String eventType;

    NodeSource(String sourceName, String sourceDescription, String nodeSourceAdmin, String nodeSourceStatus, String eventType) {
        this.sourceName = sourceName;
        this.sourceDescription = sourceDescription;
        this.nodeSourceAdmin = nodeSourceAdmin;
        this.nodeSourceStatus = NodeSourceStatus.getEnum(nodeSourceStatus);
        this.eventType = eventType;

        this.hosts = new HashMap<String, Host>();
        this.deploying = new HashMap<String, Node>();
    }

    NodeSource(NodeSource t) {
        this.sourceDescription = t.sourceDescription;
        this.sourceName = t.sourceName;
        this.nodeSourceAdmin = t.nodeSourceAdmin;
        this.nodeSourceStatus = t.nodeSourceStatus;
        this.eventType = t.eventType;

        Set<String> hostKeys = t.hosts.keySet();
        this.hosts = new HashMap<String, Host>(hostKeys.size());
        for (String hostid : hostKeys) {
            Host h = t.hosts.get(hostid);
            this.hosts.put(hostid, new Host(h));
        }

        Set<String> deployingKeys = t.deploying.keySet();
        this.deploying = new HashMap<String, Node>(deployingKeys.size());
        for (String nodeid : deployingKeys) {
            Node n = t.deploying.get(nodeid);
            this.deploying.put(nodeid, new Node(n));
        }
    }

    public boolean isRemoved() {
        return eventType.equalsIgnoreCase("NODESOURCE_REMOVED");
    }

    public Map<String, Host> getHosts() {
        return hosts;
    }

    public Map<String, Node> getDeploying() {
        return deploying;
    }

    public String getSourceName() {
        return sourceName;
    }

    public String getSourceDescription() {
        return sourceDescription;
    }

    public String getNodeSourceAdmin() {
        return nodeSourceAdmin;
    }

    public NodeSourceStatus getNodeSourceStatus() {
        return nodeSourceStatus;
    }

    public String getIcon() {
        if (nodeSourceStatus.equals(NodeSourceStatus.NODES_DEPLOYED)) {
            return RMImages.instance.nodesource_deployed_16().getSafeUri().asString();
        } else {
            return RMImages.instance.nodesource_undeployed_16().getSafeUri().asString();
        }
    }

    public static class Host {

        /** all nodes deployed on this host for one specific nodesource*/
        private Map<String, Node> nodes;

        /** name of the host ; not unique ! */
        private String hostName;

        /** parent nodesource name */
        private String sourceName;

        /** true if one of the contained nodes contains 'VIRT' in its URL */
        private boolean virtual = false;

        Host(String hostName, String sourceName) {
            this.hostName = hostName;
            this.nodes = new HashMap<String, Node>();
            this.sourceName = sourceName;
        }

        Host(Host t) {
            this.hostName = t.hostName;
            this.sourceName = t.sourceName;
            this.virtual = t.virtual;

            Set<String> nodeKeys = t.nodes.keySet();
            this.nodes = new HashMap<String, Node>(nodeKeys.size());
            for (String nodeid : nodeKeys) {
                Node clone = new Node(t.nodes.get(nodeid));
                this.nodes.put(nodeid, clone);
            }
        }

        public Map<String, Node> getNodes() {
            return nodes;
        }

        public String getHostName() {
            return hostName;
        }

        public String getSourceName() {
            return sourceName;
        }

        public boolean isVirtual() {
            return virtual;
        }

        public void setVirtual(boolean virtual) {
            this.virtual = virtual;
        }

        /**
         * @return a unique ID to differentiate each NS+host couple,
         * 		 since several NS can deploy on the same node
         */
        public String getId() {
            return sourceName + "-host-" + hostName;
        }

        public static class Node {

            /** deployed node URL */
            private String nodeUrl;

            /** current state of the node */
            private NodeState nodeState;

            /** multiline String describing the node */
            private String nodeInfo;

            /** timestamp */
            private long timeStamp;

            /** time when the node changed to this nodeState*/
            private String timeStampFormatted;

            /** user that created the node */
            private String nodeProvider;

            /** user currently using the node */
            private String nodeOwner;

            /** name of the source containing this node */
            private String sourceName;

            /** name of the host containing this node */
            private String hostName;

            /** name of the JVM running this node */
            private String vmName;

            /** toString() of the remote RMNode */
            private String description;

            /** default node JMX url */
            private String defaultJMXUrl;

            /** proactive node JMX url */
            private String proactiveJMXUrl;

            private boolean isLocked = false;

            private long lockTime = -1;

            private String nodeLocker = null;

            private String eventType = null;

            Node(String nodeUrl, String nodeState, String nodeInfo, long timeStamp, String timeStampFormatted,
                    String nodeProvider, String nodeOwner, String sourceName, String hostName, String vmName,
                    String description, String defaultJMXUrl, String proactiveJMXUrl, boolean isLocked, long lockTime,
                    String nodeLocker, String eventType) {

                this.nodeUrl = nodeUrl;
                this.nodeState = NodeState.parse(nodeState);
                this.nodeInfo = nodeInfo;
                this.timeStampFormatted = timeStampFormatted;
                this.nodeProvider = nodeProvider;
                this.nodeOwner = nodeOwner;
                this.sourceName = sourceName;
                this.timeStamp = timeStamp;
                this.hostName = hostName;
                this.vmName = vmName;
                this.description = description;
                this.defaultJMXUrl = defaultJMXUrl;
                this.proactiveJMXUrl = proactiveJMXUrl;

                this.isLocked = isLocked;

                if (this.isLocked) {
                    this.lockTime = lockTime;
                    this.nodeLocker = nodeLocker;
                }
                this.eventType = eventType;
            }

            Node(Node t) {
                this.nodeUrl = t.nodeUrl;
                this.nodeState = t.nodeState;
                this.nodeInfo = t.nodeInfo;
                this.timeStampFormatted = t.timeStampFormatted;
                this.nodeProvider = t.nodeProvider;
                this.nodeOwner = t.nodeOwner;
                this.sourceName = t.sourceName;
                this.timeStamp = t.timeStamp;
                this.hostName = t.hostName;
                this.vmName = t.vmName;
                this.description = t.description;
                this.defaultJMXUrl = t.defaultJMXUrl;
                this.proactiveJMXUrl = t.proactiveJMXUrl;

                this.isLocked = t.isLocked;

                this.eventType = t.eventType;
                if (this.isLocked) {
                    this.lockTime = t.lockTime;
                    this.nodeLocker = t.nodeLocker;
                }
            }

            public boolean isRemoved() {
                return eventType.equalsIgnoreCase("NODE_REMOVED");
            }

            public boolean isDeployingNode() {
                return hostName == null || hostName.length() == 0;
            }

            public String getNodeUrl() {
                return nodeUrl;
            }

            public NodeState getNodeState() {
                return nodeState;
            }

            public String getNodeInfo() {
                return nodeInfo;
            }

            public long getTimeStamp() {
                return timeStamp;
            }

            public String getTimeStampFormatted() {
                return timeStampFormatted;
            }

            public String getNodeProvider() {
                return nodeProvider;
            }

            public String getNodeOwner() {
                return nodeOwner;
            }

            public String getSourceName() {
                return sourceName;
            }

            public String getHostName() {
                return hostName;
            }

            public String getVmName() {
                return vmName;
            }

            public String getDescription() {
                return description;
            }

            public String getDefaultJMXUrl() {
                return defaultJMXUrl;
            }

            public String getProactiveJMXUrl() {
                return proactiveJMXUrl;
            }

            public boolean isLocked() {
                return isLocked;
            }

            public long getLockTime() {
                return lockTime;
            }

            public String getNodeLocker() {
                return nodeLocker;
            }

            public String getIcon() {
                return getIcon(null);
            }

            public String getIconLocked() {
                return getIcon(true);
            }

            public String getIconUnlocked() {
                return getIcon(false);
            }

            public boolean isVirtual() {
                return getNodeUrl().toLowerCase().contains("virt-");
            }

            /**
             * @param version if not {@code null} and {@code true}, returns a locked version of the icon whatever the
             *                real lock of the node is. if not {@code null} and {@code false}, returns a non locked
             *                version of the icon whatever the real lock of the node is. If {@code null}, returns an
             *                icon that is locked or not based on the real status of the node.
             * @return icon name corresponding to the state and lock status of the node.
             */
            private String getIcon(Boolean version) {
                RMImages instance = RMImages.instance;
                switch (nodeState) {
                    case BUSY:
                        return getIcon(instance.node_busy_16(), instance.node_busy_16_locked(), version);
                    case CONFIGURING:
                        return getIcon(instance.node_configuring_16(), instance.node_configuring_16_locked(), version);
                    case DEPLOYING:
                        return getIcon(instance.node_deploying_16(), instance.node_deploying_16_locked(), version);
                    case DOWN:
                        return getIcon(instance.node_down_16(), instance.node_down_16_locked(), version);
                    case FREE:
                        return getIcon(instance.node_free_16(), instance.node_free_16_locked(), version);
                    case LOST:
                        return getIcon(instance.node_lost_16(), instance.node_lost_16_locked(), version);
                    case TO_BE_REMOVED:
                        return getIcon(instance.node_torelease_16(), instance.node_torelease_16_locked(), version);
                }

                return "";
            }

            private String getIcon(ImageResource icon, ImageResource lockedIcon, Boolean lockedVersion) {
                if (lockedVersion != null) {
                    if (lockedVersion) {
                        return getSafeUriString(lockedIcon);
                    } else {
                        return getSafeUriString(icon);
                    }
                }

                if (isLocked) {
                    return getSafeUriString(lockedIcon);
                } else {
                    return getSafeUriString(icon);
                }
            }

            private String getSafeUriString(ImageResource imageResource) {
                return imageResource.getSafeUri().asString();
            }

        }
    }
}
