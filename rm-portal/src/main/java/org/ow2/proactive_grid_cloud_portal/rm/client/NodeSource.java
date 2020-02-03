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
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

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

    private HashMap<String, String> additionalInformation = null;

    private String eventType;

    public NodeSource(String sourceName) {
        this.sourceName = sourceName;
    }

    NodeSource(String sourceName, String sourceDescription, HashMap<String, String> additionalInformation,
            String nodeSourceAdmin, String nodeSourceStatus, String eventType) {
        this.sourceName = sourceName;
        this.sourceDescription = sourceDescription;
        this.additionalInformation = new HashMap<>(additionalInformation);
        this.nodeSourceAdmin = nodeSourceAdmin;
        this.nodeSourceStatus = NodeSourceStatus.getEnum(nodeSourceStatus);
        this.eventType = eventType;

        this.hosts = new HashMap<>();
        this.deploying = new HashMap<>();
    }

    NodeSource(NodeSource t) {
        this.sourceDescription = t.sourceDescription;
        this.additionalInformation = new HashMap<>(t.additionalInformation);
        this.sourceName = t.sourceName;
        this.nodeSourceAdmin = t.nodeSourceAdmin;
        this.nodeSourceStatus = t.nodeSourceStatus;
        this.eventType = t.eventType;

        Set<String> hostKeys = t.hosts.keySet();
        this.hosts = new HashMap<>(hostKeys.size());
        for (String hostid : hostKeys) {
            Host h = t.hosts.get(hostid);
            this.hosts.put(hostid, new Host(h));
        }

        Set<String> deployingKeys = t.deploying.keySet();
        this.deploying = new HashMap<>(deployingKeys.size());
        for (String nodeid : deployingKeys) {
            Node n = t.deploying.get(nodeid);
            this.deploying.put(nodeid, new Node(n));
        }
    }

    @Override
    public boolean equals(Object o) {
        if (this == o)
            return true;
        if (o == null || getClass() != o.getClass())
            return false;

        NodeSource that = (NodeSource) o;

        return sourceName != null ? sourceName.equals(that.sourceName) : that.sourceName == null;
    }

    @Override
    public int hashCode() {
        return sourceName != null ? sourceName.hashCode() : 0;
    }

    public boolean isRemoved() {
        return "NODESOURCE_REMOVED".equalsIgnoreCase(eventType);
    }

    public boolean isAdded() {
        return "NODESOURCE_DEFINED".equalsIgnoreCase(eventType);
    }

    public boolean isChanged() {
        return !isAdded() && !isRemoved();
    }

    public String getEventType() {
        return eventType;
    }

    void setEventType(String eventType) {
        this.eventType = eventType;
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

    public void setNodeSourceStatus(NodeSourceStatus nodeSourceStatus) {
        this.nodeSourceStatus = nodeSourceStatus;
    }

    public String getAdditionalInformationsAsString() {
        return this.additionalInformation.keySet()
                                         .stream()
                                         .map(key -> key + ":" + this.additionalInformation.get(key))
                                         .collect(Collectors.joining("\n"));
    }

    public String getIcon() {
        if (nodeSourceStatus.equals(NodeSourceStatus.NODES_DEPLOYED)) {
            return RMImages.instance.nodesource_deployed().getSafeUri().asString();
        } else {
            return RMImages.instance.nodesource_undeployed().getSafeUri().asString();
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

        public Host(String hostName, String sourceName) {
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

        @Override
        public boolean equals(Object o) {
            if (this == o)
                return true;
            if (o == null || getClass() != o.getClass())
                return false;

            Host host = (Host) o;

            return hostName != null ? hostName.equals(host.hostName) : host.hostName == null;
        }

        @Override
        public int hashCode() {
            return hostName != null ? hostName.hashCode() : 0;
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
            return generateId(this.sourceName, this.hostName);
        }

        public static String generateId(String sourceName, String hostName) {
            return sourceName + "-host-" + hostName;
        }

        public String getIcon() {
            return this.isVirtual() ? RMImages.instance.host_virtual_16().getSafeUri().asString()
                                    : RMImages.instance.host_16().getSafeUri().asString();
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

            private Map<String, String> usageInfo;

            private List<String> tokens;

            private String userAccessType = "ALL";

            Node(String nodeUrl, String nodeState, String nodeInfo, long timeStamp, String timeStampFormatted,
                    String nodeProvider, String nodeOwner, String sourceName, String hostName, String vmName,
                    String description, String defaultJMXUrl, String proactiveJMXUrl, boolean isLocked, long lockTime,
                    String nodeLocker, String eventType, Map<String, String> usageInfo, List<String> tokens) {

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
                this.usageInfo = usageInfo;
                this.tokens = tokens;
            }

            public Node(String sourceName, String hostName, String nodeUrl) {
                this.sourceName = sourceName;
                this.hostName = hostName;
                this.nodeUrl = nodeUrl;
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
                this.usageInfo = t.usageInfo;
                this.tokens = t.tokens;
                this.userAccessType = t.userAccessType;
            }

            public boolean isThereRestriction() {
                return !noLocalTokens() || (!onlyTokens() && !isALL());
            }

            public String getUserAccessTypeLocal() {
                String newTokens = "";
                if (!getTokens().isEmpty()) {
                    newTokens = "tokens=" + String.join(",", getTokens());
                }
                if (userAccessType.contains("tokens=")) {
                    return userAccessType.replaceAll("tokens=[^;|]+", newTokens);
                } else if (!newTokens.isEmpty()) {
                    return userAccessType + ";" + newTokens;
                } else {
                    return userAccessType;
                }
            }

            private boolean noLocalTokens() {
                return tokens == null || tokens.isEmpty();
            }

            private boolean isALL() {
                return userAccessType.equals("ALL");
            }

            private boolean onlyTokens() {
                return userAccessType.contains("tokens=") &&
                       (userAccessType.indexOf("=") == userAccessType.lastIndexOf("=")) &&
                       (!userAccessType.contains("|"));
            }

            @Override
            public boolean equals(Object o) {
                if (this == o)
                    return true;
                if (o == null || getClass() != o.getClass())
                    return false;

                Node node = (Node) o;

                return nodeUrl != null ? nodeUrl.equals(node.nodeUrl) : node.nodeUrl == null;
            }

            @Override
            public int hashCode() {
                return nodeUrl != null ? nodeUrl.hashCode() : 0;
            }

            public boolean isRemoved() {
                return "NODE_REMOVED".equalsIgnoreCase(eventType);
            }

            public boolean isAdded() {
                return "NODE_ADDED".equalsIgnoreCase(eventType);
            }

            public boolean isChanged() {
                return !isAdded() && !isRemoved();
            }

            public String getEventType() {
                return eventType;
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

            void setNodeState(NodeState nodeState) {
                this.nodeState = nodeState;
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

            public Map<String, String> getUsageInfo() {
                return usageInfo;
            }

            public List<String> getTokens() {
                return tokens;
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
                        if (!isThereRestriction()) {
                            return getIcon(instance.node_busy_16(), instance.node_busy_16_locked(), version);
                        } else {
                            return getIcon(instance.busy_token(), instance.busy_locked_token(), version);
                        }
                    case CONFIGURING:
                        return getIcon(instance.node_configuring_16(), instance.node_configuring_16_locked(), version);
                    case DEPLOYING:
                        return getIcon(instance.node_deploying_16(), instance.node_deploying_16_locked(), version);
                    case DOWN:
                        return getIcon(instance.node_down_16(), instance.node_down_16_locked(), version);
                    case FREE:
                        if (!isThereRestriction()) {
                            return getIcon(instance.node_free_16(), instance.node_free_16_locked(), version);
                        } else {
                            return getIcon(instance.free_token(), instance.free_locked_token(), version);
                        }
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

            public void setUserAccessType(String userAccessType) {
                this.userAccessType = userAccessType;
            }
        }
    }
}
