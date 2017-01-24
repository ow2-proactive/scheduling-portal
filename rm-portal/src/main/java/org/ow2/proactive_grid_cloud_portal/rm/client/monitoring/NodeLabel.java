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
package org.ow2.proactive_grid_cloud_portal.rm.client.monitoring;

import java.util.Map;

import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSource;
import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSource.Host.Node;

import com.smartgwt.client.widgets.Label;


/**
 * @author ActiveEon Team
 * @since 19/01/17
 */
public final class NodeLabel {

    private NodeLabel() {
    }

    public static void update(Map<String, NodeSource> nodes, Label nodeLabelToUpdate, Node nodeToUpdate) {
        if (nodeToUpdate != null) {
            update(nodes,
                   nodeLabelToUpdate,
                   nodeToUpdate.getSourceName(),
                   nodeToUpdate.getHostName(),
                   nodeToUpdate.getNodeUrl());
        }
    }

    /**
     * The purpose of this function is to update the specified {@code nodeLabelToUpdate} icon with the new one received
     * from a state refresh available within the specified {@code nodes} data structure. The specified
     * {@code nodeSourceName}, {@code hostName} and {@code nodeUrl} are used to lookup the new icon value
     * from {@code nodes}. If the specified {@code nodeUrl} is {@code null}, no action is performed.
     * 
     * @param nodes new nodes information received.
     * @param nodeSourceName the node source name associated to the node to update.
     * @param hostName the host name associated to the node to update.
     * @param nodeUrl the URL of the node to update.
     * @param nodeLabelToUpdate the label whose the icon must be updated.
     */
    public static void update(Map<String, NodeSource> nodes, Label nodeLabelToUpdate, String nodeSourceName,
            String hostName, String nodeUrl) {

        if (nodeUrl != null) {
            NodeSource nodeSource = nodes.get(nodeSourceName);

            if (nodeSource != null) {
                NodeSource.Host host = nodeSource.getHosts().get(hostName);

                if (host != null) {
                    Node node = host.getNodes().get(nodeUrl);

                    if (node != null) {
                        nodeLabelToUpdate.setIcon(node.getIcon());
                    }
                }
            }
        }
    }

}
