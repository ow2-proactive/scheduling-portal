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

import java.util.List;
import java.util.Map;

import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSource.Host;
import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSource.Host.Node;


/**
 * Contains all the interfaces for event dispatch
 * <p>
 * All in one class to limit the number of files
 *
 * @author mschnoor
 *
 */
public class RMListeners {

    public interface NodesListener {

        /**
         * This method called when any node/nodesource was add/removed/updated
         *
         * @param nodeSources map of nodesources which represents SNAPSHOT of current state, where nodesource stores hosts, host stores nodes.
         */
        default void nodesUpdated(Map<String, NodeSource> nodeSources) {
        }

        /**
         * This method called when any node/nodesource was add/removed/updated
         * However, this method provides only change of state (delta).
         * For example, since latest request to server, one node was removed,
         * than this method will be called with empty nodeSources and list of one node
         * with eventType::NODE_REMOVED
         *
         * For now, only CompactView is update by deltas, all other view take whole SNAPSHOTS of the state.
         *
         * @param nodeSources list of nodesources which represents state delta
         * @param nodes list of nodes which represents state delta
         */
        default void updateByDelta(List<NodeSource> nodeSources, List<Node> nodes) {
        }

    }

    public interface NodeSelectedListener {

        /**
         * Node selection changed
         * 
         * @param node currently selected node
         */
        public void nodeSelected(Node node);

        /**
         * Cancel node selection
         */
        public void nodeUnselected();

        /**
         * Node selection changed,
         * 
         * @param ns currently selected NS 
         */
        public void nodeSourceSelected(NodeSource ns);

        /**
         * Node selection changed
         * 
         * @param h currently selected host
         */
        public void hostSelected(Host h);

    }

}
