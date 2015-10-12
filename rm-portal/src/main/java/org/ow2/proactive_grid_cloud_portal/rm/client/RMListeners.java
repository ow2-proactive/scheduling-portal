/*
 * ################################################################
 *
 * ProActive Parallel Suite(TM): The Java(TM) library for
 *    Parallel, Distributed, Multi-Core Computing for
 *    Enterprise Grids & Clouds
 *
 * Copyright (C) 1997-2015 INRIA/University of
 *                 Nice-Sophia Antipolis/ActiveEon
 * Contact: proactive@ow2.org or contact@activeeon.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Affero General Public License
 * as published by the Free Software Foundation; version 3 of
 * the License.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
 * USA
 *
 * If needed, contact us to obtain a release under GPL Version 2 or 3
 * or a different license than the AGPL.
 *
 *  Initial developer(s):               The ProActive Team
 *                        http://proactive.inria.fr/team_members.htm
 *  Contributor(s):
 *
 * ################################################################
 * $$PROACTIVE_INITIAL_DEV$$
 */
package org.ow2.proactive_grid_cloud_portal.rm.client;

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
         * The list of nodesources, hosts and nodes has changed
         * 
         * @param nodes nodesources, hosts and nodes stored hierarchically
         */
        public void nodesUpdated(Map<String, NodeSource> nodes);

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
