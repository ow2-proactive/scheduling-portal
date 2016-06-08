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

import org.ow2.proactive_grid_cloud_portal.common.client.Model;
import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSource.Host;
import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSource.Host.Node;


/**
 * Stores and provides public access to all the data stored locally by the client
 * 
 * 
 * @author mschnoor
 *
 */
public abstract class RMModel implements Model {

    /**
     * @return most up to date view of the nodes on the remote RM
     */
    public abstract Map<String, NodeSource> getNodes();

    /**
     * @return current limit of alive nodes.
     */
    public abstract long getMaxNumberOfNodes();

    /**
     * @return node currently selected among all views, or null
     */
    public abstract Node getSelectedNode();

    /**
     * @return host currently selected among all views, or null
     */
    public abstract Host getSelectedHost();

    /**
     * @return nodesource currently selected among all views, or null
     */
    public abstract NodeSource getSelectedNodeSource();

    /**
     * @return a list of supported infrastructure managers and their parameters
     */
    public abstract Map<String, PluginDescriptor> getSupportedInfrastructures();

    /**
     * @return a list of supported policies and their parameters
     */
    public abstract Map<String, PluginDescriptor> getSupportedPolicies();

    /**
     * @return number of deploying nodes
     */
    public abstract int getNumDeploying();

    /**
     * @return number of lost nodes
     */
    public abstract int getNumLost();

    /**
     * @return number of configuring nodes
     */
    public abstract int getNumConfiguring();

    /**
     * @return number of free nodes
     */
    public abstract int getNumFree();

    /**
     * @return max number of free nodes since RM is running
     */
    public abstract int getMaxNumFree();

    /**
     * @return number of locked nodes
     */
    public abstract int getNumLocked();

    /**
     * @return number of busy nodes
     */
    public abstract int getNumBusy();

    /**
     * @return max number of busy nodes since RM is running
     */
    public abstract int getMaxNumBusy();

    /**
     * @return number of down nodes
     */
    public abstract int getNumDown();

    /**
     * @return max number of down nodes since RM is running
     */
    public abstract int getMaxNumDown();

    /**
     * @return number of nodes to be removed
     */
    public abstract int getNumToBeRemoved();

    /**
     * @return number of nodes
     */
    public abstract int getNumNodes();

    /**
     * @return number of physical hosts ; hosts which contain no node that URL matches 'VIRT'
     */
    public abstract int getNumPhysicalHosts();

    /**
     * @return number of physical hosts ; hosts which contain at least one node that URL matches 'VIRT'
     */
    public abstract int getNumVirtualHosts();

}
