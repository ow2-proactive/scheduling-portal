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

import java.util.Map;

import org.ow2.proactive_grid_cloud_portal.common.client.Model;
import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSource.Host;
import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSource.Host.Node;


/**
 * Stores and provides public access to all the data stored locally by the client
 * 
 * @author mschnoor
 */
public abstract class RMModel implements Model {

    /**
     * @return most up to date view of the node sources on the remote RM
     */
    public abstract Map<String, NodeSource> getNodeSources();

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

    /**
     * @return number of deployed node sources : node sources which acquire their nodes
     */
    public abstract int getNumDeployedNodeSources();

    /**
     * @return number of undeployed node sources : node sources which do not attempt to acquire their nodes
     */
    public abstract int getNumUndeployedNodeSources();

}
