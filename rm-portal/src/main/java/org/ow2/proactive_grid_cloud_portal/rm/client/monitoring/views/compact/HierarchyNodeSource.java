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

import java.util.LinkedList;
import java.util.List;

import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSource;


/**
 * It is a wrapper around NodeSource
 * It stores deploying nodes and hosts, in lists so their order it preserved.
 * Also it has tilesNumber tilesNumber which represent number of tilesNumber which are need to represent
 * this nodesource with all its content into CompactFlowPanel
 */
public class HierarchyNodeSource {
    private NodeSource nodeSource;

    private int tilesNumber = 1;

    private List<HierarchyHost> hosts = new LinkedList<>();

    private List<NodeSource.Host.Node> deploying = new LinkedList<>();

    HierarchyNodeSource(NodeSource nodeSource) {
        this.nodeSource = nodeSource;
    }

    public NodeSource getNodeSource() {
        return nodeSource;
    }

    public int getTilesNumber() {
        return tilesNumber;
    }

    public List<HierarchyHost> getHosts() {
        return hosts;
    }

    List<NodeSource.Host.Node> getDeploying() {
        return deploying;
    }

    void decrementTiles() {
        --tilesNumber;
    }

    void incrementTiles() {
        ++tilesNumber;
    }

    boolean isDangling() {
        return hosts.isEmpty();
    }
}
