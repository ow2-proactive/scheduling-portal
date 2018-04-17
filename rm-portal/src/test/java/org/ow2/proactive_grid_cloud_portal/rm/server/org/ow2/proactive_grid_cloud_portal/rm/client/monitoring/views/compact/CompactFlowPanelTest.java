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
package org.ow2.proactive_grid_cloud_portal.rm.server.org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.views.compact;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSource;
import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSourceStatus;
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.views.compact.CompactFlowPanel;
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.views.compact.CompactView;
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.views.compact.HierarchyNodeSource;
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.views.compact.Tile;

import com.google.gwtmockito.GwtMockitoTestRunner;


@RunWith(GwtMockitoTestRunner.class)
public class CompactFlowPanelTest {

    private CompactFlowPanel compactFlowPanel;

    @Before
    public void setUp() {
        compactFlowPanel = new CompactFlowPanel();
    }

    @Test
    public void test() {
        for (Tile nodeSourceTile : nodeSources(5)) {
            compactFlowPanel.drawNodeSource(nodeSourceTile);

            final Tile hostTile = host(nodeSourceTile.getNodesource().getSourceName());
            for (Tile nodeTile : nodes(nodeSourceTile.getNodesource().getSourceName(), 5)) {
                compactFlowPanel.drawNode(nodeTile, hostTile);
            }

        }

        assertEquals(5, compactFlowPanel.getModel().size());
        assertEquals(7, compactFlowPanel.getModel().get(0).getTilesNumber());
    }

    private Tile host(String sourceName) {
        NodeSource.Host host = new NodeSource.Host("defaultHost", sourceName);
        Tile tile = mock(Tile.class);
        when(tile.getHost()).thenReturn(host);
        return tile;
    }

    private List<Tile> nodes(String sourceName, int num) {
        return IntStream.range(0, num).mapToObj(i -> {
            NodeSource.Host.Node node = new NodeSource.Host.Node(sourceName, "defaultHost", sourceName + "i" + num);
            Tile tile = mock(Tile.class);
            when(tile.getNode()).thenReturn(node);
            return tile;
        }).collect(Collectors.toList());
    }

    private List<Tile> nodeSources(int num) {
        return IntStream.range(0, num).mapToObj(i -> {
            final NodeSource nodeSource = new NodeSource("nodeSource" + i);
            nodeSource.setNodeSourceStatus(NodeSourceStatus.NODES_DEPLOYED);
            Tile tile = mock(Tile.class);
            when(tile.getNodesource()).thenReturn(nodeSource);
            return tile;
        }).collect(Collectors.toList());
    }

}
