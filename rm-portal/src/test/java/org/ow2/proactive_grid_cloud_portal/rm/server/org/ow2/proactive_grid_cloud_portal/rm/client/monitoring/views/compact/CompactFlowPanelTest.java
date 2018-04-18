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
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.ArgumentCaptor;
import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSource;
import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSourceStatus;
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.views.compact.CompactFlowPanel;
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.views.compact.Tile;

import com.google.gwtmockito.GwtMockitoTestRunner;


@RunWith(GwtMockitoTestRunner.class)
public class CompactFlowPanelTest {

    private CompactFlowPanel spyCompactFlowPanel;

    @Before
    public void setUp() {
        spyCompactFlowPanel = spy(new CompactFlowPanel());
    }

    @Test
    public void test() {
        for (Tile nodeSourceTile : nodeSources(5)) {
            spyCompactFlowPanel.drawNodeSource(nodeSourceTile);

            final Tile hostTile = host(nodeSourceTile.getNodesource().getSourceName());
            for (Tile nodeTile : nodes(nodeSourceTile.getNodesource().getSourceName(), 5)) {
                spyCompactFlowPanel.drawNode(nodeTile, hostTile);
            }

        }

        ArgumentCaptor<Tile> captor = ArgumentCaptor.forClass(Tile.class);
        verify(spyCompactFlowPanel).insert(captor.capture(), eq(0));
        assertNotNull(captor.getValue().getNodesource());
        assertNull(captor.getValue().getHost());

        assertEquals(5, spyCompactFlowPanel.getModel().size());
        assertEquals(7, spyCompactFlowPanel.getModel().get(0).getTilesNumber());

        final NodeSource thirdNodeSource = spyCompactFlowPanel.getModel().get(2).getNodeSource();
        final NodeSource forthNodeSource = spyCompactFlowPanel.getModel().get(3).getNodeSource();
        assertEquals(14, spyCompactFlowPanel.indexOf(thirdNodeSource).get().longValue());
        spyCompactFlowPanel.remove(thirdNodeSource);

        assertEquals(14, spyCompactFlowPanel.indexOf(forthNodeSource).get().longValue());

    }

    @Test
    public void testAddHostByNecessity() {
        assertEquals(0, spyCompactFlowPanel.getTilesNumber());

        spyCompactFlowPanel.drawNodeSource(nodeSources(1).get(0));
        assertEquals(1, spyCompactFlowPanel.getTilesNumber());

        final Tile hostTile = host(spyCompactFlowPanel.getModel().get(0).getNodeSource().getSourceName());

        spyCompactFlowPanel.drawNode(nodes(spyCompactFlowPanel.getModel().get(0).getNodeSource().getSourceName(), 1)
                                                                                                                    .get(0),
                                     hostTile);
        assertEquals(3, spyCompactFlowPanel.getTilesNumber());
    }

    @Test
    public void testRemovingDanglingHost() {
        for (Tile nodeSourceTile : nodeSources(1)) {
            spyCompactFlowPanel.drawNodeSource(nodeSourceTile);

            final Tile hostTile = host(nodeSourceTile.getNodesource().getSourceName());
            for (Tile nodeTile : nodes(nodeSourceTile.getNodesource().getSourceName(), 2)) {
                spyCompactFlowPanel.drawNode(nodeTile, hostTile);
            }
        }
        assertEquals(4, spyCompactFlowPanel.getTilesNumber());
        spyCompactFlowPanel.remove(spyCompactFlowPanel.getModel().get(0).getHosts().get(0).getNodes().get(0));
        assertEquals(3, spyCompactFlowPanel.getTilesNumber());
        spyCompactFlowPanel.remove(spyCompactFlowPanel.getModel().get(0).getHosts().get(0).getNodes().get(0));
        assertEquals(1, spyCompactFlowPanel.getTilesNumber());
    }

    @Test
    public void testIndexOf() {

        for (Tile nodeSourceTile : nodeSources(1)) {
            spyCompactFlowPanel.drawNodeSource(nodeSourceTile);

            Tile hostTile = host(nodeSourceTile.getNodesource().getSourceName());
            for (Tile nodeTile : nodes(nodeSourceTile.getNodesource().getSourceName(), 20000)) {
                spyCompactFlowPanel.drawNode(nodeTile, hostTile);
            }

        }

        final NodeSource.Host.Node node = spyCompactFlowPanel.getModel().get(0).getHosts().get(0).getNodes().get(10000);
        assertEquals(10002, spyCompactFlowPanel.indexOf(node).get().longValue());

    }

    public static Tile host(String sourceName) {
        NodeSource.Host host = new NodeSource.Host("defaultHost", sourceName);
        Tile tile = mock(Tile.class);
        when(tile.getHost()).thenReturn(host);
        return tile;
    }

    public static List<Tile> nodes(String sourceName, int num) {
        return IntStream.range(0, num).mapToObj(i -> {
            NodeSource.Host.Node node = new NodeSource.Host.Node(sourceName, "defaultHost", sourceName + "i" + i);
            Tile tile = mock(Tile.class);
            when(tile.getNode()).thenReturn(node);
            return tile;
        }).collect(Collectors.toList());
    }

    public static List<Tile> nodeSources(int num) {
        return IntStream.range(0, num).mapToObj(i -> {
            final NodeSource nodeSource = new NodeSource("nodeSource" + i);
            nodeSource.setNodeSourceStatus(NodeSourceStatus.NODES_DEPLOYED);
            Tile tile = mock(Tile.class);
            when(tile.getNodesource()).thenReturn(nodeSource);
            return tile;
        }).collect(Collectors.toList());
    }

}
