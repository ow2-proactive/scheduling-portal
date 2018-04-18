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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyInt;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.ArgumentCaptor;
import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSource;
import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSourceStatus;

import com.google.gwt.user.client.ui.Widget;
import com.google.gwtmockito.GwtMockitoTestRunner;


@RunWith(GwtMockitoTestRunner.class)
public class CompactFlowPanelTest {

    private CompactFlowPanel spyCompactFlowPanel;

    @Before
    public void setUp() {
        spyCompactFlowPanel = spy(new CompactFlowPanel());
        /**
         * somehow without this setter Mockito does not see that
         * NodeRemover::findAndRemove(NodeSource) calls
         * CompactFlowPanelTest::remove(int)
         */
        spyCompactFlowPanel.setNodeRemover(new NodeRemover(spyCompactFlowPanel));
    }

    @Test
    public void testRemoveOneNodeSource() {
        for (Tile nodeSourceTile : nodeSources(5)) {
            spyCompactFlowPanel.drawNodeSource(nodeSourceTile);

            final Tile hostTile = hosts(nodeSourceTile.getNodesource().getSourceName(), 1).get(0);
            for (Tile nodeTile : nodes(nodeSourceTile.getNodesource().getSourceName(),
                                       hostTile.getHost().getHostName(),
                                       5)) {
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
    public void testNodeSourceRemoval() {
        for (Tile nodeSourceTile : nodeSources(5)) {
            spyCompactFlowPanel.drawNodeSource(nodeSourceTile);

            for (Tile hostTile : hosts(nodeSourceTile.getNodesource().getSourceName(), 5)) {
                for (Tile nodeTile : nodes(nodeSourceTile.getNodesource().getSourceName(),
                                           hostTile.getHost().getHostName(),
                                           5)) {
                    spyCompactFlowPanel.drawNode(nodeTile, hostTile);
                }
            }
        }

        long totalCount = 5 * (1 + 5 * (1 + 5));
        assertEquals(totalCount, spyCompactFlowPanel.getTilesNumber());
        verify(spyCompactFlowPanel, times((int) totalCount)).insert(any(Widget.class), anyInt());

        spyCompactFlowPanel.remove(spyCompactFlowPanel.getModel().get(0).getNodeSource());
        assertEquals(totalCount - (1 + 5 * (1 + 5)), spyCompactFlowPanel.getTilesNumber());
        verify(spyCompactFlowPanel, times(1 + 5 * (1 + 5))).remove(anyInt());

    }

    @Test
    public void testNodeRemoval() {
        List<NodeSource.Host.Node> nodes = new ArrayList<>();
        for (Tile nodeSourceTile : nodeSources(5)) {
            spyCompactFlowPanel.drawNodeSource(nodeSourceTile);

            for (Tile hostTile : hosts(nodeSourceTile.getNodesource().getSourceName(), 5)) {
                for (Tile nodeTile : nodes(nodeSourceTile.getNodesource().getSourceName(),
                                           hostTile.getHost().getHostName(),
                                           5)) {
                    nodes.add(nodeTile.getNode());
                    spyCompactFlowPanel.drawNode(nodeTile, hostTile);
                }
            }
        }

        // remove all nodes,
        // dangling hosts also should be removed automatically
        for (NodeSource.Host.Node node : nodes) {
            spyCompactFlowPanel.remove(node);
        }

        long totalCount = 5 * (1 + 5 * (1 + 5));
        verify(spyCompactFlowPanel, times((int) (totalCount - 5))).remove(anyInt());
        assertEquals(5, spyCompactFlowPanel.getTilesNumber());

    }

    @Test
    public void testAddHostByNecessity() {
        assertEquals(0, spyCompactFlowPanel.getTilesNumber());

        spyCompactFlowPanel.drawNodeSource(nodeSources(1).get(0));
        assertEquals(1, spyCompactFlowPanel.getTilesNumber());

        final Tile hostTile = hosts(spyCompactFlowPanel.getModel().get(0).getNodeSource().getSourceName(), 1).get(0);

        spyCompactFlowPanel.drawNode(nodes(spyCompactFlowPanel.getModel().get(0).getNodeSource().getSourceName(),
                                           hostTile.getHost().getHostName(),
                                           1).get(0),
                                     hostTile);
        assertEquals(3, spyCompactFlowPanel.getTilesNumber());
    }

    @Test
    public void testRemovingDanglingHost() {
        for (Tile nodeSourceTile : nodeSources(1)) {
            spyCompactFlowPanel.drawNodeSource(nodeSourceTile);

            final Tile hostTile = hosts(nodeSourceTile.getNodesource().getSourceName(), 1).get(0);
            for (Tile nodeTile : nodes(nodeSourceTile.getNodesource().getSourceName(),
                                       hostTile.getHost().getHostName(),
                                       2)) {
                spyCompactFlowPanel.drawNode(nodeTile, hostTile);
            }
        }
        assertEquals(4, spyCompactFlowPanel.getTilesNumber());
        spyCompactFlowPanel.remove(spyCompactFlowPanel.getModel().get(0).getHosts().get(0).getNodes().get(0));
        assertEquals(3, spyCompactFlowPanel.getTilesNumber());
        spyCompactFlowPanel.remove(spyCompactFlowPanel.getModel().get(0).getHosts().get(0).getNodes().get(0));
        verify(spyCompactFlowPanel, times(2)).remove(eq(2));
        verify(spyCompactFlowPanel, times(1)).remove(eq(1));
        assertEquals(1, spyCompactFlowPanel.getTilesNumber());
    }

    @Test
    public void testAddDeployingNodes() {
        for (Tile nodeSourceTile : nodeSources(1)) {
            spyCompactFlowPanel.drawNodeSource(nodeSourceTile);
            for (Tile nodeTile : deployingNodes(nodeSourceTile.getNodesource().getSourceName(), 10)) {
                spyCompactFlowPanel.drawNode(nodeTile, null);
            }
        }

        assertEquals(11, spyCompactFlowPanel.getTilesNumber());
    }

    @Test
    public void testIndexOf() {

        for (Tile nodeSourceTile : nodeSources(1)) {
            spyCompactFlowPanel.drawNodeSource(nodeSourceTile);

            Tile hostTile = hosts(nodeSourceTile.getNodesource().getSourceName(), 1).get(0);
            for (Tile nodeTile : nodes(nodeSourceTile.getNodesource().getSourceName(),
                                       hostTile.getHost().getHostName(),
                                       20000)) {
                spyCompactFlowPanel.drawNode(nodeTile, hostTile);
            }

        }

        final NodeSource.Host.Node node = spyCompactFlowPanel.getModel().get(0).getHosts().get(0).getNodes().get(10000);
        assertEquals(10002, spyCompactFlowPanel.indexOf(node).get().longValue());

    }

    @Test
    public void testAddingOneMoreNode() {

        for (Tile nodeSourceTile : nodeSources(5)) {
            spyCompactFlowPanel.drawNodeSource(nodeSourceTile);

            for (Tile deployingTile : deployingNodes(nodeSourceTile.getNodesource().getSourceName(), 5)) {
                spyCompactFlowPanel.drawNode(deployingTile, null);
            }

            for (Tile hostTile : hosts(nodeSourceTile.getNodesource().getSourceName(), 5)) {

                for (Tile nodeTile : nodes(nodeSourceTile.getNodesource().getSourceName(),
                                           hostTile.getHost().getHostName(),
                                           5)) {
                    spyCompactFlowPanel.drawNode(nodeTile, hostTile);
                }
            }
        }

        long totalCount = 5 * (1 + 5 + 5 * (1 + 5));

        assertEquals(totalCount, spyCompactFlowPanel.getTilesNumber());

        final HierarchyNodeSource hierarchyNodeSource = spyCompactFlowPanel.getModel().get(2);
        final HierarchyHost hierarchyHost = hierarchyNodeSource.getHosts().get(2);
        final Tile nodeTile = node(hierarchyNodeSource.getNodeSource().getSourceName(),
                                   hierarchyHost.getHost().getHostName(),
                                   "newNode");
        final Tile hostTile = host(hierarchyNodeSource.getNodeSource().getSourceName(),
                                   hierarchyHost.getHost().getHostName());

        spyCompactFlowPanel.drawNode(nodeTile, hostTile);

        ArgumentCaptor<Tile> tileCaptor = ArgumentCaptor.forClass(Tile.class);
        //        verify(spyCompactFlowPanel, times(1)).insert(tileCaptor.capture(), eq(12));

        verify(spyCompactFlowPanel, times(1)).insert(tileCaptor.capture(), eq(91));
        assertEquals(nodeTile, tileCaptor.getValue());

    }

    @Test
    public void testIndexOf2() {
        for (Tile nodeSourceTile : nodeSources(1)) {
            spyCompactFlowPanel.drawNodeSource(nodeSourceTile);

            final Tile hostTile = hosts(nodeSourceTile.getNodesource().getSourceName(), 1).get(0);
            for (Tile nodeTile : nodes(nodeSourceTile.getNodesource().getSourceName(),
                                       hostTile.getHost().getHostName(),
                                       1)) {
                spyCompactFlowPanel.drawNode(nodeTile, hostTile);
            }
        }

        final NodeSource.Host.Node aNode = spyCompactFlowPanel.getModel().get(0).getHosts().get(0).getNodes().get(0);
        assertEquals(2, spyCompactFlowPanel.indexOf(aNode).get().longValue());
        spyCompactFlowPanel.remove(aNode);
        assertFalse(spyCompactFlowPanel.indexOf(aNode).isPresent());
    }

    public static List<Tile> nodeSources(int num) {
        return IntStream.range(0, num).mapToObj(i -> {
            return nodeSource("nodeSource" + i);
        }).collect(Collectors.toList());
    }

    public static Tile nodeSource(String sourceName) {
        Tile tile = mock(Tile.class);
        final NodeSource nodeSource = new NodeSource(sourceName);
        nodeSource.setNodeSourceStatus(NodeSourceStatus.NODES_DEPLOYED);
        when(tile.getNodesource()).thenReturn(nodeSource);
        return tile;
    }

    public static List<Tile> hosts(String sourceName, int num) {
        return IntStream.range(0, num).mapToObj(i -> {
            return host(sourceName, "defaultHost" + i);
        }).collect(Collectors.toList());
    }

    public static Tile host(String sourceName, String hostName) {
        NodeSource.Host host = new NodeSource.Host(hostName, sourceName);
        Tile tile = mock(Tile.class);
        when(tile.getHost()).thenReturn(host);
        return tile;
    }

    public static List<Tile> nodes(String sourceName, String hostName, int num) {
        return IntStream.range(0, num).mapToObj(i -> {
            return node(sourceName, hostName, sourceName + hostName + "i" + i);
        }).collect(Collectors.toList());
    }

    public static Tile node(String sourceName, String hostName, String nodeUrl) {
        NodeSource.Host.Node node = new NodeSource.Host.Node(sourceName, hostName, nodeUrl);
        Tile tile = mock(Tile.class);
        when(tile.getNode()).thenReturn(node);
        return tile;
    }

    public static List<Tile> deployingNodes(String sourceName, int num) {
        return IntStream.range(0, num).mapToObj(i -> {
            NodeSource.Host.Node node = new NodeSource.Host.Node(sourceName, null, sourceName + "i" + i);
            Tile tile = mock(Tile.class);
            when(tile.getNode()).thenReturn(node);
            return tile;
        }).collect(Collectors.toList());
    }

}
