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
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyInt;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.views.compact.CompactFlowPanelTest.host;
import static org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.views.compact.CompactFlowPanelTest.hosts;
import static org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.views.compact.CompactFlowPanelTest.node;
import static org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.views.compact.CompactFlowPanelTest.nodeSource;
import static org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.views.compact.CompactFlowPanelTest.nodeSources;
import static org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.views.compact.CompactFlowPanelTest.nodes;

import java.util.ArrayList;
import java.util.List;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSource;

import com.google.gwt.user.client.ui.Widget;
import com.google.gwtmockito.GwtMockitoTestRunner;


@RunWith(GwtMockitoTestRunner.class)
public class CompactFlowPanelOwnTest {

    private CompactFlowPanelOwn compactFlowPanelOwn;

    @Before
    public void setUp() {
        compactFlowPanelOwn = spy(new CompactFlowPanelOwn());
        /**
         * somehow without this setter Mockito does not see that
         * NodeRemover::findAndRemove(NodeSource) calls
         * CompactFlowPanelTest::remove(int)
         */
        compactFlowPanelOwn.setNodeRemover(new NodeRemover(compactFlowPanelOwn));
    }

    @Test
    public void testRemovingDanglingHostAndNodeSource() {
        for (Tile nodeSourceTile : nodeSources(1)) {
            compactFlowPanelOwn.drawNodeSource(nodeSourceTile);

            final Tile hostTile = hosts(nodeSourceTile.getNodesource().getSourceName(), 1).get(0);
            for (Tile nodeTile : nodes(nodeSourceTile.getNodesource().getSourceName(),
                                       hostTile.getHost().getHostName(),
                                       2)) {
                compactFlowPanelOwn.drawNode(nodeTile, hostTile);
            }
        }
        assertEquals(4, compactFlowPanelOwn.getTilesNumber());
        compactFlowPanelOwn.remove(compactFlowPanelOwn.getModel().get(0).getHosts().get(0).getNodes().get(0));
        assertEquals(3, compactFlowPanelOwn.getTilesNumber());
        compactFlowPanelOwn.remove(compactFlowPanelOwn.getModel().get(0).getHosts().get(0).getNodes().get(0));
        assertEquals(0, compactFlowPanelOwn.getTilesNumber());
        verify(compactFlowPanelOwn, times(4)).remove(anyInt());
    }

    @Test
    public void testNodeRemoval() {
        List<NodeSource.Host.Node> nodes = new ArrayList<>();
        for (Tile nodeSourceTile : nodeSources(5)) {
            compactFlowPanelOwn.drawNodeSource(nodeSourceTile);

            for (Tile hostTile : hosts(nodeSourceTile.getNodesource().getSourceName(), 5)) {
                for (Tile nodeTile : nodes(nodeSourceTile.getNodesource().getSourceName(),
                                           hostTile.getHost().getHostName(),
                                           5)) {
                    nodes.add(nodeTile.getNode());
                    compactFlowPanelOwn.drawNode(nodeTile, hostTile);
                }
            }
        }

        // remove all nodes,
        // dangling hosts also should be removed automatically
        // dangling nodesources also should be removed automatically
        for (NodeSource.Host.Node node : nodes) {
            compactFlowPanelOwn.remove(node);
        }

        long totalCount = 5 * (1 + 5 * (1 + 5));
        verify(compactFlowPanelOwn, times((int) totalCount)).remove(anyInt());

        assertEquals(0, compactFlowPanelOwn.getTilesNumber());

    }

    @Test
    public void testAddANode() {
        final Tile nodeTile = node("SOURCEname", "local", "httpptth");
        final Tile hostTile = host(nodeTile.getNode().getSourceName(), nodeTile.getNode().getHostName());
        final Tile nodeSourceTile = nodeSource(nodeTile.getNode().getSourceName());
        compactFlowPanelOwn.drawNodeSource(nodeSourceTile);

        verify(compactFlowPanelOwn, times(0)).insert(any(Widget.class), anyInt());
        assertEquals(0, compactFlowPanelOwn.getTilesNumber());

        compactFlowPanelOwn.drawNode(nodeTile, hostTile);

        verify(compactFlowPanelOwn, times(1)).insert(eq(nodeSourceTile), eq(0));
        verify(compactFlowPanelOwn, times(1)).insert(eq(hostTile), eq(1));
        verify(compactFlowPanelOwn, times(1)).insert(eq(nodeTile), eq(2));

        assertEquals(3, compactFlowPanelOwn.getTilesNumber());
    }

    @Test(expected = NullPointerException.class)
    public void testAddANode1() {
        final Tile nodeTile = node("SOURCEname", "local", "httpptth");
        final Tile hostTile = host(nodeTile.getNode().getSourceName(), nodeTile.getNode().getHostName());
        compactFlowPanelOwn.drawNode(nodeTile, hostTile);
    }

}
