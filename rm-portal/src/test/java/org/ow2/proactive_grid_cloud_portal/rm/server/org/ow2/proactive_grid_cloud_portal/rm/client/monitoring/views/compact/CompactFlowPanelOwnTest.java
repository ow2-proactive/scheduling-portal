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
import static org.mockito.Mockito.spy;
import static org.ow2.proactive_grid_cloud_portal.rm.server.org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.views.compact.CompactFlowPanelTest.host;
import static org.ow2.proactive_grid_cloud_portal.rm.server.org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.views.compact.CompactFlowPanelTest.nodeSources;
import static org.ow2.proactive_grid_cloud_portal.rm.server.org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.views.compact.CompactFlowPanelTest.nodes;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.views.compact.CompactFlowPanelOwn;
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.views.compact.Tile;

import com.google.gwtmockito.GwtMockitoTestRunner;


@RunWith(GwtMockitoTestRunner.class)
public class CompactFlowPanelOwnTest {

    private CompactFlowPanelOwn compactFlowPanel;

    @Before
    public void setUp() {
        compactFlowPanel = new CompactFlowPanelOwn();
    }

    @Test
    public void testRemovingDanglingHostAndNodeSource() {
        final CompactFlowPanelOwn spyCompactFlowPanel = spy(compactFlowPanel);

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
        assertEquals(0, spyCompactFlowPanel.getTilesNumber());
    }

}
