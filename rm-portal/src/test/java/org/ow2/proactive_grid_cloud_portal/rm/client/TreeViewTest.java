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

import static org.junit.Assert.assertEquals;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.ArgumentCaptor;

import com.google.gwtmockito.GwtMockitoTestRunner;
import com.smartgwt.client.widgets.tree.Tree;
import com.smartgwt.client.widgets.tree.TreeNode;


@RunWith(GwtMockitoTestRunner.class)
public class TreeViewTest {

    private TreeView treeView;

    @Before
    public void setUp() {
        final RMController rmController = mock(RMController.class);
        when(rmController.getEventDispatcher()).thenReturn(mock(RMEventDispatcher.class));
        treeView = spy(new TreeView(rmController));
        treeView.tree = mock(Tree.class);
    }

    @Test
    public void testRemoveNodeSource() {
        final List<NodeSource> nodeSourceList = IntStream.range(0, 2).mapToObj(i -> {
            final NodeSource nodeSource = new NodeSource("sourceName" + i);
            nodeSource.setNodeSourceStatus(NodeSourceStatus.NODES_DEPLOYED);
            return nodeSource;
        }).collect(Collectors.toList());
        treeView.processNodeSources(nodeSourceList);
        verify(treeView.tree, times(2)).add(any(TreeNode.class), any(TreeNode.class));

        final List<NodeSource.Host.Node> nodeList = IntStream.range(0, 30).mapToObj(i -> {
            final NodeSource.Host.Node node = new NodeSource.Host.Node(nodeSourceList.get(0).getSourceName(),
                                                                       nodeSourceList.get(0).getSourceName() + "host",
                                                                       nodeSourceList.get(0)
                                                                                     .getSourceName() + "host" + i);
            node.setNodeState(NodeState.FREE);
            return node;
        }).collect(Collectors.toList());
        treeView.processNodes(nodeList);

        verify(treeView.tree, times(33)).add(any(TreeNode.class), any(TreeNode.class));

        assertEquals(33, treeView.currentNodes.size());

        final NodeSource nodeSource = nodeSourceList.get(0);
        nodeSource.setEventType("NODESOURCE_REMOVED");

        ArgumentCaptor<TreeNode> captor = ArgumentCaptor.forClass(TreeNode.class);
        verify(treeView.tree, times(33)).add(captor.capture(), any(TreeNode.class));

        assertEquals(33, captor.getAllValues().size());

        final List<TreeNode> allValues = captor.getAllValues();
        allValues.remove(0);
        allValues.remove(0);
        when(treeView.tree.getAllNodes(any(TreeNode.class))).thenReturn(allValues.toArray(new TreeNode[allValues.size()]));

        treeView.processNodeSources(Arrays.asList(nodeSource));

        assertEquals(1, treeView.currentNodes.size());

    }

}
