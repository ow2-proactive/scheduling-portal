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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.ow2.proactive_grid_cloud_portal.common.client.Images;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LogModel;
import org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.EditDynamicParametersWindow;
import org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.EditNodeSourceWindow;

import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.MenuItemSeparator;
import com.smartgwt.client.widgets.tree.Tree;


public class ContextMenu extends Menu {

    private String lockItemImageResource = RMImages.instance.node_add_16_locked().getSafeUri().asString();

    private String unlockItemImageResource = RMImages.instance.node_add_16().getSafeUri().asString();

    private String deployItemImageResource = RMImages.instance.nodesource_deployed().getSafeUri().asString();

    private String undeployItemImageResource = RMImages.instance.nodesource_undeployed().getSafeUri().asString();

    private String editItemImageResource = RMImages.instance.nodesource_edit().getSafeUri().asString();

    private NodeSource nodesource;

    private NodeSource.Host host;

    private NodeSource.Host.Node node;

    public static Menu createContextMenuFromTreeView(RMController controller, Object related, Tree tree) {

        MenuItem expandItem = new MenuItem("Expand all", Images.instance.expand_16().getSafeUri().asString());
        expandItem.addClickHandler(event17 -> tree.openAll());

        MenuItem collapseItem = new MenuItem("Collapse all", Images.instance.close_16().getSafeUri().asString());
        collapseItem.addClickHandler(event16 -> tree.closeAll());

        final Menu menu = createContextMenuFromCompactView(controller, related);

        final List<MenuItem> menuItems = new ArrayList<>();

        menuItems.add(expandItem);
        menuItems.add(collapseItem);
        menuItems.add(new MenuItemSeparator());
        menuItems.addAll(Arrays.asList(menu.getItems()));

        final MenuItem[] newMenuItems = menuItems.toArray(new MenuItem[menuItems.size()]);

        menu.setItems(newMenuItems);

        return menu;
    }

    public static Menu createContextMenuFromCompactView(RMController controller, Object related) {
        ContextMenu menu = new ContextMenu();

        menu.init(related);

        if (menu.node != null) {
            controller.selectNode(menu.node);
            menu.lockItemImageResource = menu.node.getIconLocked();
            menu.unlockItemImageResource = menu.node.getIconUnlocked();
        } else if (menu.host != null) {
            controller.selectHost(menu.host);
        } else if (menu.nodesource != null) {
            controller.selectNodeSource(menu.nodesource);
        }

        menu.setShowShadow(true);
        menu.setShadowDepth(10);

        MenuItem removeItem = new MenuItem("Remove", RMImages.instance.node_remove_16().getSafeUri().asString());
        removeItem.addClickHandler(event15 -> controller.removeNodes());

        MenuItem lockItem = new MenuItem("Lock", menu.lockItemImageResource);
        lockItem.addClickHandler(event14 -> controller.lockNodes());

        MenuItem unlockItem = new MenuItem("Unlock", menu.unlockItemImageResource);
        unlockItem.addClickHandler(event13 -> controller.unlockNodes());

        MenuItem deployItem = new MenuItem("Deploy", menu.deployItemImageResource);
        deployItem.addClickHandler(event12 -> controller.deployNodeSource());

        MenuItem undeployItem = new MenuItem("Undeploy", menu.undeployItemImageResource);
        undeployItem.addClickHandler(event1 -> controller.undeployNodeSource());

        MenuItem editItem = new MenuItem("Edit", menu.editItemImageResource);

        String nodeSourceName = menu.nodesource == null ? "" : menu.nodesource.getSourceName();
        NodeSourceStatus nodeSourceStatus = menu.nodesource == null ? null : menu.nodesource.getNodeSourceStatus();
        editItem.addClickHandler(event1 -> controller.editNodeSource(nodeSourceName, nodeSourceStatus));

        if (menu.node != null) {
            if (menu.node.isLocked()) {
                lockItem.setEnabled(false);
                unlockItem.setEnabled(true);
            } else {
                lockItem.setEnabled(true);
                unlockItem.setEnabled(false);
            }
        }

        if (menu.nodesource != null) {
            switch (menu.nodesource.getNodeSourceStatus()) {
                case NODES_DEPLOYED:
                    editItem.setTitle(EditDynamicParametersWindow.WINDOW_TITLE);
                    menu.enableItems(undeployItem);
                    menu.disableItems(deployItem);
                    break;
                case NODES_UNDEPLOYED:
                    editItem.setTitle(EditNodeSourceWindow.WINDOW_TITLE);
                    menu.enableItems(deployItem);
                    menu.disableItems(undeployItem);
                    break;
                default:
                    menu.disableItems(deployItem, undeployItem, editItem);
            }
        } else {
            menu.disableItems(deployItem, undeployItem, editItem);
        }

        menu.setItems(deployItem, undeployItem, editItem, new MenuItemSeparator(), lockItem, unlockItem, removeItem);

        return menu;
    }

    private void init(Object related) {
        if (related instanceof NodeSource) {
            nodesource = (NodeSource) related;
        } else if (related instanceof NodeSource.Host) {
            host = (NodeSource.Host) related;
        } else if (related instanceof NodeSource.Host.Node) {
            node = (NodeSource.Host.Node) related;
        } else {
            LogModel.getInstance().logCriticalMessage("Cannot show context menu, related object is wrong type: " +
                                                      related.getClass().getCanonicalName());
            throw new RuntimeException("Cannot show context menu, related object is wrong type: " +
                                       related.getClass().getCanonicalName());
        }
    }

    private void disableItems(MenuItem... items) {
        for (MenuItem item : items) {
            item.setEnabled(false);
        }
    }

    private void enableItems(MenuItem... items) {
        for (MenuItem item : items) {
            item.setEnabled(true);
        }
    }
}
