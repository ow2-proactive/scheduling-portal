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

import static org.ow2.proactive_grid_cloud_portal.rm.client.RMImages.instance;

import java.util.Map;

import org.ow2.proactive_grid_cloud_portal.rm.client.RMListeners.NodesListener;

import com.google.gwt.resources.client.ImageResource;
import com.smartgwt.client.types.GroupStartOpen;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.VLayout;


/**
 * Displays number of nodes per status
 * 
 * 
 * 
 * @author mschnoor
 *
 */
public class StatisticsView implements NodesListener {

    private RMController controller;

    private ListGrid grid = null;

    StatisticsView(RMController controller) {
        this.controller = controller;
        this.controller.getEventDispatcher().addNodesListener(this);
    }

    Canvas build() {
        VLayout root = new VLayout();
        root.setWidth100();
        root.setHeight100();

        this.grid = new ListGrid() {
            @Override
            protected String getCellCSSText(ListGridRecord record, int rowNum, int colNum) {
                String base = super.getCellCSSText(record, rowNum, colNum);
                if (colNum == 2) {
                    String num = record.getAttribute("count");
                    if (num.contentEquals("0")) {
                        return "color:gray;" + base;
                    }
                }
                return base;
            }
        };
        this.grid.setWidth100();
        this.grid.setHeight100();
        this.grid.setCanExpandMultipleRecords(false);
        this.grid.setCanGroupBy(false);
        this.grid.setCanReorderFields(false);
        this.grid.setCanPickFields(false);
        this.grid.setCanFreezeFields(false);

        ListGridField labelField = new ListGridField("status", "Status");
        labelField.setWidth(160);

        ListGridField iconField = new ListGridField("icon", "Caption");
        iconField.setType(ListGridFieldType.IMAGE);
        iconField.setWidth(60);

        ListGridField countField = new ListGridField("count", "Count");

        ListGridField typeField = new ListGridField("type", "Type");
        typeField.setHidden(true);

        this.grid.setFields(labelField, iconField, countField, typeField);
        this.grid.setGroupByField("type");
        this.grid.setGroupStartOpen(GroupStartOpen.ALL);

        root.addMember(this.grid);

        return root;
    }

    private ListGridRecord createListGridRecord(String status, String type, int count, ImageResource icon) {
        ListGridRecord listGridRecord = createListGridRecord(status, type, count);
        listGridRecord.setAttribute("icon", icon.getSafeUri().asString());
        return listGridRecord;
    }

    private ListGridRecord createListGridRecord(String status, String type, int count) {
        ListGridRecord listGridRecord = new ListGridRecord();
        listGridRecord.setAttribute("status", status);
        listGridRecord.setAttribute("type", type);
        listGridRecord.setAttribute("count", count);
        return listGridRecord;
    }

    public void nodesUpdated(Map<String, NodeSource> nodes) {

        ListGridRecord[] r = new ListGridRecord[14];

        int index = 0;

        r[index++] = createListGridRecord("Total", "Nodes", controller.getModel().getNumNodes());
        r[index++] = createListGridRecord("Free", "Nodes", controller.getModel().getNumFree(), instance.node_free_16());
        r[index++] = createListGridRecord("Needed", "Nodes", controller.getModel().getNeededNodes());
        r[index++] = createListGridRecord("Busy", "Nodes", controller.getModel().getNumBusy(), instance.node_busy_16());
        r[index++] = createListGridRecord("To be released",
                                          "Nodes",
                                          controller.getModel().getNumToBeRemoved(),
                                          instance.node_torelease_16());
        r[index++] = createListGridRecord("Deploying",
                                          "Nodes",
                                          controller.getModel().getNumDeploying(),
                                          instance.node_deploying_16());
        r[index++] = createListGridRecord("Configuring",
                                          "Nodes",
                                          controller.getModel().getNumConfiguring(),
                                          instance.node_configuring_16());
        r[index++] = createListGridRecord("Down", "Nodes", controller.getModel().getNumDown(), instance.node_down_16());
        r[index++] = createListGridRecord("Lost", "Nodes", controller.getModel().getNumLost(), instance.node_lost_16());

        ListGridRecord aliveLimit = new ListGridRecord();
        aliveLimit.setAttribute("status", "Node limit");
        aliveLimit.setAttribute("type", "Nodes");
        if (isAliveNodesLimited()) {
            aliveLimit.setAttribute("count", controller.getModel().getMaxNumberOfNodes());
        } else {
            aliveLimit.setAttribute("count", "None");
        }
        r[index++] = aliveLimit;

        r[index++] = createListGridRecord("Nodes locked",
                                          "Nodes",
                                          controller.getModel().getNumLocked(),
                                          instance.padlock());
        int protectedByToken = controller.getModel()
                                         .getNodeSources()
                                         .values()
                                         .stream()
                                         .map(ns -> ns.getHosts()
                                                      .values()
                                                      .stream()
                                                      .map(host -> host.getNodes()
                                                                       .values()
                                                                       .stream()
                                                                       .map(node -> node.isThereRestriction() ? 1
                                                                                                              : 0)
                                                                       .reduce(0,
                                                                               Integer::sum))
                                                      .reduce(0, Integer::sum))
                                         .reduce(0, Integer::sum);

        r[index++] = createListGridRecord("Node with Usage Restriction", "Nodes", protectedByToken);

        r[index++] = createListGridRecord("Physical",
                                          "Hosts",
                                          controller.getModel().getNumPhysicalHosts(),
                                          instance.host_16());
        r[index++] = createListGridRecord("Virtual",
                                          "Hosts",
                                          controller.getModel().getNumVirtualHosts(),
                                          instance.host_virtual_16());

        r[index++] = createListGridRecord("Deployed",
                                          "Node Sources",
                                          controller.getModel().getNumDeployedNodeSources(),
                                          instance.nodesource_deployed());
        r[index++] = createListGridRecord("Undeployed",
                                          "Node Sources",
                                          controller.getModel().getNumUndeployedNodeSources(),
                                          instance.nodesource_undeployed());

        this.grid.setData(r);
    }

    private boolean isAliveNodesLimited() {
        return controller.getModel().getMaxNumberOfNodes() > -1;
    }
}
