/*
 * ################################################################
 *
 * ProActive Parallel Suite(TM): The Java(TM) library for
 *    Parallel, Distributed, Multi-Core Computing for
 *    Enterprise Grids & Clouds
 *
 * Copyright (C) 1997-2011 INRIA/University of
 *                 Nice-Sophia Antipolis/ActiveEon
 * Contact: proactive@ow2.org or contact@activeeon.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Affero General Public License
 * as published by the Free Software Foundation; version 3 of
 * the License.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
 * USA
 *
 * If needed, contact us to obtain a release under GPL Version 2 or 3
 * or a different license than the AGPL.
 *
 *  Initial developer(s):               The ProActive Team
 *                        http://proactive.inria.fr/team_members.htm
 *  Contributor(s):
 *
 * ################################################################
 * $$PROACTIVE_INITIAL_DEV$$
 */
package org.ow2.proactive_grid_cloud_portal.rm.client;

import java.util.Map;

import org.ow2.proactive_grid_cloud_portal.rm.client.RMListeners.NodesListener;

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
                    if (Integer.parseInt(num) == 0) {
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
        labelField.setWidth(100);

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

    public void nodesUpdated(Map<String, NodeSource> nodes) {

        ListGridRecord[] r = new ListGridRecord[11];

        ListGridRecord r1 = new ListGridRecord();
        r1.setAttribute("status", "Deploying");
        r1.setAttribute("type", "Node");
        r1.setAttribute("icon", RMImages.instance.node_deploying_16().getSafeUri().asString());
        r1.setAttribute("count", controller.getModel().getNumDeploying());
        r[0] = r1;

        ListGridRecord r2 = new ListGridRecord();
        r2.setAttribute("status", "Lost");
        r2.setAttribute("type", "Node");
        r2.setAttribute("icon", RMImages.instance.node_lost_16().getSafeUri().asString());
        r2.setAttribute("count", controller.getModel().getNumLost());
        r[1] = r2;

        ListGridRecord r3 = new ListGridRecord();
        r3.setAttribute("status", "Configuring");
        r3.setAttribute("type", "Node");
        r3.setAttribute("icon", RMImages.instance.node_configuring_16().getSafeUri().asString());
        r3.setAttribute("count", controller.getModel().getNumConfiguring());
        r[2] = r3;

        ListGridRecord r4 = new ListGridRecord();
        r4.setAttribute("status", "Free");
        r4.setAttribute("type", "Node");
        r4.setAttribute("icon", RMImages.instance.node_free_16().getSafeUri().asString());
        r4.setAttribute("count", controller.getModel().getNumFree());
        r[3] = r4;

        ListGridRecord r5 = new ListGridRecord();
        r5.setAttribute("status", "Locked");
        r5.setAttribute("type", "Node");
        r5.setAttribute("icon", RMImages.instance.node_locked_16().getSafeUri().asString());
        r5.setAttribute("count", controller.getModel().getNumLocked());
        r[4] = r5;

        ListGridRecord r6 = new ListGridRecord();
        r6.setAttribute("status", "Busy");
        r6.setAttribute("type", "Node");
        r6.setAttribute("icon", RMImages.instance.node_busy_16().getSafeUri().asString());
        r6.setAttribute("count", controller.getModel().getNumBusy());
        r[5] = r6;

        ListGridRecord r7 = new ListGridRecord();
        r7.setAttribute("status", "To be released");
        r7.setAttribute("type", "Node");
        r7.setAttribute("icon", RMImages.instance.node_torelease_16().getSafeUri().asString());
        r7.setAttribute("count", controller.getModel().getNumToBeRemoved());
        r[6] = r7;

        ListGridRecord r8 = new ListGridRecord();
        r8.setAttribute("status", "Down");
        r8.setAttribute("type", "Node");
        r8.setAttribute("icon", RMImages.instance.node_down_16().getSafeUri().asString());
        r8.setAttribute("count", controller.getModel().getNumDown());
        r[7] = r8;

        ListGridRecord r9 = new ListGridRecord();
        r9.setAttribute("status", "Total");
        r9.setAttribute("type", "Node");
        r9.setAttribute("count", controller.getModel().getNumNodes());
        r[8] = r9;

        ListGridRecord r10 = new ListGridRecord();
        r10.setAttribute("status", "Physical");
        r10.setAttribute("type", "Host");
        r10.setAttribute("icon", RMImages.instance.host_16().getSafeUri().asString());
        r10.setAttribute("count", controller.getModel().getNumPhysicalHosts());
        r[9] = r10;

        ListGridRecord r11 = new ListGridRecord();
        r11.setAttribute("status", "Virtual");
        r11.setAttribute("type", "Host");
        r11.setAttribute("icon", RMImages.instance.host_virtual_16().getSafeUri().asString());
        r11.setAttribute("count", controller.getModel().getNumVirtualHosts());
        r[10] = r11;

        this.grid.setData(r);
    }

}
