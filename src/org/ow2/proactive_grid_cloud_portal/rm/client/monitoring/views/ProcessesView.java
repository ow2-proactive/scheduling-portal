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
package org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.views;

import java.util.ArrayList;
import java.util.List;

import org.ow2.proactive_grid_cloud_portal.rm.client.RMController;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMModel;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMServiceAsync;
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.Reloadable;

import com.google.gwt.json.client.JSONArray;
import com.google.gwt.json.client.JSONObject;
import com.google.gwt.json.client.JSONParser;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.VLayout;


/**
 * Processes tab in host monitoring.
 */
public class ProcessesView extends VLayout implements Reloadable {
    private ListGrid processesGrid = new ListGrid();
    private RMController controller;
    private String url;

    public ProcessesView(RMController controller, String url) {

        this.controller = controller;
        this.url = url;

        ListGridField pid = new ListGridField("pid", "pid");
        pid.setType(ListGridFieldType.INTEGER);
        ListGridField owner = new ListGridField("owner", "owner");
        ListGridField startTime = new ListGridField("startTime", "Dime");
        ListGridField memSize = new ListGridField("memSize", "Memory");
        ListGridField memRss = new ListGridField("memRss", "Res Memory");
        ListGridField memShare = new ListGridField("memShare", "Share Memory");
        ListGridField cpuTime = new ListGridField("cpuTime", "Cpu Time");
        ListGridField state = new ListGridField("state", "state");
        ListGridField description = new ListGridField("description", "description");
        processesGrid
                .setFields(pid, owner, startTime, memSize, memRss, memShare, cpuTime, state, description);

        setWidth100();
        addMember(processesGrid);
        load();
    }

    public void load() {

        final List<String> attrs = new ArrayList<String>();
        attrs.add("Processes");

        final RMServiceAsync rm = controller.getRMService();
        final RMModel model = controller.getModel();
        final long t = System.currentTimeMillis();

        // loading runtime info
        rm.getNodeMBeanInfo(model.getSessionId(), url, "sigar:Type=Processes", attrs,
                new AsyncCallback<String>() {
                    public void onSuccess(String result) {
                        if (!model.isLoggedIn())
                            return;

                        model
                                .logMessage("Fetched Runtime info in " + (System.currentTimeMillis() - t) +
                                    "ms");

                        //[{"name":"Processes","value":[{"startTime":"Dec8","memSize":"4.0M","memRss":"848K","description":"/sbin/init","memShare":"620K","owner":"root","state":"S","pid":1,"cpuTime":"0:3"}]}]

                        JSONArray processes = JSONParser.parseStrict(result).isArray().get(0).isObject().get(
                                "value").isArray();
                        if (processes != null) {
                            ListGridRecord[] records = new ListGridRecord[processes.size()];
                            for (int i = 0; i < processes.size(); i++) {
                                records[i] = new ListGridRecord();
                                JSONObject process = processes.get(i).isObject();
                                try {
                                    for (String key : process.keySet()) {
                                        ListGridField lgf = processesGrid.getField(key);
                                        if (lgf.getType() == ListGridFieldType.INTEGER) {
                                            records[i].setAttribute(key, Integer.parseInt(process.get(key)
                                                    .toString()));
                                        } else {
                                            records[i].setAttribute(key, process.get(key).toString());
                                        }
                                    }
                                } catch (RuntimeException ex) {
                                    continue;
                                }
                            }
                            processesGrid.setData(records);
                        }
                    }

                    public void onFailure(Throwable caught) {
                        if (RMController.getJsonErrorCode(caught) == 401) {
                            model.logMessage("You have been disconnected from the server.");
                        } else {
                            //error("Failed to fetch RM State: " + RMController.getJsonErrorMessage(caught));
                        }
                    }
                });

        processesGrid.draw();
    }

    public void reload() {
        processesGrid.setData(new ListGridRecord[0]);
        processesGrid.draw();
        load();
    }

    @Override
    public void onFinish(Runnable callback) {
        throw new UnsupportedOperationException("Not implemented");
    }
}
