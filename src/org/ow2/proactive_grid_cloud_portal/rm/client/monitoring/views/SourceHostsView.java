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

import java.util.List;
import java.util.ArrayList;
import com.smartgwt.client.widgets.Label;
import com.google.gwt.json.client.JSONArray;
import com.google.gwt.json.client.JSONValue;
import com.google.gwt.json.client.JSONObject;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.smartgwt.client.widgets.viewer.DetailViewer;
import com.smartgwt.client.widgets.viewer.DetailViewerField;
import com.smartgwt.client.widgets.viewer.DetailViewerRecord;

import org.ow2.proactive_grid_cloud_portal.common.shared.RestServerException;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMModel;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMController;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMServiceAsync;
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.Reloadable;
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.charts.MBeanSourceDetailedView;

/**
 * Hosts & VMs tab in node source monitoring.
 */
public class SourceHostsView extends VLayout implements Reloadable {
        
    private RMController controller;
    private String url;
    private String nsname;
    private AsyncCallback<String> extraCallback;
    private VLayout currpane;
        
        
    public SourceHostsView(final RMController controller, final String url, String nsname, AsyncCallback<String> extraCallback) {

        this.controller = controller;
        this.url = url; 
        this.nsname = nsname; 
        this.extraCallback = extraCallback;
        
        this.reload();
    }
    
    @Override
    public void reload() {
        if (currpane != null) 
            removeMember(currpane);
        currpane = new VLayout();
        addMember(currpane);
        
        Label label = new Label("Reloading...");
        label.setHeight(50);
        currpane.addMember(label);
        currpane.draw();
        
        load();
        
    }
    
    public void load(){
        final RMServiceAsync rm = controller.getRMService();
        final RMModel model = controller.getModel();
        final String mbeanname = SourceOverview.MBEAN_NAME_PREFIX + "-" + nsname;
        final List<String> mbeanAttrs = new ArrayList<String>();
        mbeanAttrs.add("Summary");

        setWidth100();

        rm.getNodeMBeansInfo(model.getSessionId(), url, mbeanname, mbeanAttrs,
            new AsyncCallback<String>() {
                public void onSuccess(String result) {
                        
                    // Result should look like this: 
                    // {"ProActiveResourceManager:name=IaasMonitoring-OPENSTACK_NS_26599":
                    // [{"name":"Summary","value":
                    // {
                    // "host2":{
                    //    "cpu.usage":"0.25",
                    //    "vmsinfo": {"vm1":{"cpu.usage":"0.25","mem.usage":"0.35"},"vm2":{"cpu.usage":"0.25","mem.usage":"0.35"}},
                    //    "mem.usage":"0.35"},
                    // "host1":{
                    //    "cpu.usage":"0.25",
                    //    "vmsinfo": {"vm1":{"cpu.usage":"0.25","mem.usage":"0.35"},"vm2":{"cpu.usage":"0.25","mem.usage":"0.35"}},
                    //    "mem.usage":"0.35"}
                    // }}]
                    // }
                        
                    if (!model.isLoggedIn())
                        return;
                    
                    JSONObject hosts;
                    try { 
                        JSONObject object = controller.parseJSON(result).isObject();
                        JSONValue mbeaninfo = null; 
                        for (String key: object.keySet())
                            mbeaninfo = object.get(key);
                        JSONArray array = mbeaninfo.isArray();
                        JSONObject nsinfo = array.get(0).isObject(); 
                        JSONValue masbueno = nsinfo.get("value");
                        hosts = masbueno.isObject();
                    } catch (Exception e) {
                        // Failure. Could not parse the JSON.
                        return;
                    }
                    
                    VLayout allpane = new VLayout();
                    
                    if (hosts != null) {
                        for (String hostname : hosts.keySet()) {
                                
                            VLayout hostpane = new VLayout();
                            Label label = new Label("<nobr style='font-weight:bold;'>" + hostname + "<nobr>");
                            label.setHeight(50);
                            hostpane.addMember(label);
                                
                            JSONObject hostc = hosts.get(hostname).isObject();
                                
                            DetailViewer hostdetails = generateDetails(hostc);
                            
                            hostpane.addMember(hostdetails);
                            
                            allpane.addMember(hostpane);
                            
                            JSONObject vms = hostc.get("vmsinfo").isObject();
                            if (vms != null) {
                                for (String vmname : vms.keySet() ) {
                                    JSONObject vmc = vms.get(vmname).isObject();
                                    if (vmc != null) {
                                        DetailViewer vmdetails = generateDetails(vmc);
                                        VLayout vmlay = new VLayout();
                                        Label vmlabel = new Label(vmname);
                                        vmlabel.setHeight(50);
                                        vmlay.addMember(vmlabel);
                                        vmlay.addMember(vmdetails);
                                        allpane.addMember(vmlay);
                                    }
                                }
                            }
                        }

                        if (currpane != null) 
                            removeMember(currpane);
                        
                        addMember(allpane);
                        currpane = allpane;
                    }
                }

                public void onFailure(Throwable caught) {
                    if (extraCallback != null) {
                        String errmessage = caught.getMessage();
                        if (caught instanceof RestServerException &&
                            errmessage.contains(MBeanSourceDetailedView.NO_MONITORING_INFO_EXCEPTION_NAME)
                            ) {
                                extraCallback.onFailure(
                                            new Exception ("Node Source monitoring information " +
                                                "not available."));
                        } else {
                            extraCallback.onFailure(caught);
                        } 
                    }

                        
                    if (RMController.getJsonErrorCode(caught) == 401) {
                        model.logMessage("You have been disconnected from the server.");
                    } 
                }
            }
        );
    }
    
    // Generate a GUI object with details about hosts or vms' data in JSONObject format.
    private DetailViewer generateDetails(JSONObject hostc) {
        DetailViewer details = new DetailViewer();
        DetailViewerRecord dv = new DetailViewerRecord();
        DetailViewerField[] fields = new DetailViewerField[hostc.keySet().size()];
        
        int j = 0;
        for (String hostatt : hostc.keySet()) {
            fields[j++] = new DetailViewerField(convertIdToJavascriptId(hostatt));
        }
        details.setFields(fields);

        for (String hostattkey : hostc.keySet()) {
            String value = null;
            if (hostc.get(hostattkey).isString() != null) {
                value = hostc.get(hostattkey).isString().stringValue();
            } else if (hostc.get(hostattkey).isNumber() != null) {
                value = hostc.get(hostattkey).isNumber().toString();
            } else if (hostc.get(hostattkey).isObject() != null) {
                value = "< composed data >";
            } else {
                value = "";
            }
            
            dv.setAttribute(convertIdToJavascriptId(hostattkey), value);
        }
        details.setData(new DetailViewerRecord[] { dv });
        details.setWidth("45%");
        return details;
    }
    
    private String convertIdToJavascriptId(String id){
        return id.replace(".", "_");
    }
    
    @Override
    public synchronized void onFinish(Runnable onFinish) {
    }
}
