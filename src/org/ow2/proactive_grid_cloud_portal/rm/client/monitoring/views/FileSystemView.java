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

import com.google.gwt.json.client.JSONArray;
import com.google.gwt.json.client.JSONObject;
import com.google.gwt.json.client.JSONParser;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.google.gwt.visualization.client.AbstractDataTable.ColumnType;
import com.google.gwt.visualization.client.DataTable;
import com.google.gwt.visualization.client.LegendPosition;
import com.google.gwt.visualization.client.visualizations.corechart.Options;
import com.google.gwt.visualization.client.visualizations.corechart.PieChart;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.viewer.DetailViewer;
import com.smartgwt.client.widgets.viewer.DetailViewerField;
import com.smartgwt.client.widgets.viewer.DetailViewerRecord;

/**
 * File System tab in host monitoring.
 */
public class FileSystemView extends VLayout { 
	
	public FileSystemView(RMController controller, String url) {
		setWidth100();
		
		final List<String> attrs = new ArrayList<String>();
		
		attrs.add("DevName");
		attrs.add("DirName");
		attrs.add("Files");
		attrs.add("Options");
		attrs.add("SysTypeName");
		attrs.add("Free");
		attrs.add("Used");
		attrs.add("Total");
		
		final RMServiceAsync rm = controller.getRMService();
		final RMModel model = controller.getModel();
		final long t = System.currentTimeMillis();
		
		// loading runtime info
		rm.getNodeMBeansInfo(model.getSessionId(), url, "sigar:Type=FileSystem,Name=*", attrs, new AsyncCallback<String>() {
			public void onSuccess(String result) {
				if (!model.isLoggedIn())
					return;

				model.logMessage("Fetched Runtime info in " + (System.currentTimeMillis() - t) + "ms");
		
				//{"sigar:Name=/boot,Type=FileSystem":[{"name":"DevName","value":"/dev/sda1"},{"name":"DirName","value":"/boot"},{"name":"Files","value":76912},{"name":"Options","value":"rw"},{"name":"SysTypeName","value":"ext4"},{"name":"Free","value":236558},{"name":"Used","value":60927},{"name":"Total","value":297485}],"sigar:Name=/,Type=FileSystem":[{"name":"DevName","value":"/dev/sda2"},{"name":"DirName","value":"/"},{"name":"Files","value":1921360},{"name":"Options","value":"rw"},{"name":"SysTypeName","value":"ext4"},{"name":"Free","value":15705152},{"name":"Used","value":14532496},{"name":"Total","value":30237648}],"sigar:Name=/local,Type=FileSystem":[{"name":"DevName","value":"/dev/sda5"},{"name":"DirName","value":"/local"},{"name":"Files","value":58851328},{"name":"Options","value":"rw"},{"name":"SysTypeName","value":"ext4"},{"name":"Free","value":916766088},{"name":"Used","value":9996480},{"name":"Total","value":926762568}]}
				
				Options opts = Options.create();
				opts.setLegend(LegendPosition.NONE);
				opts.setColors("#fcaf3e", "#3a668d", "#35a849", "#fcaf3e", "#24c1ff", "#1e4ed7", "#ef2929", "#000000");
				
				JSONObject object = JSONParser.parseStrict(result).isObject();
				if (object != null) {

					for (String disk: object.keySet()) {
						
						HLayout diskLayout = new HLayout();
						 
						DataTable pieData = DataTable.create();
						pieData.addColumn(ColumnType.STRING, "Type");
				        pieData.addColumn(ColumnType.NUMBER, "Bytes");
				        
						DetailViewer details = new DetailViewer();
						DetailViewerRecord dv = new DetailViewerRecord();
						DetailViewerField[] fields = new DetailViewerField[attrs.size()];
						for (int i=0; i<fields.length; i++) {
							fields[i] = new DetailViewerField(attrs.get(i));
						}
						details.setFields(fields);
				        
				        JSONArray properties = object.get(disk).isArray();
				        
				        for (int i = 0; i < properties.size(); i++) {
				        	String name = properties.get(i).isObject().get("name").isString().stringValue();					        	
				        	String value = properties.get(i).isObject().get("value").toString();
				        	if (name.equals("Free") || name.equals("Used")) {
					        	pieData.addRow();
					        	pieData.setValue(pieData.getNumberOfRows()-1, 0, name);
				        		pieData.setValue(pieData.getNumberOfRows()-1, 1, properties.get(i).isObject().get("value").isNumber().doubleValue());
				        	}
				        	
				        	if (name.equals("Free") || name.equals("Used") || name.equals("Total")) {
				        		int inMb = Integer.parseInt(value) / 1024;
				        		if (inMb > 1024) {
				        			value = (inMb/1024) + " Gb";
				        		} else {
					        		value = inMb + " Mb";				        			
				        		}
				        	}
							dv.setAttribute(name, value);				        	
				        }
						details.setData(new DetailViewerRecord[] { dv });
						details.setWidth("50%");
				        					        
						PieChart pie = new PieChart(pieData, opts);
						pie.setWidth("50%");
						pie.draw(pieData, opts);
						diskLayout.addMember(details);
						diskLayout.addMember(pie);
						addMember(diskLayout);
					}
					
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
	}
}
