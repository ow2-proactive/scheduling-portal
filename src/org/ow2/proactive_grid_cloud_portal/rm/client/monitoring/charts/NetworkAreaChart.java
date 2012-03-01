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
package org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.charts;

import java.util.Date;

import org.ow2.proactive_grid_cloud_portal.rm.client.RMController;

import com.google.gwt.i18n.client.DateTimeFormat;
import com.google.gwt.i18n.client.DateTimeFormat.PredefinedFormat;
import com.google.gwt.json.client.JSONObject;
import com.google.gwt.json.client.JSONParser;
import com.google.gwt.regexp.shared.MatchResult;
import com.google.gwt.regexp.shared.RegExp;
import com.google.gwt.visualization.client.AbstractDataTable.ColumnType;
import com.google.gwt.visualization.client.visualizations.corechart.AxisOptions;

/**
 * Shows all network interfaces on host.
 */
public class NetworkAreaChart extends MBeansTimeAreaChart {

	// store last tx bytes & time stamp
	private long time[];
	private long txBytes[];
	
	public NetworkAreaChart(RMController controller, String jmxServerUrl) {
		super(controller, jmxServerUrl, "sigar:Type=NetInterface,Name=*",
				"RxBytes", "Network");

		AxisOptions vAxis = AxisOptions.create();
		vAxis.set("format", "#.# Mb/s");
		loadOpts.setVAxisOptions(vAxis);
	}

	@Override
	public void processResult(String result) {
		
		// Result:{"sigar:Name=lo,Type=NetInterface":[{"name":"TxBytes","value":147762795896}],"sigar:Name=eth0,Type=NetInterface":[{"name":"TxBytes","value":249539647369}]}
		JSONObject object = JSONParser.parseStrict(result).isObject();
		if (object != null) {
			
			String timeStamp = DateTimeFormat.getFormat(
					PredefinedFormat.HOUR24_MINUTE).format(
					new Date(System.currentTimeMillis()));
			
			addRow();
			loadTable.setValue(loadTable.getNumberOfRows() - 1, 0, timeStamp);
			
			boolean initColumns = super.initColumns();
			
			if (initColumns) {
				time = new long[object.size()];			
				txBytes = new long[object.size()];				
			}
			
			int colIndex = 1;
			for (String key: object.keySet()) {
				
				if (initColumns) {
					loadTable.addColumn(ColumnType.NUMBER, beautifyName(key));
				}
				
				long value = Long.parseLong(object.get(key).isArray().get(0).isObject().get("value").toString());
				long t = System.currentTimeMillis();
				if (txBytes[colIndex-1] > 0) {					
					double bytePerMilliSec = (value - txBytes[colIndex-1]) / (t - time[colIndex-1]);
					double mbPerSec = bytePerMilliSec * 1000 / (1024 * 1024);
					loadTable.setValue(loadTable.getNumberOfRows() - 1, colIndex, (long)mbPerSec);
				}
				
				txBytes[colIndex-1] = value;
				time[colIndex-1] = t;
				
				colIndex++;
			}

			loadChart.draw(loadTable, loadOpts);
		}
	}
	
	private String beautifyName(String mbeanName) {
		// sigar:Name=lo,Type=NetInterface
		String patternStr = "sigar:Name=(.*),Type=NetInterface";
		RegExp pattern = RegExp.compile(patternStr);
		MatchResult matcher = pattern.exec(mbeanName);		
		return matcher.getGroup(1);
	}
}
