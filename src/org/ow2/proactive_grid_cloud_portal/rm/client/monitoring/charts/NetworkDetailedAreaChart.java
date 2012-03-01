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
import com.google.gwt.json.client.JSONArray;
import com.google.gwt.json.client.JSONParser;
import com.google.gwt.visualization.client.LegendPosition;
import com.google.gwt.visualization.client.visualizations.corechart.AxisOptions;

/**
 * Shows full detailes about all network interfaces on host and current network load charts.
 */
public class NetworkDetailedAreaChart extends MBeanTimeAreaChart {

	// store last tx bytes & time stamp
	private long[] history;
	private long[] time;
	
	public NetworkDetailedAreaChart(RMController controller, String jmxServerUrl, String bean) {
		super(controller, jmxServerUrl, bean, new String[] {"RxBytes", "TxBytes"}, "");

		history = new long[2];
		time = new long[2];
		
		AxisOptions vAxis = AxisOptions.create();
		vAxis.set("format", "#.# Mb/s");
		loadOpts.setVAxisOptions(vAxis);
		loadOpts.setLegend(LegendPosition.RIGHT);
		loadTable.setColumnLabel(1, "RX");
		loadTable.setColumnLabel(2, "TX");
	}

	@Override
	public void processResult(String result) {
		JSONArray array = JSONParser.parseStrict(result).isArray();
		if (array != null) {
			String timeStamp = DateTimeFormat.getFormat(PredefinedFormat.HOUR24_MINUTE).format(new Date(System.currentTimeMillis()));
			addRow();
			
			loadTable.setValue(loadTable.getNumberOfRows()-1, 0, timeStamp);
			
			// getting primitive values of all attributes
			for (int i = 0; i < attrs.length; i++) {
				double value = array.get(i).isObject().get("value").isNumber().doubleValue();
				long t = System.currentTimeMillis();
				if (history[i] > 0) {					
					double bytePerMilliSec = (value - history[i]) / (t - time[i]);
					double mbPerSec = bytePerMilliSec * 1000 / (1024 * 1024);
					loadTable.setValue(loadTable.getNumberOfRows() - 1, i+1, (long)mbPerSec);
				} else {
					loadTable.setValue(loadTable.getNumberOfRows() - 1, i+1, 0);					
				}
				
				history[i] = (long)value;
				time[i] = t;
			}
			
			loadChart.draw(loadTable, loadOpts);
		}
	}	
}
