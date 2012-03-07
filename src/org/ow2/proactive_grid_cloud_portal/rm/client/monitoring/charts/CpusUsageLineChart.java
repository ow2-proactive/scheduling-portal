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
import com.google.gwt.json.client.JSONValue;
import com.google.gwt.regexp.shared.MatchResult;
import com.google.gwt.regexp.shared.RegExp;
import com.google.gwt.visualization.client.AbstractDataTable.ColumnType;
import com.google.gwt.visualization.client.DataTable;
import com.google.gwt.visualization.client.visualizations.corechart.AxisOptions;
import com.google.gwt.visualization.client.visualizations.corechart.CoreChart;
import com.google.gwt.visualization.client.visualizations.corechart.LineChart;
import com.google.gwt.visualization.client.visualizations.corechart.Options;

/**
 * Shows the CPU usage per core.
 */
public class CpusUsageLineChart extends MBeansTimeAreaChart {
	
	public CpusUsageLineChart(RMController controller, String jmxServerUrl) {
		super(controller, jmxServerUrl, "sigar:Type=CpuCoreUsage,Name=*",
				"Combined", "Load History");
		
		AxisOptions vAxis = AxisOptions.create();
		vAxis.set("format", "#%");
		loadOpts.setVAxisOptions(vAxis);
		chartContainer.setHeight("300px");
		loadChart.setHeight("300px");
	}

	@Override
	public void processResult(String result) {
		JSONObject object = JSONParser.parseStrict(result).isObject();
		if (object != null) {
			
			String timeStamp = DateTimeFormat.getFormat(
					PredefinedFormat.HOUR24_MINUTE).format(
					new Date(System.currentTimeMillis()));
			
			addRow();
			loadTable.setValue(loadTable.getNumberOfRows() - 1, 0, timeStamp);

			boolean initColumns = super.initColumns();
			int colIndex = 1;
			for (String key: object.keySet()) {
				
				if (initColumns) {
					loadTable.addColumn(ColumnType.NUMBER, beautifyName(key));
				}
				
				double value = 0;
				JSONValue jsonVal = object.get(key).isArray().get(0).isObject().get("value");
				if (jsonVal != null && jsonVal.isNumber() != null) {
					value = jsonVal.isNumber().doubleValue();
				}
				loadTable.setValue(loadTable.getNumberOfRows() - 1, colIndex++, value);
			}

			loadChart.draw(loadTable, loadOpts);
		}
	}
	
	private String beautifyName(String mbeanName) {
		// sigar:Name=lo,Type=NetInterface
		String patternStr = "sigar:Name=(.*),Type=CpuCore";
		RegExp pattern = RegExp.compile(patternStr);
		MatchResult matcher = pattern.exec(mbeanName);		
		return matcher.getGroup(1);
	}

	@Override
	public CoreChart createChart(DataTable data, Options opts) {
		return new LineChart(data, opts);
	}
}
