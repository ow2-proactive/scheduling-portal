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

import java.util.Arrays;

import org.ow2.proactive_grid_cloud_portal.rm.client.RMController;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMModel;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMServiceAsync;
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.Reloadable;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.google.gwt.user.client.ui.AbsolutePanel;
import com.google.gwt.visualization.client.DataTable;
import com.google.gwt.visualization.client.LegendPosition;
import com.google.gwt.visualization.client.visualizations.corechart.CoreChart;
import com.google.gwt.visualization.client.visualizations.corechart.HorizontalAxisOptions;
import com.google.gwt.visualization.client.visualizations.corechart.Options;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.layout.VLayout;


/**
 * Chart that retrieves information from MBean.
 */
public abstract class MBeanChart extends VLayout implements Reloadable {

	protected final int MAX_ROWS_NUMBER = 100;

	protected RMController controller;
	protected String jmxServerUrl;
	protected String mbeanName;
	protected String[] attrs;

	protected CoreChart loadChart;
	protected DataTable loadTable;
	protected Options loadOpts;
	protected AbsolutePanel chartContainer;

	protected Runnable onFinish;

	public MBeanChart(RMController controller, String jmxServerUrl, String mbean, String[] attrs, String title) {
		this.controller = controller;
		this.jmxServerUrl = jmxServerUrl;
		this.mbeanName = mbean;
		this.attrs = attrs;

		loadOpts = Options.create();
		HorizontalAxisOptions loadAxis = HorizontalAxisOptions.create();
		loadAxis.setMaxAlternation(1);
		loadAxis.setSlantedText(false);
		loadOpts.setLegend(LegendPosition.NONE);
		loadOpts.setHAxisOptions(loadAxis);
		loadOpts.setColors("#fcaf3e", "#3a668d", "#35a849", "#fcaf3e", "#24c1ff", "#1e4ed7", "#ef2929",
				"#000000");
		loadAxis.setMinValue(0);

		loadTable = DataTable.create();

		setWidth100();
		setHeight100();

		if (title.length() > 0) {
			Label label = new Label("<nobr style='font-weight:bold;'>" + title + "<nobr>");
			label.setHeight(30);
			addMember(label);
		}

		chartContainer = new AbsolutePanel();
		chartContainer.setWidth("100%");
		chartContainer.setHeight("200px");

		loadChart = createChart(loadTable, loadOpts);
		loadChart.setWidth("100%");
		loadChart.setHeight("200px");
		chartContainer.add(loadChart);
		addMember(chartContainer);
	}

	@Override
	public void reload() {
		final RMServiceAsync rm = controller.getRMService();
		final RMModel model = controller.getModel();
		final long t = System.currentTimeMillis();

		rm.getNodeMBeanInfo(model.getSessionId(), jmxServerUrl, mbeanName, Arrays.asList(attrs),
				new AsyncCallback<String>() {
					public void onSuccess(String result) {
						if (onFinish != null) {
							onFinish.run();
						}
						if (!model.isLoggedIn())
							return;

						model.logMessage("Fetched " + mbeanName + ":" + Arrays.toString(attrs) + " in " +
							(System.currentTimeMillis() - t) + "ms");
						processResult(result);
					}

					public void onFailure(Throwable caught) {
						if (onFinish != null) {
							onFinish.run();
						}
						if (RMController.getJsonErrorCode(caught) == 401) {
							model.logMessage("You have been disconnected from the server.");
						} else {
							//error("Failed to fetch RM State: " + RMController.getJsonErrorMessage(caught));
						}
					}
				});
	}

	protected void addRow() {
		if (loadTable.getNumberOfRows() > MAX_ROWS_NUMBER) {
			loadTable.removeRow(0);
		}
		loadTable.addRow();
	}

	public abstract CoreChart createChart(DataTable data, Options opts);

	public abstract void processResult(String result);

	public void onFinish(Runnable onFinish) {
		this.onFinish = onFinish;
	}
}
