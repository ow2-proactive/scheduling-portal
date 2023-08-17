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
package org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.views;

import java.util.ArrayList;
import java.util.List;

import org.ow2.proactive_grid_cloud_portal.common.client.Model;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMController;
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.Reloadable;
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.charts.CpusUsageLineChart;
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.charts.MBeanChart;
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.charts.MBeanDetailedView;

import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.layout.VLayout;


/**
 * CPU tab in host monitoring.
 */
public class CpuView extends VLayout implements Reloadable {

    private MBeanDetailedView overview;

    private MBeanChart cpusUsage;

    public CpuView(RMController controller, String nodeUrl) {

        setWidth100();
        Label label = new Label("<nobr style='font-weight:bold;'>Details<nobr>");
        label.setHeight(50);

        overview = new MBeanDetailedView();
        overview.setWidth100();

        // cpu graph
        cpusUsage = new CpusUsageLineChart(controller, nodeUrl);

        addMember(cpusUsage);
        addMember(label);
        addMember(overview);

        List<String> attrs = new ArrayList<String>();

        attrs.add("CacheSize");
        attrs.add("CoresPerSocket");
        attrs.add("Idle");
        attrs.add("Irq");
        attrs.add("Mhz");
        attrs.add("Model");
        attrs.add("Nice");
        attrs.add("SoftIrq");
        attrs.add("Sys");
        attrs.add("Total");
        attrs.add("TotalCores");
        attrs.add("TotalSockets");
        attrs.add("User");
        attrs.add("Vendor");
        attrs.add("Wait");

        overview.load(controller, nodeUrl, "sigar:Type=Cpu", attrs);
    }

    public void selectRange(Model.StatHistory.Range timeRange) {
        cpusUsage.selectRange(timeRange);
    }

    @Override
    public void reload() {
        cpusUsage.reload();
    }

    @Override
    public void onFinish(Runnable onFinish) {
        cpusUsage.onFinish(onFinish);
    }
}
