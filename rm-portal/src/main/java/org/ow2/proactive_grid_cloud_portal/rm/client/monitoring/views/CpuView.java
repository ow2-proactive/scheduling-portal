/*
 * ################################################################
 *
 * ProActive Parallel Suite(TM): The Java(TM) library for
 *    Parallel, Distributed, Multi-Core Computing for
 *    Enterprise Grids & Clouds
 *
 * Copyright (C) 1997-2015 INRIA/University of
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

    @Override
    public void reload() {
        cpusUsage.reload();
    }

    @Override
    public void onFinish(Runnable onFinish) {
        cpusUsage.onFinish(onFinish);
    }
}
