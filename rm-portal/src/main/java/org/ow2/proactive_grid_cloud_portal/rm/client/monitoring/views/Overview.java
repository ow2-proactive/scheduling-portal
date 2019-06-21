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

import java.util.Arrays;

import org.ow2.proactive_grid_cloud_portal.rm.client.RMController;
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.Reloadable;
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.charts.CpuUsageAreaChart;
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.charts.DiskPieChart;
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.charts.MBeanChart;
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.charts.MBeanDetailedView;
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.charts.MemoryLineChart;
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.charts.NetworkAreaChart;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;


/**
 * Overview tab in host monitoring.
 */
public class Overview extends VLayout implements Reloadable {

    private MBeanDetailedView osInfo;

    private MBeanChart cpuUsage;

    private MBeanChart memory;

    private MBeanChart network;

    private MBeanChart disk;

    private ReloadableChain chain;

    public Overview(RMController controller, String url, AsyncCallback<String> extraCallback) {

        osInfo = new MBeanDetailedView(extraCallback);
        osInfo.load(controller, url, "java.lang:type=OperatingSystem", Arrays.asList("Name", "Arch", "Version"));
        cpuUsage = new CpuUsageAreaChart(controller, url);
        memory = new MemoryLineChart(controller, url);
        network = new NetworkAreaChart(controller, url);
        disk = new DiskPieChart(controller, url);

        disk.reload();

        chain = new ReloadableChain(cpuUsage, memory, network);

        VLayout osInfoRow = new VLayout();
        HLayout cpuMemRow = new HLayout();
        HLayout netRow = new HLayout();
        HLayout fileSysRow = new HLayout();

        Label osLabel = new Label("<nobr style='font-weight:bold;'>Operating system<nobr>");
        osLabel.setHeight(50);
        osInfoRow.addMember(osLabel);
        osInfoRow.addMember(osInfo);
        osInfoRow.setWidth("50%");
        osInfoRow.setHeight("120px");

        cpuMemRow.addMember(cpuUsage);
        cpuMemRow.addMember(memory);
        netRow.addMember(network);
        fileSysRow.addMember(disk);

        addMember(osInfoRow);
        addMember(cpuMemRow);
        addMember(netRow);
        addMember(fileSysRow);
    }

    @Override
    public void reload() {
        chain.reload();
    }

    @Override
    public void onFinish(final Runnable callback) {
        chain.onFinish(new Runnable() {
            public void run() {
                callback.run();
            }
        });
    }
}
