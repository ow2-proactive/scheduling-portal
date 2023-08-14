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

import org.ow2.proactive_grid_cloud_portal.common.client.Model;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMController;
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.Reloadable;
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.charts.MBeanChart;
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.charts.MemoryLineChart;
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.charts.SwapLineChart;

import com.smartgwt.client.widgets.layout.VLayout;


/**
 * Memory tab in host monitoring.
 */
public class MemoryView extends VLayout implements Reloadable {

    private MBeanChart ram;

    private MBeanChart swap;

    private ReloadableChain chain;

    public MemoryView(RMController controller, String url) {
        // memory view
        ram = new MemoryLineChart(controller, url);
        swap = new SwapLineChart(controller, url);

        chain = new ReloadableChain(ram, swap);

        setWidth100();
        addMember(ram);
        addMember(swap);
    }

    public void selectRange(Model.StatHistory.Range timeRange) {
        ram.selectRange(timeRange);
        swap.selectRange(timeRange);
    }

    @Override
    public void reload() {
        chain.reload();
    }

    @Override
    public void onFinish(final Runnable onFinish) {
        chain.onFinish(new Runnable() {
            public void run() {
                onFinish.run();
            }
        });
    }
}
