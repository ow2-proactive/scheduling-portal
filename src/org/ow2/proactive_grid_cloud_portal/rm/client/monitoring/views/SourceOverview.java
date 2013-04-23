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

import java.util.Arrays;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.layout.VLayout;
import com.google.gwt.user.client.rpc.AsyncCallback;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMController;
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.Reloadable;
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.charts.MBeanSourceDetailedView;


/**
 * Overview tab in node source monitoring.
 */
public class SourceOverview extends VLayout implements Reloadable {

    private MBeanSourceDetailedView nsInfo;

    public SourceOverview(RMController controller, String url, String nsname,
            AsyncCallback<String> extraCallback) {

        nsInfo = new MBeanSourceDetailedView(extraCallback, controller, url,
            MonitoringSourceView.MBEAN_NAME_PREFIX + "-" + nsname, Arrays.asList("Hosts", "VMs"));

        nsInfo.reload();

        VLayout nsInfoRow = new VLayout();

        Label nsLabel = new Label("<nobr style='font-weight:bold;'>Overview<nobr>");
        nsLabel.setHeight(50);
        nsInfoRow.addMember(nsLabel);
        nsInfoRow.addMember(nsInfo);
        nsInfoRow.setWidth("50%");
        nsInfoRow.setHeight("120px");

        addMember(nsInfoRow);
    }

    @Override
    public void reload() {
        nsInfo.reload();
    }

    @Override
    public void onFinish(Runnable callback) {
    }

}
