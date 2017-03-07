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
package org.ow2.proactive_grid_cloud_portal.scheduler.client;

import org.ow2.proactive_grid_cloud_portal.common.client.JSUtil;
import org.ow2.proactive_grid_cloud_portal.common.client.Settings;
import org.ow2.proactive_grid_cloud_portal.scheduler.shared.SchedulerConfig;
import org.ow2.proactive_grid_cloud_portal.scheduler.shared.SchedulerPortalDisplayConfig;
import org.ow2.proactive_grid_cloud_portal.scheduler.shared.SharedProperties;

import com.google.gwt.core.client.EntryPoint;
import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.smartgwt.client.util.SC;


public class SchedulerPortal implements EntryPoint {

    private final SchedulerServiceAsync rpcService = GWT.create(SchedulerService.class);

    public void onModuleLoad() {
        JSUtil.addScript("portal/raphael-min.js");
        loadProperties();
    }

    private void loadProperties() {
        rpcService.getProperties(new AsyncCallback<SharedProperties>() {

            public void onSuccess(SharedProperties result) {
                SchedulerConfig.get().load(result.getProperties());
                SchedulerPortalDisplayConfig.get().load(result.getPortalProperties());
                Settings.load();
                Scheduler.setSchedulerService(rpcService);
                SchedulerController c = new SchedulerController(rpcService);
                GWT.setUncaughtExceptionHandler(c);
            }

            public void onFailure(Throwable caught) {
                SC.warn("Unable to get the client properties:<br>" + caught.getMessage());
            }
        });
    }

}
