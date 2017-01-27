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
package org.ow2.proactive_grid_cloud_portal.rm.client;

import java.util.Map;

import org.ow2.proactive_grid_cloud_portal.common.client.Settings;
import org.ow2.proactive_grid_cloud_portal.rm.shared.RMConfig;

import com.google.gwt.core.client.EntryPoint;
import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.smartgwt.client.util.SC;


public class RMPortal implements EntryPoint {

    private final RMServiceAsync rmService = GWT.create(RMService.class);

    public void onModuleLoad() {
        loadProperties();
    }

    private void loadProperties() {
        rmService.getProperties(new AsyncCallback<Map<String, String>>() {

            public void onSuccess(Map<String, String> result) {
                RMConfig.get().load(result);
                Settings.load();
                RMController c = new RMController(rmService);
                GWT.setUncaughtExceptionHandler(c);
            }

            public void onFailure(Throwable caught) {
                SC.warn("Unable to get the client properties:<br>" + caught.getMessage());
            }
        });
    }

}
