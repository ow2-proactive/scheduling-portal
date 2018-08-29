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

import com.google.gwt.user.client.rpc.IsSerializable;


public enum OutputMode implements IsSerializable {

    LOG_OUT_ERR("Out & Err (1024 lines)"),
    LOG_ERR("Std Err"),
    LOG_OUT("Std Out"),
    LOG_FULL("Full logs (download)");

    public final String label;

    private OutputMode(String label) {
        this.label = label;
    }

    public static String[] toStringArray() {
        OutputMode[] modes = OutputMode.values();
        String[] result = new String[modes.length];
        for (int i = 0; i < modes.length; i++) {
            result[i] = modes[i].label;
        }
        return result;
    }
}
