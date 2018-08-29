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
package org.ow2.proactive_grid_cloud_portal.rm.shared;

import java.util.Arrays;


public enum NodeSourceAction {

    CREATE("create", true),

    EDIT("edit", true),

    UPDATE("update", false),

    UNKNOWN("unknown", false);

    private final String actionDescription;

    private final boolean fullEditAllowed;

    NodeSourceAction(String actionDescription, boolean fullEditAllowed) {
        this.actionDescription = actionDescription;
        this.fullEditAllowed = fullEditAllowed;
    }

    public String getActionDescription() {
        return actionDescription;
    }

    public boolean isFullEditAllowed() {
        return this.fullEditAllowed;
    }

    public static NodeSourceAction getEnum(String actionDescription) {
        return Arrays.stream(NodeSourceAction.values())
                     .filter(status -> status.actionDescription.equals(actionDescription))
                     .findAny()
                     .orElseThrow(IllegalArgumentException::new);
    }

}
