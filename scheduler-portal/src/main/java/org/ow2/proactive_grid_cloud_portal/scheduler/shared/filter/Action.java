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
package org.ow2.proactive_grid_cloud_portal.scheduler.shared.filter;

import com.google.gwt.user.client.rpc.IsSerializable;


/**
 * @author ActiveEon Team
 * @since May 9, 2017
 */
public enum Action implements IsSerializable {
    EQUALS("Equals");
    //CONTAINS("Contains"),
    //STARTS_WITH("Starts with"),
    //LESS_THAN_OR_EQUAL_TO("Less than or equal to"),
    //GREATER_THAN_OR_EQUAL_TO("Greater than or equal to");

    private String name;

    private Action(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public static Action get(String name) {
        for (Action action : Action.values()) {
            if (action.name.equals(name)) {
                return action;
            }
        }
        return EQUALS;
    }
}
