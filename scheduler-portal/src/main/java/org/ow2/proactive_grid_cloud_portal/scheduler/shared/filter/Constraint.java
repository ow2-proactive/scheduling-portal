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

public class Constraint implements IsSerializable {
    private Field targetField;

    private Action action;

    private String value;

    public Constraint() {
    }

    public Constraint(Field targetField, Action action, String value) {
        this.targetField = targetField;
        this.action = action;
        this.value = value;
    }

    public Field getTargetField() {
        return targetField;
    }

    public void setTargetField(Field targetField) {
        this.targetField = targetField;
    }

    public Action getAction() {
        return action;
    }

    public void setAction(Action action) {
        this.action = action;
    }

    public String getValue() {
        return value;
    }

    public void setValue(String value) {
        this.value = value;
    }

    public String getFilteringString(Field field) {
        if (field != targetField) {
            return null;
        }

        switch (action) {
            //case CONTAINS:
            //    return "*" + value + "*";
            case EQUALS:
                return value;
            //case GREATER_THAN_OR_EQUAL_TO:
            //    return null;
            //case LESS_THAN_OR_EQUAL_TO:
            //    return null;
            //case STARTS_WITH:
            //    return value + "*";
            default:
                return null;
        }
    }
}
