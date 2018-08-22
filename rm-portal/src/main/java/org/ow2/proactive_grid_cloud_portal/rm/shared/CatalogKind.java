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

public enum CatalogKind {

    NODE_SOURCE("NodeSource", "", "Node Source"),

    INFRASTRUCTURE("InfrastructureNodeSource", "_Infrastructure", "Infrastructure"),

    POLICY("PolicyNodeSource", "_Policy", "Policy");

    private final String kindString;

    private final String suffix;

    private final String description;

    CatalogKind(String kindString, String suffix, String description) {
        this.kindString = kindString;
        this.suffix = suffix;
        this.description = description;
    }

    public String getKindString() {
        return this.kindString;
    }

    public String getSuffix() {
        return this.suffix;
    }

    public String getDescription() {
        return this.description;
    }

}
