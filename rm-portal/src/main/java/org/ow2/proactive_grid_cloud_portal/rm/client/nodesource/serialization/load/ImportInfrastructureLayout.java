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
package org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.serialization.load;

import org.ow2.proactive_grid_cloud_portal.rm.client.PluginDescriptor;
import org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.NodeSourceWindow;
import org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.serialization.NodeSourceConfigurationParser;
import org.ow2.proactive_grid_cloud_portal.rm.shared.CatalogConstants;
import org.ow2.proactive_grid_cloud_portal.rm.shared.CatalogKind;


public class ImportInfrastructureLayout extends ImportLayout {

    public ImportInfrastructureLayout(NodeSourceWindow nodeSourceWindow, String layoutTitle) {
        super(nodeSourceWindow, layoutTitle);
    }

    @Override
    public CatalogKind getKind() {
        return CatalogKind.INFRASTRUCTURE;
    }

    @Override
    public void handleImport(String infrastructureJsonString) {
        NodeSourceConfigurationParser nodeSourceConfigurationParser = new NodeSourceConfigurationParser();
        String importedNodeSourceJsonString = nodeSourceConfigurationParser.wrapInfrastructureJsonString(infrastructureJsonString);
        PluginDescriptor infrastructurePluginDescriptor = nodeSourceConfigurationParser.parseNodeSourceConfiguration(importedNodeSourceJsonString)
                                                                                       .getInfrastructurePluginDescriptor();
        this.nodeSourceWindow.setCreatedFromImport();
        this.nodeSourceWindow.replaceInfrastructureItems(infrastructurePluginDescriptor);
        this.nodeSourceWindow.resetCreatedFromImport();
    }

}
