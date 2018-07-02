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
package org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.serialization.export.file;

import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSourceConfiguration;
import org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.serialization.NodeSourceConfigurationParser;
import org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.serialization.SerializationType;
import org.ow2.proactive_grid_cloud_portal.rm.shared.ExportToFileConstants;

import com.google.gwt.json.client.JSONObject;


public class ExportPolicyToFileHandler extends ExportToFileHandler {

    public ExportPolicyToFileHandler(String nodeSourceName) {
        super(nodeSourceName);
    }

    @Override
    protected String getFormTarget() {
        return SerializationType.EXPORT_TO_FILE.getFormTarget();
    }

    @Override
    protected void handleNodeSourceConfigurationResponse(String fileContentJson) {
        NodeSourceConfigurationParser nodeSourceConfigurationParser = new NodeSourceConfigurationParser();
        NodeSourceConfiguration nodeSourceConfiguration = nodeSourceConfigurationParser.parseNodeSourceConfiguration(fileContentJson);
        JSONObject jsonObject = nodeSourceConfigurationParser.parseJSON(fileContentJson).isObject();
        String infrastructurePluginDescriptorJson = jsonObject.get("policyPluginDescriptor").isObject().toString();

        this.fileContentItem.setValue(infrastructurePluginDescriptorJson);
        this.fileSuffixItem.setValue(ExportToFileConstants.POLICY_FILE_NAME_SUFFIX);
        this.nodeSourceNameItem.setValue(nodeSourceConfiguration.getNodeSourceName());
    }

}
