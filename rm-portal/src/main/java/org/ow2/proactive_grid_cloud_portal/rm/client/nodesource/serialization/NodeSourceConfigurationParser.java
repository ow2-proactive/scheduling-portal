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
package org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.serialization;

import java.util.HashMap;

import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSourceConfiguration;
import org.ow2.proactive_grid_cloud_portal.rm.client.PluginDescriptor;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMController;

import com.google.gwt.json.client.JSONArray;
import com.google.gwt.json.client.JSONException;
import com.google.gwt.json.client.JSONObject;


public class NodeSourceConfigurationParser {

    private RMController controller;

    public NodeSourceConfigurationParser(RMController controller) {
        this.controller = controller;
    }

    public NodeSourceConfiguration parseNodeSourceConfiguration(String json) throws ImportException {

        JSONObject jsonObject;

        try {
            jsonObject = this.controller.parseJSON(json).isObject();
        } catch (JSONException e) {
            throw new ImportException("The imported node source is not a valid JSON file", e);
        }

        try {
            String nodeSourceName = jsonObject.get("nodeSourceName").isString().stringValue();
            boolean nodesRecoverable = jsonObject.get("nodesRecoverable").isBoolean().booleanValue();

            JSONObject infrastructurePluginDescriptorJson = jsonObject.get("infrastructurePluginDescriptor").isObject();
            String infrastructurePluginName = infrastructurePluginDescriptorJson.get("pluginName")
                                                                                .isString()
                                                                                .stringValue();
            PluginDescriptor infrastructurePluginDescriptor = getPluginDescriptor(infrastructurePluginDescriptorJson,
                                                                                  infrastructurePluginName);

            JSONObject policyPluginDescriptorJson = jsonObject.get("policyPluginDescriptor").isObject();
            String policyPluginName = policyPluginDescriptorJson.get("pluginName").isString().stringValue();
            PluginDescriptor policyPluginDescriptor = getPluginDescriptor(policyPluginDescriptorJson, policyPluginName);

            return new NodeSourceConfiguration(nodeSourceName,
                                               nodesRecoverable,
                                               infrastructurePluginDescriptor,
                                               policyPluginDescriptor);
        } catch (RuntimeException e) {
            throw new ImportException("The imported node source has incorrect parameters.", e);
        }
    }

    public HashMap<String, PluginDescriptor> parsePluginDescriptors(String json) {

        JSONArray arr = this.controller.parseJSON(json).isArray();
        HashMap<String, PluginDescriptor> plugins = new HashMap<>();

        for (int i = 0; i < arr.size(); i++) {
            JSONObject p = arr.get(i).isObject();

            String pluginName = p.get("pluginName").isString().stringValue();
            PluginDescriptor pluginDescriptor = getPluginDescriptor(p, pluginName);

            plugins.put(pluginName, pluginDescriptor);
        }

        return plugins;
    }

    private PluginDescriptor getPluginDescriptor(JSONObject p, String pluginName) {

        String pluginDescription = p.get("pluginDescription").isString().stringValue();
        PluginDescriptor desc = new PluginDescriptor(pluginName, pluginDescription);

        JSONArray fields = p.get("configurableFields").isArray();
        for (int j = 0; j < fields.size(); j++) {
            JSONObject field = fields.get(j).isObject();

            String name = field.get("name").isString().stringValue();
            String value = field.get("value").isString().stringValue();

            JSONObject meta = field.get("meta").isObject();
            String metaType = meta.get("type").isString().stringValue();
            String descr = meta.get("description").isString().stringValue();
            boolean dynamic = meta.get("dynamic").isBoolean().booleanValue();

            boolean password = false;
            boolean credentials = false;
            boolean file = false;

            if (metaType.equalsIgnoreCase("password")) {
                password = true;
            } else if (metaType.equalsIgnoreCase("fileBrowser")) {
                file = true;
            } else if (metaType.equalsIgnoreCase("credential")) {
                credentials = true;
            }

            PluginDescriptor.Field f = new PluginDescriptor.Field(name,
                                                                  value,
                                                                  descr,
                                                                  password,
                                                                  credentials,
                                                                  file,
                                                                  dynamic);

            desc.getConfigurableFields().add(f);
        }
        return desc;
    }

}
