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
import java.util.Map;
import java.util.Optional;

import org.ow2.proactive_grid_cloud_portal.common.client.model.LogModel;
import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSourceConfiguration;
import org.ow2.proactive_grid_cloud_portal.rm.client.PluginDescriptor;

import com.google.gwt.json.client.*;


public class NodeSourceConfigurationParser {

    private static final String JSON_FILE_ERROR_MESSAGE = "The imported file is not a valid JSON file";

    public JSONValue parseJSON(String jsonStr) {
        try {
            return JSONParser.parseStrict(jsonStr);
        } catch (Throwable t) {
            LogModel logger = LogModel.getInstance();
            logger.logCriticalMessage("JSON Parser failed with " + t.getClass().getName() + ": " + t.getMessage());
            logger.logCriticalMessage("Raw input was: " + jsonStr);
            return new JSONObject();
        }
    }

    public NodeSourceConfiguration parseNodeSourceConfiguration(String json) {

        JSONObject jsonObject;

        try {
            jsonObject = parseJSON(json).isObject();
        } catch (JSONException e) {
            throw new IllegalArgumentException(JSON_FILE_ERROR_MESSAGE, e);
        }

        try {
            String nodeSourceName = jsonObject.get("nodeSourceName").isString().stringValue();
            boolean nodesRecoverable = jsonObject.get("nodesRecoverable").isBoolean().booleanValue();

            PluginDescriptor infrastructurePluginDescriptor = getInfrastructurePluginDescriptor(jsonObject);
            PluginDescriptor policyPluginDescriptor = getPolicyPluginDescriptor(jsonObject);

            return new NodeSourceConfiguration(nodeSourceName,
                                               nodesRecoverable,
                                               infrastructurePluginDescriptor,
                                               policyPluginDescriptor);
        } catch (RuntimeException e) {
            throw new IllegalArgumentException("The imported file has incorrect parameters.", e);
        }
    }

    private PluginDescriptor getInfrastructurePluginDescriptor(JSONObject jsonObject) {
        JSONObject infrastructurePluginDescriptorJson = jsonObject.get("infrastructurePluginDescriptor").isObject();
        String infrastructurePluginName = infrastructurePluginDescriptorJson.get("pluginName").isString().stringValue();
        return getPluginDescriptor(infrastructurePluginDescriptorJson, infrastructurePluginName);
    }

    private PluginDescriptor getPolicyPluginDescriptor(JSONObject jsonObject) {
        JSONObject policyPluginDescriptorJson = jsonObject.get("policyPluginDescriptor").isObject();
        String policyPluginName = policyPluginDescriptorJson.get("pluginName").isString().stringValue();
        return getPluginDescriptor(policyPluginDescriptorJson, policyPluginName);
    }

    public HashMap<String, PluginDescriptor> parsePluginDescriptors(String json) {

        JSONArray arr = parseJSON(json).isArray();
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
        Map<Integer, String> sectionDescriptions = Optional.ofNullable(p.get("sectionDescriptions"))
                                                           .map(JSONValue::isObject)
                                                           .map(jsonSectionDescriptions -> {
                                                               Map<Integer, String> newSections = new HashMap<>();
                                                               for (String key : jsonSectionDescriptions.keySet()) {
                                                                   int sectionSelector = Integer.parseInt(key);
                                                                   String sectionDescription = jsonSectionDescriptions.get(key)
                                                                                                                      .isString()
                                                                                                                      .stringValue();
                                                                   newSections.put(sectionSelector, sectionDescription);
                                                               }
                                                               return newSections;
                                                           })
                                                           .orElse(new HashMap<>());

        PluginDescriptor desc = new PluginDescriptor(pluginName, pluginDescription, sectionDescriptions);

        JSONArray fields = p.get("configurableFields").isArray();
        for (int j = 0; j < fields.size(); j++) {
            JSONObject field = fields.get(j).isObject();

            String name = field.get("name").isString().stringValue();
            String value = field.get("value").isString().stringValue();

            JSONObject meta = field.get("meta").isObject();
            String metaType = meta.get("type").isString().stringValue();
            String descr = meta.get("description").isString().stringValue();
            boolean dynamic = meta.get("dynamic").isBoolean().booleanValue();
            int sectionSelector = Optional.ofNullable(meta.get("sectionSelector"))
                                          .map(JSONValue::isNumber)
                                          .map(JSONNumber::doubleValue)
                                          .map(Double::intValue)
                                          .orElse(0);
            boolean important = Optional.ofNullable(meta.get("important"))
                                        .map(JSONValue::isBoolean)
                                        .map(JSONBoolean::booleanValue)
                                        .orElse(false);
            boolean password = false;
            boolean credentials = false;
            boolean file = false;
            boolean textArea = false;

            if (metaType.equalsIgnoreCase("password")) {
                password = true;
            } else if (metaType.equalsIgnoreCase("fileBrowser")) {
                file = true;
            } else if (metaType.equalsIgnoreCase("credential")) {
                credentials = true;
            } else if (metaType.equalsIgnoreCase("textArea")) {
                textArea = true;
            }

            PluginDescriptor.Field f = new PluginDescriptor.Field(name,
                                                                  value,
                                                                  descr,
                                                                  password,
                                                                  credentials,
                                                                  file,
                                                                  textArea,
                                                                  dynamic,
                                                                  sectionSelector,
                                                                  important);

            desc.getConfigurableFields().add(f);
        }
        return desc;
    }

    public String wrapInfrastructureJsonString(String infrastructureJsonString) {

        JSONObject nodeSourceJsonObject = new JSONObject();

        // general parameters
        nodeSourceJsonObject.put("nodeSourceName", new JSONString(""));
        nodeSourceJsonObject.put("nodesRecoverable", JSONBoolean.getInstance(false));

        // infrastructure
        JSONObject infrastructureJsonObject;
        try {
            infrastructureJsonObject = parseJSON(infrastructureJsonString).isObject();
        } catch (JSONException e) {
            throw new IllegalArgumentException(JSON_FILE_ERROR_MESSAGE, e);
        }
        nodeSourceJsonObject.put("infrastructurePluginDescriptor", infrastructureJsonObject);

        // policy
        JSONObject policyJsonObject = createEmptyPlugin();
        nodeSourceJsonObject.put("policyPluginDescriptor", policyJsonObject);

        return nodeSourceJsonObject.toString();
    }

    public String wrapPolicyJsonString(String importedPolicyJsonString) {
        JSONObject nodeSourceJsonObject = new JSONObject();

        // general parameters
        nodeSourceJsonObject.put("nodeSourceName", new JSONString(""));
        nodeSourceJsonObject.put("nodesRecoverable", JSONBoolean.getInstance(false));

        // infrastructure
        JSONObject policyJsonObject = createEmptyPlugin();
        nodeSourceJsonObject.put("infrastructurePluginDescriptor", policyJsonObject);

        // infrastructure
        JSONObject infrastructureJsonObject;
        try {
            infrastructureJsonObject = parseJSON(importedPolicyJsonString).isObject();
        } catch (JSONException e) {
            throw new IllegalArgumentException(JSON_FILE_ERROR_MESSAGE, e);
        }
        nodeSourceJsonObject.put("policyPluginDescriptor", infrastructureJsonObject);

        return nodeSourceJsonObject.toString();
    }

    private JSONObject createEmptyPlugin() {
        JSONObject emptyPlugin = new JSONObject();
        emptyPlugin.put("pluginName", new JSONString(""));
        emptyPlugin.put("pluginDescription", new JSONString(""));
        emptyPlugin.put("configurableFields", new JSONArray());
        emptyPlugin.put("sectionDescriptions", new JSONObject());
        emptyPlugin.put("defaultValues", new JSONObject());
        return emptyPlugin;
    }

}
