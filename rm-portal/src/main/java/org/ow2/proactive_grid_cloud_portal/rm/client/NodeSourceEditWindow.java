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

import java.util.*;

import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.*;


public class NodeSourceEditWindow extends NodeSourceWindow {

    private String nodeSourceName;

    public NodeSourceEditWindow(RMController controller, String nodeSourceName) {
        super(controller, "Edit node source", "Retrieving current node source configuration");
        this.nodeSourceName = nodeSourceName;
        this.buildForm();
    }

    @Override
    protected void populateFormValues(Label windowLabel, DynamicForm windowForm, TextItem nodeSourceNameItem,
            CheckboxItem nodesRecoverableItem) {
        this.controller.fetchSupportedInfrastructuresAndPolicies(() -> fetchNodeSourceConfigurationWithCallback(windowLabel,
                                                                                                                windowForm,
                                                                                                                nodeSourceNameItem,
                                                                                                                nodesRecoverableItem),
                                                                 () -> window.hide());
    }

    @Override
    protected boolean isNodeSourceEdited() {
        return true;
    }

    private void fetchNodeSourceConfigurationWithCallback(Label windowLabel, DynamicForm windowForm,
            TextItem nodeSourceNameItem, CheckboxItem nodesRecoverableItem) {
        this.controller.fetchNodeSourceConfiguration(this.nodeSourceName, () -> {

            NodeSourceConfiguration nodeSourceConfiguration = controller.getModel().getEditedNodeSourceConfiguration();

            nodeSourceNameItem.setDefaultValue(nodeSourceConfiguration.getNodeSourceName());
            nodeSourceNameItem.disable();

            nodesRecoverableItem.setValue(nodeSourceConfiguration.getNodesRecoverable());

            HashMap<String, List<FormItem>> formParametersMap = new HashMap<>();

            ArrayList<FormItem> formParameters = prepareFormParameters();

            LinkedHashMap<String, String> selectItemValues = new LinkedHashMap<>();

            PluginDescriptor infrastructurePluginDescriptor = nodeSourceConfiguration.getInfrastructurePluginDescriptor();
            formParameters.add(infraSelect);
            addAllFormValues(formParametersMap,
                             formParameters,
                             selectItemValues,
                             infrastructurePluginDescriptor,
                             controller.getModel().getSupportedInfrastructures());
            infraSelect.setValueMap(selectItemValues);
            infraSelect.setDefaultToFirstOption(true);
            oldInfra = infrastructurePluginDescriptor.getPluginName();

            formParameters.add(new SpacerItem());
            selectItemValues.clear();

            PluginDescriptor policyPluginDescriptor = nodeSourceConfiguration.getPolicyPluginDescriptor();
            formParameters.add(policySelect);
            addAllFormValues(formParametersMap,
                             formParameters,
                             selectItemValues,
                             policyPluginDescriptor,
                             controller.getModel().getSupportedPolicies());
            policySelect.setValueMap(selectItemValues);
            policySelect.setDefaultToFirstOption(true);
            oldPolicy = policyPluginDescriptor.getPluginName();

            infraSelect.addChangedHandler(changedEvent -> {
                resetFormForInfrastructureChange(formParametersMap);
            });
            policySelect.addChangedHandler(changedEvent -> {
                resetFormForPolicyChange(formParametersMap);
            });

            windowForm.setFields(formParameters.toArray(new FormItem[formParameters.size()]));
            windowLabel.hide();
            windowForm.show();

            for (Map.Entry<String, List<FormItem>> entry : formParametersMap.entrySet()) {
                String pluginName = entry.getKey();
                if (!pluginName.equals(infrastructurePluginDescriptor.getPluginName()) &&
                    !pluginName.equals(policyPluginDescriptor.getPluginName())) {
                    for (FormItem item : entry.getValue()) {
                        item.hide();
                    }
                }
            }

        }, () -> window.hide());
    }

    private void addAllFormValues(HashMap<String, List<FormItem>> formParametersMap, ArrayList<FormItem> formParameters,
            LinkedHashMap<String, String> selectListValues, PluginDescriptor pluginDescriptor,
            Map<String, PluginDescriptor> supportedPlugins) {
        String shortName = getPluginShortName(pluginDescriptor);
        selectListValues.put(pluginDescriptor.getPluginName(), shortName);

        ArrayList<FormItem> pluginFormItems = getPrefilledFormItems(pluginDescriptor);
        formParameters.addAll(pluginFormItems);
        formParametersMap.put(pluginDescriptor.getPluginName(), pluginFormItems);
        addPluginDescriptorsToValues(formParametersMap,
                                     formParameters,
                                     selectListValues,
                                     pluginDescriptor,
                                     supportedPlugins);
    }

    private void addPluginDescriptorsToValues(HashMap<String, List<FormItem>> formParametersMap,
            ArrayList<FormItem> formParameters, LinkedHashMap<String, String> selectItemValues,
            PluginDescriptor excludedPluginDescriptor, Map<String, PluginDescriptor> supportedPlugins) {
        for (Map.Entry<String, PluginDescriptor> entry : supportedPlugins.entrySet()) {
            PluginDescriptor pluginDescriptor = entry.getValue();
            if (!pluginDescriptor.getPluginName().equals(excludedPluginDescriptor.getPluginName())) {
                selectItemValues.put(pluginDescriptor.getPluginName(), getPluginShortName(pluginDescriptor));
                ArrayList<FormItem> currentPluginFormItems = getPrefilledFormItems(pluginDescriptor);
                formParameters.addAll(currentPluginFormItems);
                formParametersMap.put(pluginDescriptor.getPluginName(), currentPluginFormItems);
            }
        }
    }

}
