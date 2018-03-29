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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.SpacerItem;
import com.smartgwt.client.widgets.form.fields.TextItem;


/**
 * Dialog window to edit a node source. Downloads dynamically the information 
 * of the current node source, and downloads dynamically all the supported
 * infrastructures and policies when the window is created.
 */
public class NodeSourceEditWindow extends NodeSourceWindow {

    private String nodeSourceName;

    public NodeSourceEditWindow(RMController controller, String nodeSourceName) {
        super(controller, "Edit node source", "Retrieving current node source configuration");
        this.nodeSourceName = nodeSourceName;
        this.buildForm();
    }

    @Override
    protected boolean isNodeSourceEdited() {
        return true;
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

    private void fetchNodeSourceConfigurationWithCallback(Label windowLabel, DynamicForm windowForm,
            TextItem nodeSourceNameItem, CheckboxItem nodesRecoverableItem) {

        this.controller.fetchNodeSourceConfiguration(this.nodeSourceName, () -> {

            NodeSourceConfiguration nodeSourceConfiguration = controller.getModel().getEditedNodeSourceConfiguration();

            nodeSourceNameItem.setDefaultValue(nodeSourceConfiguration.getNodeSourceName());
            // we do not allow the node source name to be modified
            nodeSourceNameItem.disable();

            nodesRecoverableItem.setValue(nodeSourceConfiguration.getNodesRecoverable());

            HashMap<String, List<FormItem>> allFormItemsPerPlugin = new HashMap<>();
            LinkedHashMap<String, String> selectItemValues = new LinkedHashMap<>();

            ArrayList<FormItem> allFormItems = prepareFormItems();

            PluginDescriptor focusedInfrastructurePlugin = nodeSourceConfiguration.getInfrastructurePluginDescriptor();
            Map<String, PluginDescriptor> allSupportedInfrastructures = controller.getModel()
                                                                                  .getSupportedInfrastructures();

            allFormItems.add(infrastructureSelectItem);
            addAllPluginValuesToAllFormItems(allFormItemsPerPlugin,
                                             allFormItems,
                                             selectItemValues,
                                             focusedInfrastructurePlugin,
                                             allSupportedInfrastructures);
            infrastructureSelectItem.setValueMap(selectItemValues);
            infrastructureSelectItem.setDefaultToFirstOption(true);
            previousSelectedInfrastructure = focusedInfrastructurePlugin.getPluginName();

            allFormItems.add(new SpacerItem());
            selectItemValues.clear();

            PluginDescriptor focusedPolicyPlugin = nodeSourceConfiguration.getPolicyPluginDescriptor();
            Map<String, PluginDescriptor> allSupportedPolicies = controller.getModel().getSupportedPolicies();

            allFormItems.add(policySelectItem);
            addAllPluginValuesToAllFormItems(allFormItemsPerPlugin,
                                             allFormItems,
                                             selectItemValues,
                                             focusedPolicyPlugin,
                                             allSupportedPolicies);
            policySelectItem.setValueMap(selectItemValues);
            policySelectItem.setDefaultToFirstOption(true);
            previousSelectedPolicy = focusedPolicyPlugin.getPluginName();

            infrastructureSelectItem.addChangedHandler(changedEvent -> resetFormForInfrastructureSelectChange(allFormItemsPerPlugin));
            policySelectItem.addChangedHandler(changedEvent -> resetFormForPolicySelectChange(allFormItemsPerPlugin));

            windowForm.setFields(allFormItems.toArray(new FormItem[allFormItems.size()]));
            windowLabel.hide();
            windowForm.show();

            hideNotFocusedFormItems(allFormItemsPerPlugin, focusedInfrastructurePlugin, focusedPolicyPlugin);

        }, () -> window.hide());
    }

    private void hideNotFocusedFormItems(HashMap<String, List<FormItem>> allFormItemsPerPlugin,
            PluginDescriptor focusedInfrastructurePlugin, PluginDescriptor focusedPolicyPlugin) {

        for (Map.Entry<String, List<FormItem>> entry : allFormItemsPerPlugin.entrySet()) {

            String pluginName = entry.getKey();
            List<FormItem> formItemsForPlugin = entry.getValue();

            if (!pluginName.equals(focusedInfrastructurePlugin.getPluginName()) &&
                !pluginName.equals(focusedPolicyPlugin.getPluginName())) {

                for (FormItem formItem : formItemsForPlugin) {
                    formItem.hide();
                }
            }
        }
    }

    private void addAllPluginValuesToAllFormItems(HashMap<String, List<FormItem>> allFormItemsPerPlugin,
            ArrayList<FormItem> allFormItems, LinkedHashMap<String, String> selectItemValues,
            PluginDescriptor focusedPlugin, Map<String, PluginDescriptor> allPluginDescriptors) {

        String shortName = getPluginShortName(focusedPlugin);
        selectItemValues.put(focusedPlugin.getPluginName(), shortName);

        ArrayList<FormItem> pluginFormItems = getPrefilledFormItems(focusedPlugin);
        allFormItems.addAll(pluginFormItems);
        allFormItemsPerPlugin.put(focusedPlugin.getPluginName(), pluginFormItems);

        addPluginValuesToAllFormItemOtherThanFocused(allFormItemsPerPlugin,
                                                     allFormItems,
                                                     selectItemValues,
                                                     focusedPlugin,
                                                     allPluginDescriptors);
    }

    private void addPluginValuesToAllFormItemOtherThanFocused(HashMap<String, List<FormItem>> allFormItemsPerPlugin,
            ArrayList<FormItem> allFormItems, LinkedHashMap<String, String> selectItemValues,
            PluginDescriptor focusedPlugin, Map<String, PluginDescriptor> allPluginDescriptors) {

        for (Map.Entry<String, PluginDescriptor> entry : allPluginDescriptors.entrySet()) {

            PluginDescriptor plugin = entry.getValue();

            if (!plugin.getPluginName().equals(focusedPlugin.getPluginName())) {

                selectItemValues.put(plugin.getPluginName(), getPluginShortName(plugin));
                ArrayList<FormItem> currentPluginFormItems = getPrefilledFormItems(plugin);
                allFormItems.addAll(currentPluginFormItems);
                allFormItemsPerPlugin.put(plugin.getPluginName(), currentPluginFormItems);
            }
        }
    }

}
