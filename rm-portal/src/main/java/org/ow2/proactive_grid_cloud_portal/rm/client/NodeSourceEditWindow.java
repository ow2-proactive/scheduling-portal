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


/**
 * Dialog window to edit a node source. Downloads dynamically the information 
 * of the current node source, and downloads dynamically all the supported
 * infrastructures and policies when the window is created.
 */
public class NodeSourceEditWindow extends NodeSourceWindow {

    public static final String KEEP_OR_CHANGE_FORM_ITEM_SUFFIX = ".keepOrChange";

    public static final String KEEP_FORM_ITEM_SUFFIX = ".keep";

    public static final String KEEP_RADIO_OPTION_NAME = "keep";

    public static final String CHANGE_RADIO_OPTION_NAME = "change";

    private String nodeSourceName;

    private String focusedInfrastructurePluginName;

    private String focusedPolicyPluginName;

    public NodeSourceEditWindow(RMController controller, String nodeSourceName) {
        super(controller, "Edit node source", "Retrieving current node source configuration");
        this.nodeSourceName = nodeSourceName;
        buildForm();
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
                                                                 () -> this.window.hide());
    }

    @Override
    protected List<FormItem> handleNonTextualPluginField(PluginDescriptor plugin, PluginDescriptor.Field pluginField) {

        FormItem chooseCredentialsFormItem;
        List<FormItem> formItems = new LinkedList<>();

        if (plugin.getPluginName().equals(this.focusedInfrastructurePluginName) ||
            plugin.getPluginName().equals(this.focusedPolicyPluginName)) {

            LinkedHashMap<String, String> radioOptions = new LinkedHashMap<>();
            radioOptions.put(KEEP_RADIO_OPTION_NAME, "Keep previous");
            radioOptions.put(CHANGE_RADIO_OPTION_NAME, "Change");
            RadioGroupItem keepOrChangeFormItem = new RadioGroupItem(plugin.getPluginName() + pluginField.getName() +
                                                                     KEEP_OR_CHANGE_FORM_ITEM_SUFFIX,
                                                                     pluginField.getName());
            keepOrChangeFormItem.setValueMap(radioOptions);
            keepOrChangeFormItem.setVertical(false);
            keepOrChangeFormItem.setDefaultValue(KEEP_RADIO_OPTION_NAME);

            keepOrChangeFormItem.addChangedHandler(changedEvent -> {
                String radioValue = changedEvent.getValue().toString();
                List<FormItem> formItemsForPlugin = this.allFormItemsPerPlugin.get(plugin.getPluginName());
                formItemsForPlugin.stream()
                                  .filter(formItem -> formItem.getName()
                                                              .equals(plugin.getPluginName() + pluginField.getName()))
                                  .findFirst()
                                  .ifPresent(formItem -> enableFormItemIfChangeIsSelected(radioValue, formItem));
            });

            formItems.add(keepOrChangeFormItem);

            AutoFitTextAreaItem previousValueItem = new AutoFitTextAreaItem(plugin.getPluginName() +
                                                                            pluginField.getName() +
                                                                            KEEP_FORM_ITEM_SUFFIX, "");
            previousValueItem.setDefaultValue(pluginField.getValue());
            //previousValueItem.hide();
            formItems.add(previousValueItem);

            chooseCredentialsFormItem = new UploadItem(plugin.getPluginName() + pluginField.getName(), "");
            chooseCredentialsFormItem.disable();

        } else {
            chooseCredentialsFormItem = new UploadItem(plugin.getPluginName() + pluginField.getName(),
                                                       pluginField.getName());
        }

        addCredentialsPickerIcon(pluginField, formItems, chooseCredentialsFormItem);

        return formItems;
    }

    private void enableFormItemIfChangeIsSelected(String radioValue, FormItem formItem) {
        if (radioValue.equals(KEEP_RADIO_OPTION_NAME)) {
            formItem.disable();
        } else {
            formItem.enable();
        }
    }

    private void fetchNodeSourceConfigurationWithCallback(Label windowLabel, DynamicForm windowForm,
            TextItem nodeSourceNameItem, CheckboxItem nodesRecoverableItem) {

        this.controller.fetchNodeSourceConfiguration(this.nodeSourceName, () -> {

            NodeSourceConfiguration nodeSourceConfiguration = this.controller.getModel()
                                                                             .getEditedNodeSourceConfiguration();

            nodeSourceNameItem.setDefaultValue(nodeSourceConfiguration.getNodeSourceName());
            // we do not allow the node source name to be modified
            nodeSourceNameItem.disable();

            nodesRecoverableItem.setValue(nodeSourceConfiguration.getNodesRecoverable());

            LinkedHashMap<String, String> selectItemValues = new LinkedHashMap<>();

            ArrayList<FormItem> allFormItems = prepareFormItems();

            PluginDescriptor focusedInfrastructurePlugin = nodeSourceConfiguration.getInfrastructurePluginDescriptor();
            this.focusedInfrastructurePluginName = focusedInfrastructurePlugin.getPluginName();
            Map<String, PluginDescriptor> allSupportedInfrastructures = this.controller.getModel()
                                                                                       .getSupportedInfrastructures();

            allFormItems.add(this.infrastructureSelectItem);
            addAllPluginValuesToAllFormItems(allFormItems,
                                             selectItemValues,
                                             focusedInfrastructurePlugin,
                                             allSupportedInfrastructures);
            this.infrastructureSelectItem.setValueMap(selectItemValues);
            this.infrastructureSelectItem.setDefaultToFirstOption(true);
            this.previousSelectedInfrastructure = focusedInfrastructurePlugin.getPluginName();

            allFormItems.add(new SpacerItem());
            selectItemValues.clear();

            PluginDescriptor focusedPolicyPlugin = nodeSourceConfiguration.getPolicyPluginDescriptor();
            this.focusedPolicyPluginName = focusedPolicyPlugin.getPluginName();
            Map<String, PluginDescriptor> allSupportedPolicies = this.controller.getModel().getSupportedPolicies();

            allFormItems.add(this.policySelectItem);
            addAllPluginValuesToAllFormItems(allFormItems, selectItemValues, focusedPolicyPlugin, allSupportedPolicies);
            this.policySelectItem.setValueMap(selectItemValues);
            this.policySelectItem.setDefaultToFirstOption(true);
            this.previousSelectedPolicy = focusedPolicyPlugin.getPluginName();

            this.infrastructureSelectItem.addChangedHandler(changedEvent -> resetFormForInfrastructureSelectChange());
            this.policySelectItem.addChangedHandler(changedEvent -> resetFormForPolicySelectChange());

            windowForm.setFields(allFormItems.toArray(new FormItem[allFormItems.size()]));
            windowLabel.hide();
            windowForm.show();

            hideNotFocusedFormItems(focusedInfrastructurePlugin, focusedPolicyPlugin);

        }, () -> this.window.hide());
    }

    private void hideNotFocusedFormItems(PluginDescriptor focusedInfrastructurePlugin,
            PluginDescriptor focusedPolicyPlugin) {

        for (Map.Entry<String, List<FormItem>> entry : this.allFormItemsPerPlugin.entrySet()) {

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

    private void addAllPluginValuesToAllFormItems(ArrayList<FormItem> allFormItems,
            LinkedHashMap<String, String> selectItemValues, PluginDescriptor focusedPlugin,
            Map<String, PluginDescriptor> allPluginDescriptors) {

        String pluginShortName = getPluginShortName(focusedPlugin);
        selectItemValues.put(focusedPlugin.getPluginName(), pluginShortName);

        ArrayList<FormItem> pluginFormItems = getPrefilledFormItems(focusedPlugin);
        allFormItems.addAll(pluginFormItems);
        this.allFormItemsPerPlugin.put(focusedPlugin.getPluginName(), pluginFormItems);

        addPluginValuesToAllFormItemOtherThanFocused(allFormItems,
                                                     selectItemValues,
                                                     focusedPlugin,
                                                     allPluginDescriptors);
    }

    private void addPluginValuesToAllFormItemOtherThanFocused(ArrayList<FormItem> allFormItems,
            LinkedHashMap<String, String> selectItemValues, PluginDescriptor focusedPlugin,
            Map<String, PluginDescriptor> allPluginDescriptors) {

        for (Map.Entry<String, PluginDescriptor> entry : allPluginDescriptors.entrySet()) {

            PluginDescriptor plugin = entry.getValue();

            if (!plugin.getPluginName().equals(focusedPlugin.getPluginName())) {

                selectItemValues.put(plugin.getPluginName(), getPluginShortName(plugin));
                ArrayList<FormItem> currentPluginFormItems = getPrefilledFormItems(plugin);
                allFormItems.addAll(currentPluginFormItems);
                this.allFormItemsPerPlugin.put(plugin.getPluginName(), currentPluginFormItems);
            }
        }
    }

}
