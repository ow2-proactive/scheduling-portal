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
package org.ow2.proactive_grid_cloud_portal.rm.client.nodesource;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSourceConfiguration;
import org.ow2.proactive_grid_cloud_portal.rm.client.PluginDescriptor;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMController;

import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.RadioGroupItem;
import com.smartgwt.client.widgets.form.fields.SpacerItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.UploadItem;


/**
 * Dialog window to edit a node source. Downloads dynamically the information 
 * of the current node source, and downloads dynamically all the supported
 * infrastructures and policies when the window is created.
 */
public class NodeSourceEditWindow extends NodeSourceWindow {

    public static final String EDIT_OR_UPLOAD_FORM_ITEM_SUFFIX = ".modify";

    public static final String EDIT_FORM_ITEM_SUFFIX = ".edit";

    public static final String EDIT_RADIO_OPTION_NAME = "edit";

    public static final String UPLOAD_RADIO_OPTION_NAME = "upload";

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
                                                                 this.window::hide);
    }

    @Override
    protected List<FormItem> handleNonTextualPluginField(PluginDescriptor plugin, PluginDescriptor.Field pluginField) {

        FormItem chooseCredentialsFormItem;
        List<FormItem> formItems = new LinkedList<>();

        if (plugin.getPluginName().equals(this.focusedInfrastructurePluginName) ||
            plugin.getPluginName().equals(this.focusedPolicyPluginName)) {

            RadioGroupItem editOrUploadFormItem = createRadioItemToModifyPluginField(plugin, pluginField);
            formItems.add(editOrUploadFormItem);

            TextAreaItem previousValueItem = createTextItemPrefilledWithFileContent(plugin, pluginField);
            formItems.add(previousValueItem);

            chooseCredentialsFormItem = createUploadItemDisabled(plugin, pluginField);

        } else {

            chooseCredentialsFormItem = new UploadItem(plugin.getPluginName() + pluginField.getName(),
                                                       pluginField.getName());
        }

        addCredentialsPickerIcon(pluginField, formItems, chooseCredentialsFormItem);

        return formItems;
    }

    private FormItem createUploadItemDisabled(PluginDescriptor plugin, PluginDescriptor.Field pluginField) {

        FormItem chooseCredentialsFormItem;
        chooseCredentialsFormItem = new UploadItem(plugin.getPluginName() + pluginField.getName(), "");
        chooseCredentialsFormItem.disable();
        return chooseCredentialsFormItem;
    }

    private TextAreaItem createTextItemPrefilledWithFileContent(PluginDescriptor plugin,
            PluginDescriptor.Field pluginField) {

        TextAreaItem previousValueItem = new TextAreaItem(plugin.getPluginName() + pluginField.getName() +
                                                          EDIT_FORM_ITEM_SUFFIX, "");

        if (isBlank(pluginField.getValue())) {
            previousValueItem.setDefaultValue("toto");
        } else {
            previousValueItem.setDefaultValue(pluginField.getValue());
        }
        return previousValueItem;
    }

    private boolean isBlank(String text) {
        return text == null || text.trim().length() == 0;
    }

    private RadioGroupItem createRadioItemToModifyPluginField(PluginDescriptor plugin,
            PluginDescriptor.Field pluginField) {

        String formItemPrefixName = plugin.getPluginName() + pluginField.getName();

        LinkedHashMap<String, String> radioOptions = new LinkedHashMap<>();
        radioOptions.put(EDIT_RADIO_OPTION_NAME, "In-line editing");
        radioOptions.put(UPLOAD_RADIO_OPTION_NAME, "Upload new file");

        RadioGroupItem editOrUploadFormItem = new RadioGroupItem(formItemPrefixName + EDIT_OR_UPLOAD_FORM_ITEM_SUFFIX,
                                                                 pluginField.getName());
        editOrUploadFormItem.setValueMap(radioOptions);
        editOrUploadFormItem.setVertical(false);
        editOrUploadFormItem.setDefaultValue(EDIT_RADIO_OPTION_NAME);

        editOrUploadFormItem.addChangedHandler(changedEvent -> {

            String radioValue = changedEvent.getValue().toString();
            List<FormItem> formItemsForPlugin = this.allFormItemsPerPlugin.get(plugin.getPluginName());

            if (radioValue.equals(EDIT_RADIO_OPTION_NAME)) {
                enableEditInLine(formItemPrefixName, formItemsForPlugin);
            } else if (radioValue.equals(UPLOAD_RADIO_OPTION_NAME)) {
                enableUploadNewFile(formItemPrefixName, formItemsForPlugin);
            }
        });

        return editOrUploadFormItem;
    }

    private void enableUploadNewFile(String pluginFieldName, List<FormItem> formItemsForPlugin) {

        formItemsForPlugin.stream()
                          .filter(formItem -> formItem.getName().startsWith(pluginFieldName))
                          .forEach(formItem -> enableUploadNewFile(pluginFieldName, formItem));
    }

    private void enableUploadNewFile(String pluginFieldName, FormItem formItem) {

        if (formItem.getName().equals(pluginFieldName)) {
            formItem.enable();

        } else if (formItem.getName().startsWith(pluginFieldName) &&
                   formItem.getName().endsWith(EDIT_FORM_ITEM_SUFFIX)) {
            formItem.disable();
        }
    }

    private void enableEditInLine(String pluginFieldName, List<FormItem> formItemsForPlugin) {

        formItemsForPlugin.stream()
                          .filter(formItem -> formItem.getName().startsWith(pluginFieldName))
                          .forEach(formItem -> enableEditInLine(pluginFieldName, formItem));
    }

    private void enableEditInLine(String pluginFieldName, FormItem formItem) {

        if (formItem.getName().equals(pluginFieldName)) {
            formItem.disable();

        } else if (formItem.getName().startsWith(pluginFieldName) &&
                   formItem.getName().endsWith(EDIT_FORM_ITEM_SUFFIX)) {
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

        }, this.window::hide);
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
