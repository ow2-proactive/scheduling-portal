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

import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;

import org.ow2.proactive_grid_cloud_portal.rm.client.PluginDescriptor;

import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.RadioGroupItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.UploadItem;


public class NonTextualItemAternativeChoiceCreator {

    public static final String EDIT_OR_UPLOAD_FORM_ITEM_SUFFIX = ".modify";

    public static final String EDIT_FORM_ITEM_SUFFIX = ".edit";

    public static final String EDIT_RADIO_OPTION_NAME = "edit";

    public static final String UPLOAD_RADIO_OPTION_NAME = "upload";

    private final NodeSourceWindow nodeSourceWindow;

    public NonTextualItemAternativeChoiceCreator(NodeSourceWindow nodeSourceWindow) {
        this.nodeSourceWindow = nodeSourceWindow;
    }

    List<FormItem> getModificationChoiceItemsForNonTextualFields(PluginDescriptor plugin,
            PluginDescriptor.Field pluginField) {
        FormItem chooseCredentialsFormItem;
        List<FormItem> formItemsReplacingNonTextualFormItems = new LinkedList<FormItem>();

        if (plugin.getPluginName().equals(nodeSourceWindow.getFocusedInfrastructurePluginName()) ||
            plugin.getPluginName().equals(nodeSourceWindow.getFocusedPolicyPluginName())) {

            RadioGroupItem editOrUploadFormItem = createRadioItemToModifyPluginField(plugin, pluginField);
            formItemsReplacingNonTextualFormItems.add(editOrUploadFormItem);

            TextAreaItem previousValueItem = createTextItemPrefilledWithFileContent(plugin, pluginField);
            formItemsReplacingNonTextualFormItems.add(previousValueItem);

            chooseCredentialsFormItem = createUploadItemDisabled(plugin, pluginField);

        } else {

            chooseCredentialsFormItem = new UploadItem(plugin.getPluginName() + pluginField.getName(),
                                                       pluginField.getName());
        }

        nodeSourceWindow.addCredentialsPickerIcon(pluginField,
                                                  formItemsReplacingNonTextualFormItems,
                                                  chooseCredentialsFormItem);

        return formItemsReplacingNonTextualFormItems;
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

        previousValueItem.setDefaultValue(pluginField.getValue());

        return previousValueItem;
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
            List<FormItem> formItemsForPlugin = this.nodeSourceWindow.allFormItemsPerPlugin.get(plugin.getPluginName());

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

}
