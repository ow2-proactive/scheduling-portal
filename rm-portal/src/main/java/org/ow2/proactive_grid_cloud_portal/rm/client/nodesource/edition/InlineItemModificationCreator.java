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
package org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.edition;

import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;

import org.ow2.proactive_grid_cloud_portal.rm.client.PluginDescriptor;
import org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.NodeSourceWindow;

import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.RadioGroupItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.UploadItem;


public class InlineItemModificationCreator {

    public static final String EDIT_OR_UPLOAD_FORM_ITEM_SUFFIX = ".modify";

    public static final String EDIT_FORM_ITEM_SUFFIX = ".edit";

    private static final String EDIT_RADIO_OPTION_NAME = "edit";

    private static final String UPLOAD_RADIO_OPTION_NAME = "upload";

    private final NodeSourceWindow nodeSourceWindow;

    private FormItem inlineEditingFormItem;

    private FormItem chooseCredentialsFormItem;

    private RadioGroupItem editOrUploadFormItem;

    public InlineItemModificationCreator(NodeSourceWindow nodeSourceWindow) {
        this.nodeSourceWindow = nodeSourceWindow;
    }

    public List<FormItem> getModificationChoiceItemsForNonTextualFields(PluginDescriptor plugin,
            PluginDescriptor.Field pluginField) {
        List<FormItem> formItemsReplacingNonTextualFormItems = new LinkedList<>();
        if (plugin.getPluginName().equals(this.nodeSourceWindow.infrastructureSelectItem.getValueAsString()) ||
            plugin.getPluginName().equals(this.nodeSourceWindow.policySelectItem.getValueAsString())) {
            this.inlineEditingFormItem = new TextAreaItem(plugin.getPluginName() + pluginField.getName() +
                                                          EDIT_FORM_ITEM_SUFFIX, "");
            this.inlineEditingFormItem.setValue(pluginField.getValue());
            this.chooseCredentialsFormItem = new UploadItem(plugin.getPluginName() + pluginField.getName(), "");
            this.chooseCredentialsFormItem.disable();
            createRadioItemToModifyPluginField(plugin, pluginField);

            formItemsReplacingNonTextualFormItems.add(this.editOrUploadFormItem);
            formItemsReplacingNonTextualFormItems.add(this.inlineEditingFormItem);
        } else {
            this.chooseCredentialsFormItem = new UploadItem(plugin.getPluginName() + pluginField.getName(),
                                                            pluginField.getName());
        }
        this.nodeSourceWindow.addCredentialsPickerIcon(pluginField,
                                                       formItemsReplacingNonTextualFormItems,
                                                       this.chooseCredentialsFormItem);
        return formItemsReplacingNonTextualFormItems;
    }

    private void createRadioItemToModifyPluginField(PluginDescriptor plugin, PluginDescriptor.Field pluginField) {
        String formItemPrefixName = plugin.getPluginName() + pluginField.getName();
        LinkedHashMap<String, String> radioOptions = new LinkedHashMap<>();
        radioOptions.put(EDIT_RADIO_OPTION_NAME, "In-line editing");
        radioOptions.put(UPLOAD_RADIO_OPTION_NAME, "Upload new file");
        this.editOrUploadFormItem = new RadioGroupItem(formItemPrefixName + EDIT_OR_UPLOAD_FORM_ITEM_SUFFIX,
                                                       pluginField.getName());
        this.editOrUploadFormItem.setVertical(false);
        this.editOrUploadFormItem.setValueMap(radioOptions);
        this.editOrUploadFormItem.setDefaultValue(EDIT_RADIO_OPTION_NAME);
        this.editOrUploadFormItem.addChangedHandler(changedEvent -> enableRightItem());
    }

    private void enableRightItem() {
        if (this.editOrUploadFormItem.getValueAsString().equals(EDIT_RADIO_OPTION_NAME)) {
            this.inlineEditingFormItem.enable();
            this.chooseCredentialsFormItem.disable();
        } else if (this.editOrUploadFormItem.getValueAsString().equals(UPLOAD_RADIO_OPTION_NAME)) {
            this.chooseCredentialsFormItem.enable();
            this.inlineEditingFormItem.disable();
        }
    }

}
