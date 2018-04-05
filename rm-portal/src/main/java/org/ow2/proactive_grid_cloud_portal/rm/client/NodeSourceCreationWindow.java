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
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;

import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.SpacerItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.UploadItem;


/**
 * Dialog window to create a node source. Downloads dynamically supported
 * infrastructures and policies when the window is created.
 */
public class NodeSourceCreationWindow extends NodeSourceWindow {

    public NodeSourceCreationWindow(RMController controller) {
        super(controller, "Add Node Source", "Updating available Infrastructures and Policies");
        buildForm();
    }

    @Override
    protected boolean isNodeSourceEdited() {
        return false;
    }

    @Override
    protected void populateFormValues(Label windowLabel, DynamicForm windowForm, TextItem nodeSourceNameItem,
            CheckboxItem nodesRecoverableItem) {

        nodesRecoverableItem.setValue(true);

        this.controller.fetchSupportedInfrastructuresAndPolicies(() -> {

            LinkedHashMap<String, String> selectItemValues = new LinkedHashMap<>();

            ArrayList<FormItem> allFormItems = prepareFormItems();

            allFormItems.add(this.infrastructureSelectItem);
            addAllPluginValuesToAllFormItems(allFormItems,
                                             selectItemValues,
                                             this.controller.getModel().getSupportedInfrastructures().values());
            this.infrastructureSelectItem.setValueMap(selectItemValues);

            allFormItems.add(new SpacerItem());
            selectItemValues.clear();

            allFormItems.add(this.policySelectItem);
            addAllPluginValuesToAllFormItems(allFormItems,
                                             selectItemValues,
                                             this.controller.getModel().getSupportedPolicies().values());
            this.policySelectItem.setValueMap(selectItemValues);

            this.infrastructureSelectItem.addChangedHandler(changedEvent -> resetFormForInfrastructureSelectChange());
            this.policySelectItem.addChangedHandler(changedEvent -> resetFormForPolicySelectChange());

            windowForm.setFields(allFormItems.toArray(new FormItem[allFormItems.size()]));
            windowLabel.hide();
            windowForm.show();

            hideAllFormItems();

        }, this.window::hide);
    }

    @Override
    protected List<FormItem> handleNonTextualPluginField(PluginDescriptor plugin, PluginDescriptor.Field pluginField) {

        List<FormItem> formItems = new LinkedList<>();
        FormItem chooseCredentialsFormItem = new UploadItem(plugin.getPluginName() + pluginField.getName(),
                                                            pluginField.getName());
        addCredentialsPickerIcon(pluginField, formItems, chooseCredentialsFormItem);

        return formItems;
    }

    private void hideAllFormItems() {

        for (List<FormItem> li : this.allFormItemsPerPlugin.values()) {

            for (FormItem it : li) {
                it.hide();
            }
        }
    }

    private void addAllPluginValuesToAllFormItems(ArrayList<FormItem> allFormItems,
            LinkedHashMap<String, String> selectItemValues, Collection<PluginDescriptor> allPluginDescriptors) {

        for (PluginDescriptor pluginDescriptor : allPluginDescriptors) {

            String shortName = getPluginShortName(pluginDescriptor);
            selectItemValues.put(pluginDescriptor.getPluginName(), shortName);

            ArrayList<FormItem> currentPluginFormItems = getPrefilledFormItems(pluginDescriptor);
            allFormItems.addAll(currentPluginFormItems);
            this.allFormItemsPerPlugin.put(pluginDescriptor.getPluginName(), currentPluginFormItems);
        }
    }

}
