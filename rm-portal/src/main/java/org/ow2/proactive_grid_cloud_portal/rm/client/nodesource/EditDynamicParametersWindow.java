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

import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSourceAction;
import org.ow2.proactive_grid_cloud_portal.rm.client.PluginDescriptor;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMController;

import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.HiddenItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.layout.HLayout;


public class EditDynamicParametersWindow extends EditNodeSourceWindow {

    public static final String WINDOW_TITLE = "Update Dynamic Parameters";

    public EditDynamicParametersWindow(RMController controller, String nodeSourceName) {
        super(controller, nodeSourceName, WINDOW_TITLE);
        buildForm();
    }

    @Override
    protected NodeSourceAction getNodeSourceAction() {
        return NodeSourceAction.UPDATE;
    }

    @Override
    protected void populateFormValues(Label windowLabel, DynamicForm windowForm, TextItem nodeSourceNameItem,
            CheckboxItem nodesRecoverableItem) {
        fetchNodeSourceConfigurationWithCallback(windowLabel, windowForm, nodeSourceNameItem, nodesRecoverableItem);
    }

    @Override
    protected List<FormItem> handleNonTextualPluginField(PluginDescriptor plugin, PluginDescriptor.Field pluginField) {

        List<FormItem> formItemsReplacingNonTextualFormItems = new LinkedList<>();

        TextAreaItem previousValueItem = createTextItemPrefilledWithFileContent(plugin, pluginField);
        formItemsReplacingNonTextualFormItems.add(previousValueItem);

        return formItemsReplacingNonTextualFormItems;
    }

    private TextAreaItem createTextItemPrefilledWithFileContent(PluginDescriptor plugin,
            PluginDescriptor.Field pluginField) {

        TextAreaItem itemWithFileContent = new TextAreaItem(plugin.getPluginName() + pluginField.getName() +
                                                            EDIT_FORM_ITEM_SUFFIX, pluginField.getName());

        itemWithFileContent.setDefaultValue(pluginField.getValue());
        return itemWithFileContent;
    }

    @Override
    protected void handleAdditionalPolicyFormItems(Map<String, String> selectItemValues,
            PluginDescriptor focusedPolicyPlugin) {
        // do nothing
    }

    @Override
    protected void handleAdditionalInfrastructureFormItems(Map<String, String> selectItemValues,
            PluginDescriptor focusedInfrastructurePlugin) {
        // do nothing
    }

    @Override
    protected List<FormItem> modifyFormItemsAfterCreation(PluginDescriptor focusedInfrastructurePlugin,
            PluginDescriptor focusedPolicyPlugin) {

        List<PluginDescriptor.Field> policyDynamicFields = focusedPolicyPlugin.getConfigurableFields()
                                                                              .stream()
                                                                              .filter(PluginDescriptor.Field::isDynamic)
                                                                              .collect(Collectors.toList());

        List<PluginDescriptor.Field> infrastructureDynamicIFields = focusedInfrastructurePlugin.getConfigurableFields()
                                                                                               .stream()
                                                                                               .filter(PluginDescriptor.Field::isDynamic)
                                                                                               .collect(Collectors.toList());
        List<PluginDescriptor.Field> allDynamicFields = new LinkedList<>();
        allDynamicFields.addAll(policyDynamicFields);
        allDynamicFields.addAll(infrastructureDynamicIFields);

        List<FormItem> allFormItemsWithHiddenFields = new LinkedList<>();

        for (FormItem formItem : this.allFormItems) {
            allFormItemsWithHiddenFields.add(formItem);
            if (allDynamicFields.stream()
                                .noneMatch(field -> isPartOfDynamicFields(focusedInfrastructurePlugin,
                                                                          focusedPolicyPlugin,
                                                                          formItem,
                                                                          field))) {

                if (this.allFormItemsPerPlugin.keySet()
                                              .stream()
                                              .anyMatch(pluginName -> formItem.getName().startsWith(pluginName)) ||
                    formItem.getName().equals(INFRASTRUCTURE_FORM_KEY) || formItem.getName().equals(POLICY_FORM_KEY)) {

                    disableNonDynamicItem(allFormItemsWithHiddenFields, formItem);
                }
            }
        }

        return allFormItemsWithHiddenFields;
    }

    private boolean isPartOfDynamicFields(PluginDescriptor focusedInfrastructurePlugin,
            PluginDescriptor focusedPolicyPlugin, FormItem formItem, PluginDescriptor.Field field) {

        return formItem.getName().equals(focusedPolicyPlugin.getPluginName() + field.getName()) ||
               formItem.getName().equals(focusedInfrastructurePlugin.getPluginName() + field.getName()) ||
               formItem.getName()
                       .equals(focusedPolicyPlugin.getPluginName() + field.getName() + EDIT_FORM_ITEM_SUFFIX) ||
               formItem.getName()
                       .equals(focusedInfrastructurePlugin.getPluginName() + field.getName() + EDIT_FORM_ITEM_SUFFIX);
    }

    @Override
    protected void addButtonsToButtonsLayout(HLayout buttonsLayout) {
        buttonsLayout.setMembers(this.applyModificationsButton, this.cancelButton);
    }

    @Override
    protected void manageNodeSourceWindowItems(TextItem nodeSourceNameItem, CheckboxItem nodesRecoverableItem) {
        super.manageNodeSourceWindowItems(nodeSourceNameItem, nodesRecoverableItem);
        nodesRecoverableItem.disable();
    }

    private void disableNonDynamicItem(List<FormItem> allFormItemsWithHiddenFields, FormItem formItem) {

        formItem.disable();

        // since the form item is disabled, we need to add the item content in
        // a hidden item for it to be submitted with the form
        HiddenItem hiddenItem = new HiddenItem(formItem.getName());
        hiddenItem.setValue(formItem.getValue());
        allFormItemsWithHiddenFields.add(hiddenItem);
    }

}
