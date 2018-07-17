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

import static org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.edition.InlineItemModificationCreator.EDIT_FORM_ITEM_SUFFIX;

import java.util.Collection;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.stream.Collectors;

import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSourceConfiguration;
import org.ow2.proactive_grid_cloud_portal.rm.client.PluginDescriptor;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMController;
import org.ow2.proactive_grid_cloud_portal.rm.shared.NodeSourceAction;

import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.HiddenItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
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
    protected void modifyFormItemsAfterCreation() {
        this.controller.fetchNodeSourceConfiguration(this.nodeSourceName, () -> {
            NodeSourceConfiguration nodeSourceConfiguration = this.controller.getModel()
                                                                             .getEditedNodeSourceConfiguration();
            this.nodeSourceNameText.setDefaultValue(nodeSourceConfiguration.getNodeSourceName());
            this.nodeSourceNameText.disable();
            this.nodesRecoverableCheckbox.setValue(nodeSourceConfiguration.getNodesRecoverable());
            this.nodesRecoverableCheckbox.disable();

            fillPluginFormItems(nodeSourceConfiguration);
            disablePluginFormItems(nodeSourceConfiguration);

            this.generalParametersLabel.setStyleName("generalParametersStyleDisabled");
            this.generalParametersLabel.redraw();
        }, this.window::hide);
    }

    private void disablePluginFormItems(NodeSourceConfiguration nodeSourceConfiguration) {
        PluginDescriptor focusedInfrastructurePlugin = nodeSourceConfiguration.getInfrastructurePluginDescriptor();
        PluginDescriptor focusedPolicyPlugin = nodeSourceConfiguration.getPolicyPluginDescriptor();

        List<String> allDynamicFieldFullNames = focusedInfrastructurePlugin.getConfigurableFields()
                                                                           .stream()
                                                                           .filter(PluginDescriptor.Field::isDynamic)
                                                                           .map(field -> focusedInfrastructurePlugin.getPluginName() +
                                                                                         field.getName())
                                                                           .collect(Collectors.toList());
        allDynamicFieldFullNames.addAll(focusedPolicyPlugin.getConfigurableFields()
                                                           .stream()
                                                           .filter(PluginDescriptor.Field::isDynamic)
                                                           .map(field -> focusedPolicyPlugin.getPluginName() +
                                                                         field.getName())
                                                           .collect(Collectors.toList()));

        for (FormItem formItem : this.formItemsByName.values()
                                                     .stream()
                                                     .flatMap(Collection::stream)
                                                     .collect(Collectors.toList())) {
            if (allDynamicFieldFullNames.stream()
                                        .noneMatch(fieldFullName -> isItemEqualToDynamicField(formItem,
                                                                                              fieldFullName))) {
                filterAndDisableNonDynamicItem(formItem);
            }
        }
    }

    private boolean isItemEqualToDynamicField(FormItem formItem, String fieldFullName) {
        return formItem.getName().equals(fieldFullName) ||
               formItem.getName().equals(fieldFullName + EDIT_FORM_ITEM_SUFFIX);
    }

    private void filterAndDisableNonDynamicItem(FormItem formItem) {
        if (this.formItemsByName.keySet().stream().anyMatch(pluginName -> formItem.getName().startsWith(pluginName)) ||
            formItem.getName().equals(INFRASTRUCTURE_FORM_KEY) || formItem.getName().equals(POLICY_FORM_KEY)) {
            disableNonDynamicItem(formItem);
        }
    }

    private void disableNonDynamicItem(FormItem formItem) {
        formItem.disable();
        // when a form item is disabled, it will not be given as parameter of
        // the submitted form, so we need to add the item content in a hidden
        // item for it to be submitted with the form. This is true for all
        // form items but the SelectItem (they are submitted as part of the
        // form even if disabled), this is why we make a special case.
        if (!(formItem instanceof SelectItem)) {
            HiddenItem hiddenItem = new HiddenItem(formItem.getName());
            hiddenItem.setValue(formItem.getValue());
            this.formItemsByName.put(formItem.getName(), Collections.singletonList(hiddenItem));
        }
    }

    @Override
    protected void addButtonsToButtonsLayout(HLayout buttonsLayout) {
        buttonsLayout.setMembers(this.applyModificationsButton, this.cancelButton);
    }

}
