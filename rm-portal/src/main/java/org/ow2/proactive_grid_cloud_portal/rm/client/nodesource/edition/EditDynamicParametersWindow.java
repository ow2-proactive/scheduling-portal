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

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSourceConfiguration;
import org.ow2.proactive_grid_cloud_portal.rm.client.PluginDescriptor;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMController;
import org.ow2.proactive_grid_cloud_portal.rm.shared.NodeSourceAction;

import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.layout.HLayout;


public class EditDynamicParametersWindow extends EditNodeSourceWindow {

    public static final String WINDOW_TITLE = "Update Dynamic Parameters";

    public EditDynamicParametersWindow(RMController controller, String nodeSourceName) {
        super(controller, nodeSourceName, WINDOW_TITLE);
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
        itemWithFileContent.setValue(pluginField.getValue());
        return itemWithFileContent;
    }

    @Override
    protected void afterItemsCreation() {
        this.controller.fetchNodeSourceConfiguration(this.nodeSourceName, () -> {
            NodeSourceConfiguration nodeSourceConfiguration = this.controller.getModel()
                                                                             .getEditedNodeSourceConfiguration();
            this.nodeSourceNameText.setValue(nodeSourceConfiguration.getNodeSourceName());
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
        List<String> dynamicParametersName = Stream.concat(getDynamicParametersName(focusedInfrastructurePlugin),
                                                           getDynamicParametersName(focusedPolicyPlugin))
                                                   .collect(Collectors.toList());
        Arrays.stream(this.nodeSourcePluginsForm.getFields())
              .filter(formItem -> isNotDynamic(formItem.getName(), dynamicParametersName) &&
                                  isFocusedPluginItem(focusedInfrastructurePlugin, focusedPolicyPlugin, formItem))
              .filter(formItem -> !(formItem instanceof StaticTextItem))
              .forEach(FormItem::disable);
    }

    private Stream<String> getDynamicParametersName(PluginDescriptor focusedInfrastructurePlugin) {
        return focusedInfrastructurePlugin.getConfigurableFields()
                                          .stream()
                                          .filter(PluginDescriptor.Field::isDynamic)
                                          .map(field -> focusedInfrastructurePlugin.getPluginName() + field.getName());
    }

    private boolean isFocusedPluginItem(PluginDescriptor focusedInfrastructurePlugin,
            PluginDescriptor focusedPolicyPlugin, FormItem formItem) {
        return formItem.getName().startsWith(focusedInfrastructurePlugin.getPluginName()) ||
               formItem.getName().startsWith(focusedPolicyPlugin.getPluginName()) ||
               formItem.getName().equals(INFRASTRUCTURE_FORM_KEY) || formItem.getName().equals(POLICY_FORM_KEY);
    }

    private boolean isNotDynamic(String formItemName, List<String> dynamicParametersName) {
        return dynamicParametersName.stream().noneMatch(dynamicItemName -> areEqual(formItemName, dynamicItemName));
    }

    private boolean areEqual(String formItemName, String otherFormItemName) {
        return formItemName.equals(otherFormItemName) || formItemName.equals(otherFormItemName + EDIT_FORM_ITEM_SUFFIX);
    }

    @Override
    protected void addButtonsToButtonsLayout(HLayout buttonsLayout) {
        buttonsLayout.setMembers(this.applyModificationsButton, this.cancelButton);
    }

    @Override
    protected void beforeSubmit() {
        // All disabled items (that the user cannot modify) need to be
        // re-enabled before being submitted, otherwise they won't be part of
        // the form data
        Arrays.stream(this.nodeSourcePluginsForm.getFields()).forEach(FormItem::enable);
    }

}
