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
package org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.creation;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;

import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSourceAction;
import org.ow2.proactive_grid_cloud_portal.rm.client.PluginDescriptor;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMController;
import org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.NodeSourceWindow;
import org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.edition.InlineItemModificationCreator;

import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.SpacerItem;
import com.smartgwt.client.widgets.form.fields.UploadItem;
import com.smartgwt.client.widgets.layout.HLayout;


/**
 * Dialog window to create a node source. Downloads dynamically supported
 * infrastructures and policies when the window is created.
 */
public class CreateNodeSourceWindow extends NodeSourceWindow {

    private final InlineItemModificationCreator inlineItemModificationCreator;

    public CreateNodeSourceWindow(RMController controller) {
        super(controller, "Add Node Source", "Updating Available Infrastructures and Policies");
        this.inlineItemModificationCreator = new InlineItemModificationCreator(this);
        buildForm();
    }

    @Override
    protected NodeSourceAction getNodeSourceAction() {
        return NodeSourceAction.CREATE;
    }

    @Override
    protected void populateFormValues() {
        this.controller.fetchSupportedInfrastructuresAndPolicies(() -> {
            prepareFormItems();
            this.nodesRecoverableCheckbox.setValue(true);
            this.formItemsByName.put(INFRASTRUCTURE_FORM_KEY, Collections.singletonList(this.infrastructureSelectItem));
            addInfrastructurePluginValuesToAllFormItems(this.controller.getModel()
                                                                       .getSupportedInfrastructures()
                                                                       .values());
            this.formItemsByName.put("spacer3", Collections.singletonList(new SpacerItem()));
            this.formItemsByName.put(POLICY_FORM_KEY, Collections.singletonList(this.policySelectItem));
            addPolicyPluginValuesToAllFormItems(this.controller.getModel().getSupportedPolicies().values());
            this.infrastructureSelectItem.addChangedHandler(changedEvent -> resetFormForInfrastructureSelectChange());
            this.policySelectItem.addChangedHandler(changedEvent -> resetFormForPolicySelectChange());

            this.nodeSourcePluginsForm.setFields(this.formItemsByName.values()
                                                                     .stream()
                                                                     .flatMap(Collection::stream)
                                                                     .toArray(FormItem[]::new));
            this.nodeSourcePluginsWaitingLabel.hide();
            this.nodeSourcePluginsForm.show();
            hideAllPluginFormItems();
        }, this.window::hide);
    }

    @Override
    protected List<FormItem> handleNonTextualPluginField(PluginDescriptor plugin, PluginDescriptor.Field pluginField) {
        if (this.createdFromImport) {
            return this.inlineItemModificationCreator.getModificationChoiceItemsForNonTextualFields(plugin,
                                                                                                    pluginField);
        } else {
            List<FormItem> formItems = new LinkedList<>();
            FormItem chooseCredentialsFormItem = new UploadItem(plugin.getPluginName() + pluginField.getName(),
                                                                pluginField.getName());
            addCredentialsPickerIcon(pluginField, formItems, chooseCredentialsFormItem);

            return formItems;
        }
    }

    @Override
    protected void addButtonsToButtonsLayout(HLayout buttonsLayout) {
        buttonsLayout.setMembers(this.deployNowButton, this.saveAndKeepUndeployedButton, this.cancelButton);
    }

    private void addInfrastructurePluginValuesToAllFormItems(Collection<PluginDescriptor> allPluginDescriptors) {
        LinkedHashMap<String, String> selectItemValues = new LinkedHashMap<>();
        addAllPluginValuesToAllFormItems(allPluginDescriptors, selectItemValues);
        this.infrastructureSelectItem.setValueMap(selectItemValues);
    }

    private void addPolicyPluginValuesToAllFormItems(Collection<PluginDescriptor> allPluginDescriptors) {
        LinkedHashMap<String, String> selectItemValues = new LinkedHashMap<>();
        addAllPluginValuesToAllFormItems(allPluginDescriptors, selectItemValues);
        this.policySelectItem.setValueMap(selectItemValues);
    }

    private void addAllPluginValuesToAllFormItems(Collection<PluginDescriptor> allPluginDescriptors,
            LinkedHashMap<String, String> selectItemValues) {
        for (PluginDescriptor pluginDescriptor : allPluginDescriptors) {
            String shortName = getPluginShortName(pluginDescriptor);
            selectItemValues.put(pluginDescriptor.getPluginName(), shortName);
            ArrayList<FormItem> currentPluginFormItems = getPrefilledFormItems(pluginDescriptor);
            this.formItemsByName.put(pluginDescriptor.getPluginName(), currentPluginFormItems);
        }
    }

}
