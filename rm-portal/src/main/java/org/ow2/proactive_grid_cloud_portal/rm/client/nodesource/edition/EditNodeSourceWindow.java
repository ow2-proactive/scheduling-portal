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
import java.util.List;

import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSourceAction;
import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSourceConfiguration;
import org.ow2.proactive_grid_cloud_portal.rm.client.PluginDescriptor;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMController;
import org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.NodeSourceWindow;

import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.SpacerItem;
import com.smartgwt.client.widgets.layout.HLayout;


/**
 * Dialog window to edit a node source. Downloads dynamically the information 
 * of the current node source, and downloads dynamically all the supported
 * infrastructures and policies when the window is created.
 */
public class EditNodeSourceWindow extends NodeSourceWindow {

    public static final String WINDOW_TITLE = "Edit Node Source";

    private InlineItemModificationCreator inlineItemModificationCreator;

    protected String nodeSourceName;

    public EditNodeSourceWindow(RMController controller, String nodeSourceName) {
        super(controller, WINDOW_TITLE, "Retrieving current node source configuration");
        this.nodeSourceName = nodeSourceName;
        this.inlineItemModificationCreator = new InlineItemModificationCreator(this);
        buildForm();
    }

    protected EditNodeSourceWindow(RMController controller, String nodeSourceName, String windowTitle) {
        super(controller, windowTitle, "Retrieving current node source configuration");
        this.nodeSourceName = nodeSourceName;
        this.inlineItemModificationCreator = new InlineItemModificationCreator(this);
        buildForm();
    }

    @Override
    protected NodeSourceAction getNodeSourceAction() {
        return NodeSourceAction.EDIT;
    }

    @Override
    protected void populateFormValues() {
        this.controller.fetchSupportedInfrastructuresAndPolicies(() -> fetchNodeSourceConfigurationWithCallback(),
                                                                 this.window::hide);
    }

    @Override
    protected List<FormItem> handleNonTextualPluginField(PluginDescriptor plugin, PluginDescriptor.Field pluginField) {
        return inlineItemModificationCreator.getModificationChoiceItemsForNonTextualFields(plugin, pluginField);
    }

    @Override
    protected void addButtonsToButtonsLayout(HLayout buttonsLayout) {
        buttonsLayout.setMembers(this.deployNowButton, this.saveAndKeepUndeployedButton, this.cancelButton);
    }

    @Override
    public void manageNodeSourceWindowItems() {
        // we never allow the node source name to be modified
        this.nodeSourceNameText.disable();
    }

    protected void fetchNodeSourceConfigurationWithCallback() {

        this.controller.fetchNodeSourceConfiguration(this.nodeSourceName, () -> {

            NodeSourceConfiguration nodeSourceConfiguration = this.controller.getModel()
                                                                             .getEditedNodeSourceConfiguration();

            LinkedHashMap<String, String> selectItemValues = new LinkedHashMap<>();
            this.allFormItems = prepareFormItems();
            this.nodeSourceNameText.setDefaultValue(nodeSourceConfiguration.getNodeSourceName());
            this.nodesRecoverableCheckbox.setValue(nodeSourceConfiguration.getNodesRecoverable());
            manageNodeSourceWindowItems();

            PluginDescriptor focusedInfrastructurePlugin = prepareInfrastructureFormItems(nodeSourceConfiguration,
                                                                                          selectItemValues);

            this.allFormItems.add(new SpacerItem());
            selectItemValues.clear();

            PluginDescriptor focusedPolicyPlugin = preparePolicyFormItems(nodeSourceConfiguration, selectItemValues);

            this.infrastructureSelectItem.addChangedHandler(changedEvent -> resetFormForInfrastructureSelectChange());
            this.policySelectItem.addChangedHandler(changedEvent -> resetFormForPolicySelectChange());

            this.allFormItems = modifyFormItemsAfterCreation(focusedInfrastructurePlugin, focusedPolicyPlugin);

            this.nodeSourcePluginsForm.setFields(this.allFormItems.toArray(new FormItem[this.allFormItems.size()]));
            this.nodeSourcePluginsWaitingLabel.hide();
            this.nodeSourcePluginsForm.show();

        }, this.window::hide);
    }

    private PluginDescriptor preparePolicyFormItems(NodeSourceConfiguration nodeSourceConfiguration,
            LinkedHashMap<String, String> selectItemValues) {
        PluginDescriptor focusedPolicyPlugin = nodeSourceConfiguration.getPolicyPluginDescriptor();
        this.focusedPolicyPluginName = focusedPolicyPlugin.getPluginName();
        this.allFormItems.add(this.policySelectItem);
        fillFocusedPluginValues(selectItemValues, focusedPolicyPlugin);
        handleAdditionalPolicyFormItems(selectItemValues, focusedPolicyPlugin);
        this.policySelectItem.setValueMap(selectItemValues);
        this.policySelectItem.setDefaultToFirstOption(true);
        this.previousSelectedPolicy = focusedPolicyPlugin.getPluginName();
        return focusedPolicyPlugin;
    }

    private PluginDescriptor prepareInfrastructureFormItems(NodeSourceConfiguration nodeSourceConfiguration,
            LinkedHashMap<String, String> selectItemValues) {
        PluginDescriptor focusedInfrastructurePlugin = nodeSourceConfiguration.getInfrastructurePluginDescriptor();
        this.focusedInfrastructurePluginName = focusedInfrastructurePlugin.getPluginName();
        this.allFormItems.add(this.infrastructureSelectItem);
        fillFocusedPluginValues(selectItemValues, focusedInfrastructurePlugin);
        handleAdditionalInfrastructureFormItems(selectItemValues, focusedInfrastructurePlugin);
        this.infrastructureSelectItem.setValueMap(selectItemValues);
        this.infrastructureSelectItem.setDefaultToFirstOption(true);
        this.previousSelectedInfrastructure = focusedInfrastructurePlugin.getPluginName();
        return focusedInfrastructurePlugin;
    }

}
