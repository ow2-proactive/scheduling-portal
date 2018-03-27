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


public class NodeSourceEditWindow extends NodeSourceWindow {

    private String nodeSourceName;

    public NodeSourceEditWindow(RMController controller, String nodeSourceName) {
        super(controller, "Edit node source", "Retrieving current node source configuration");
        this.nodeSourceName = nodeSourceName;
        this.buildForm();
    }

    @Override
    protected void populateFormValues(Label windowLabel, DynamicForm windowForm, TextItem nodeSourceNameItem,
            CheckboxItem nodesRecoverableItem) {
        this.controller.fetchSupportedInfrastructuresAndPolicies(() -> fetchNodeSourceConfigurationWithCallback(windowLabel,
                                                                                                                windowForm,
                                                                                                                nodeSourceNameItem,
                                                                                                                nodesRecoverableItem),
                                                                 () -> window.hide());
    }

    private void fetchNodeSourceConfigurationWithCallback(Label windowLabel, DynamicForm windowForm,
            TextItem nodeSourceNameItem, CheckboxItem nodesRecoverableItem) {
        this.controller.fetchNodeSourceConfiguration(this.nodeSourceName, () -> {

            NodeSourceConfiguration nodeSourceConfiguration = controller.getModel().getEditedNodeSourceConfiguration();

            nodeSourceNameItem.setDefaultValue(nodeSourceConfiguration.getNodeSourceName());
            nodeSourceNameItem.setCanEdit(false);

            nodesRecoverableItem.setValue(nodeSourceConfiguration.getNodesRecoverable());

            HashMap<String, List<FormItem>> allForms = new HashMap<>();

            ArrayList<FormItem> formParameters = prepareFormParameters();

            LinkedHashMap<String, String> values = new LinkedHashMap<>();

            PluginDescriptor infrastructurePluginDescriptor = nodeSourceConfiguration.getInfrastructurePluginDescriptor();
            String infrastructureShortName = getPluginShortName(infrastructurePluginDescriptor);
            values.put(infrastructurePluginDescriptor.getPluginName(), infrastructureShortName);

            ArrayList<FormItem> infraFormItems = getPrefilledOnlyTextFormItems(infrastructurePluginDescriptor);
            formParameters.addAll(infraFormItems);
            allForms.put(infrastructurePluginDescriptor.getPluginName(), infraFormItems);
            addPluginDescriptorsToValues(values,
                                         infrastructurePluginDescriptor,
                                         controller.getModel().getSupportedInfrastructures());
            infraSelect.setValueMap(values);
            infraSelect.setDefaultToFirstOption(true);

            formParameters.add(new SpacerItem());

            values.clear();

            formParameters.add(policySelect);

            PluginDescriptor policyPluginDescriptor = nodeSourceConfiguration.getPolicyPluginDescriptor();
            String policyShortName = getPluginShortName(policyPluginDescriptor);
            values.put(policyPluginDescriptor.getPluginName(), policyShortName);

            ArrayList<FormItem> policyFormItems = getPrefilledOnlyTextFormItems(policyPluginDescriptor);
            formParameters.addAll(policyFormItems);
            allForms.put(policyPluginDescriptor.getPluginName(), policyFormItems);
            addPluginDescriptorsToValues(values, policyPluginDescriptor, controller.getModel().getSupportedPolicies());
            policySelect.setValueMap(values);
            policySelect.setDefaultToFirstOption(true);

            infraSelect.addChangedHandler(changedEvent -> resetFormForInfrastructureChange(allForms));

            policySelect.addChangedHandler(changedEvent -> resetFormForPolicyChange(allForms));

            windowForm.setFields(formParameters.toArray(new FormItem[formParameters.size()]));
            windowLabel.hide();
            windowForm.show();

        }, () -> window.hide());
    }

    private void addPluginDescriptorsToValues(LinkedHashMap<String, String> values,
            PluginDescriptor infrastructurePluginDescriptor, Map<String, PluginDescriptor> supportedInfrastructures) {
        for (Map.Entry<String, PluginDescriptor> entry : supportedInfrastructures.entrySet()) {
            PluginDescriptor pluginDescriptor = entry.getValue();
            if (!pluginDescriptor.getPluginName().equals(infrastructurePluginDescriptor.getPluginName())) {
                values.put(pluginDescriptor.getPluginName(), getPluginShortName(pluginDescriptor));
            }
        }
    }

}
