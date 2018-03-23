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
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;

import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.*;


/**
 * NodeSource creation dialog.
 * <p>
 * Dynamically downloads infrastructure and policy info when created.
 */
public class NodeSourceCreationWindow extends NodeSourceWindow {

    public NodeSourceCreationWindow(RMController controller) {
        super(controller, "Add Node Source", "Updating available Infrastructures and Policies");
    }

    @Override
    protected void populateFormValues(Label infraLabel, DynamicForm infraForm, TextItem nameItem,
            CheckboxItem nodesRecoverableItem) {

        nodesRecoverableItem.setValue(true);

        controller.fetchSupportedInfrastructuresAndPolicies(() -> {

            HashMap<String, List<FormItem>> allForms = new HashMap<>();

            infraSelect = new SelectItem("infra", "Infrastructure");
            infraSelect.setRequired(true);
            policySelect = new SelectItem("policy", "Policy");
            policySelect.setRequired(true);

            infraSelect.setWidth(300);
            policySelect.setWidth(300);

            HiddenItem name = new HiddenItem("nsName");
            HiddenItem nodesRecoverable = new HiddenItem("nodesRecoverable");
            HiddenItem deploy = new HiddenItem("deploy");
            HiddenItem callback = new HiddenItem("nsCallback");
            HiddenItem session = new HiddenItem("sessionId");

            ArrayList<FormItem> formParameters = new ArrayList<>();
            formParameters.add(name);
            formParameters.add(nodesRecoverable);
            formParameters.add(deploy);
            formParameters.add(callback);
            formParameters.add(session);
            formParameters.add(infraSelect);

            LinkedHashMap<String, String> values = new LinkedHashMap<>();
            for (PluginDescriptor inf : controller.getModel().getSupportedInfrastructures().values()) {
                String shortName = inf.getPluginName().substring(inf.getPluginName().lastIndexOf('.') + 1);
                values.put(inf.getPluginName(), shortName);

                ArrayList<FormItem> infraFormItems = getPrefilledFormItems(inf);
                formParameters.addAll(infraFormItems);
                allForms.put(inf.getPluginName(), infraFormItems);
            }
            infraSelect.setValueMap(values);

            formParameters.add(new SpacerItem());
            values.clear();
            formParameters.add(policySelect);
            for (PluginDescriptor inf : controller.getModel().getSupportedPolicies().values()) {
                String shortName = inf.getPluginName().substring(inf.getPluginName().lastIndexOf('.') + 1);
                values.put(inf.getPluginName(), shortName);

                ArrayList<FormItem> policyFormItems = getPrefilledFormItems(inf);
                formParameters.addAll(policyFormItems);
                allForms.put(inf.getPluginName(), policyFormItems);
            }
            policySelect.setValueMap(values);

            infraSelect.addChangedHandler(changedEvent -> resetFormForInfrastructureChange(allForms));

            policySelect.addChangedHandler(changedEvent -> resetFormForPolicyChange(allForms));

            infraForm.setFields(formParameters.toArray(new FormItem[formParameters.size()]));
            infraLabel.hide();
            infraForm.show();

            for (List<FormItem> li : allForms.values()) {
                for (FormItem it : li) {
                    it.hide();
                }
            }
        }, () -> window.hide());
    }

}
