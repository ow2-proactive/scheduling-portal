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
import java.util.LinkedList;
import java.util.List;

import org.ow2.proactive_grid_cloud_portal.common.client.CredentialsWindow;
import org.ow2.proactive_grid_cloud_portal.common.client.Images;
import org.ow2.proactive_grid_cloud_portal.common.client.JSUtil;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LogModel;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LoginModel;
import org.ow2.proactive_grid_cloud_portal.rm.client.PluginDescriptor.Field;

import com.google.gwt.core.client.GWT;
import com.google.gwt.json.client.JSONObject;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.Encoding;
import com.smartgwt.client.types.FormMethod;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.HiddenItem;
import com.smartgwt.client.widgets.form.fields.PasswordItem;
import com.smartgwt.client.widgets.form.fields.PickerIcon;
import com.smartgwt.client.widgets.form.fields.PickerIcon.Picker;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SpacerItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.UploadItem;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.layout.VStack;


/**
 * NodeSource creation dialog.
 * <p>
 * Dynamically downloads infrastructure and policy info when created.
 *
 * @author mschnoor
 */
public class NSCreationWindow {

    private RMController controller;

    private Window window;

    private SelectItem infraSelect, policySelect;

    private String oldInfra = null, oldPolicy = null;

    NSCreationWindow(RMController controller) {
        this.controller = controller;
        this.build();
    }

    public void show() {
        this.window.show();
    }

    public void destroy() {
        this.window.destroy();
    }

    private void build() {
        final VLayout layout = new VLayout();
        layout.setMargin(5);

        final VStack infraLayout = new VStack();
        infraLayout.setHeight(26);
        final Label infraLabel = new Label("Updating available Infrastructures and Policies");
        infraLabel.setIcon("loading.gif");
        infraLabel.setHeight(26);
        infraLabel.setAlign(Alignment.CENTER);
        infraLayout.addMember(infraLabel);

        final DynamicForm infraForm = new DynamicForm();
        infraForm.setEncoding(Encoding.MULTIPART);
        infraForm.setMethod(FormMethod.POST);
        infraForm.setAction(GWT.getModuleBaseURL() + "createnodesource");
        infraForm.setTarget("__hiddenFrame");

        infraLayout.addMember(infraForm);

        final Label label = new Label("A Node Source is a combination of an Infrastructure, which defines how resources" +
                                      " will be acquired, and a Policy, that dictates when resources can be acquired.");
        label.setHeight(40);

        final HashMap<String, List<FormItem>> allForms = new HashMap<String, List<FormItem>>();

        controller.fetchSupportedInfrastructuresAndPolicies(new Runnable() {
            public void run() {

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
            }
        }, () -> window.hide());

        final TextItem nameItem = new TextItem("nsName", "Name");
        DynamicForm nameForm = new DynamicForm();

        CheckboxItem nodesRecoverableItem = new CheckboxItem("nodesRecoverable", "Nodes Recoverable");
        nodesRecoverableItem.setValue(true);
        nodesRecoverableItem.setTooltip("Defines whether the nodes of this node source can be recovered after a crash of the Resource Manager");
        nameForm.setFields(nameItem, nodesRecoverableItem);

        HLayout buttons = new HLayout();

        buttons.setWidth100();
        buttons.setHeight(22);
        buttons.setMargin(5);
        buttons.setAlign(Alignment.RIGHT);
        buttons.setMembersMargin(5);

        final IButton createAndDeployNodeSourceButton = new IButton("Deploy Now");
        createAndDeployNodeSourceButton.setIcon(Images.instance.ok_16().getSafeUri().asString());
        createAndDeployNodeSourceButton.setShowDisabledIcon(false);
        final IButton createOnlyNodeSourceButton = new IButton("Save and Keep Undeployed");
        createOnlyNodeSourceButton.setWidth(createAndDeployNodeSourceButton.getWidth() * 2);
        createOnlyNodeSourceButton.setIcon(Images.instance.ok_16().getSafeUri().asString());
        createOnlyNodeSourceButton.setShowDisabledIcon(false);
        final IButton cancelButton = new IButton("Cancel");
        cancelButton.setIcon(Images.instance.cancel_16().getSafeUri().asString());
        cancelButton.setShowDisabledIcon(false);
        List<IButton> buttonsList = new LinkedList<>();
        buttonsList.add(createAndDeployNodeSourceButton);
        buttonsList.add(createOnlyNodeSourceButton);
        buttonsList.add(cancelButton);

        createAndDeployNodeSourceButton.addClickHandler(clickEvent -> prepareCreateAndDeployFormAndSubmit(layout,
                                                                                                          infraLabel,
                                                                                                          infraForm,
                                                                                                          label,
                                                                                                          nameItem,
                                                                                                          nodesRecoverableItem,
                                                                                                          buttonsList));
        createOnlyNodeSourceButton.addClickHandler(clickEvent -> prepareCreateOnlyFormAndSubmit(layout,
                                                                                                infraLabel,
                                                                                                infraForm,
                                                                                                label,
                                                                                                nameItem,
                                                                                                nodesRecoverableItem,
                                                                                                buttonsList));
        cancelButton.addClickHandler(clickEvent -> window.hide());
        buttons.setMembers(createAndDeployNodeSourceButton, createOnlyNodeSourceButton, cancelButton);

        VLayout scroll = new VLayout();
        scroll.setHeight100();
        scroll.setWidth100();
        scroll.setMembers(infraLayout);
        scroll.setOverflow(Overflow.AUTO);
        scroll.setBorder("1px solid #ddd");
        scroll.setBackgroundColor("#fafafa");

        layout.addMember(label);
        layout.addMember(nameForm);
        layout.addMember(scroll);
        layout.addMember(buttons);

        int winWidth = com.google.gwt.user.client.Window.getClientWidth() * 80 / 100;
        int winHeight = com.google.gwt.user.client.Window.getClientHeight() * 80 / 100;
        winWidth = Math.min(1000, winWidth);
        winHeight = Math.min(1000, winHeight);

        this.window = new Window();
        this.window.setTitle("Create Node Source");
        this.window.setShowMinimizeButton(false);
        this.window.setIsModal(true);
        this.window.setShowModalMask(true);
        this.window.addItem(layout);
        this.window.setWidth(winWidth);
        this.window.setHeight(winHeight);
        this.window.setCanDragResize(true);
        this.window.setCanDragReposition(true);
        this.window.centerInPage();
    }

    private ArrayList<FormItem> getPrefilledFormItems(PluginDescriptor inf) {
        List<Field> configurableFields = inf.getConfigurableFields();
        ArrayList<FormItem> forms = new ArrayList<>(configurableFields.size());
        for (Field f : configurableFields) {
            FormItem pol = null;
            if (f.isPassword()) {
                pol = new PasswordItem(inf.getPluginName() + f.getName(), f.getName());
            } else if (f.isFile() || f.isCredential()) {
                pol = new UploadItem(inf.getPluginName() + f.getName(), f.getName());
                if (f.isCredential()) {
                    PickerIcon cred = new PickerIcon(new Picker(Images.instance.key_16().getSafeUri().asString()),
                                                     formItemIconClickEvent -> {
                                                         CredentialsWindow win = new CredentialsWindow();
                                                         win.show();
                                                     });
                    cred.setPrompt("Create a Credential file");
                    cred.setWidth(16);
                    cred.setHeight(16);
                    cred.setAttribute("hspace", 6);
                    pol.setIcons(cred);
                }
            } else {
                pol = new TextItem(inf.getPluginName() + f.getName(), f.getName());
            }
            pol.setValue(f.getValue());
            pol.setWidth(250);
            pol.setHint("<nobr>" + f.getDescription() + "</nobr>");
            forms.add(pol);
        }
        return forms;
    }

    private void resetFormForPolicyChange(HashMap<String, List<FormItem>> allForms) {
        if (infraSelect.getValueAsString() == null) {
            return;
        }

        String policy = policySelect.getValueAsString();
        if (oldPolicy != null) {
            for (FormItem f : allForms.get(oldPolicy)) {
                f.hide();
            }
        }
        for (FormItem f : allForms.get(policy)) {
            f.show();
        }

        if (oldPolicy == null) {
            oldPolicy = policy;
            resetFormForInfrastructureChange(allForms);
        } else {
            oldPolicy = policy;
        }
    }

    private void resetFormForInfrastructureChange(HashMap<String, List<FormItem>> allForms) {
        if (policySelect.getValueAsString() == null) {
            return;
        }

        String nsName = infraSelect.getValueAsString();
        if (oldInfra != null) {
            for (FormItem f : allForms.get(oldInfra)) {
                f.hide();
            }
        }
        for (FormItem f : allForms.get(nsName)) {
            f.show();
        }

        if (oldInfra == null) {
            oldInfra = nsName;
            resetFormForPolicyChange(allForms);
        } else {
            oldInfra = nsName;
        }
    }

    private void prepareCreateAndDeployFormAndSubmit(VLayout layout, Label infraLabel, DynamicForm infraForm,
            Label label, TextItem nameItem, CheckboxItem nodesRecoverableItem, List<IButton> buttonsList) {
        infraForm.setValue("deploy", Boolean.TRUE.toString());
        prepareCreateOnlyFormAndSubmit(layout,
                                       infraLabel,
                                       infraForm,
                                       label,
                                       nameItem,
                                       nodesRecoverableItem,
                                       buttonsList);
    }

    private void prepareCreateOnlyFormAndSubmit(VLayout layout, Label infraLabel, DynamicForm infraForm, Label label,
            TextItem nameItem, CheckboxItem nodesRecoverableItem, List<IButton> buttonsList) {
        infraForm.setValue("infra", infraSelect.getValueAsString());
        infraForm.setValue("nsName", nameItem.getValueAsString());
        infraForm.setValue("nodesRecoverable", nodesRecoverableItem.getValueAsBoolean().toString());
        infraForm.setValue("policy", policySelect.getValueAsString());
        infraForm.setValue("sessionId", LoginModel.getInstance().getSessionId());
        infraForm.setCanSubmit(true);

        /*
         * this smartGWT form looks nice but cannot do callbacks ;
         * we register a native JS function to the document, send it to
         * the servlet so that it writes it back when returning
         * when the browser reads the return value and interprets it as JS,
         * the callback is called
         */
        infraForm.setValue("nsCallback", JSUtil.register(javascriptObject -> {
            JSONObject js = new JSONObject(javascriptObject);
            if (js.containsKey("result") && js.get("result").isBoolean().booleanValue()) {
                window.hide();
                LogModel.getInstance().logMessage("Successfully created nodesource: " + nameItem.getValueAsString());
            } else {
                String msg;
                if (js.get("errorMessage").isString() != null) {
                    msg = js.get("errorMessage").isString().stringValue();
                } else {
                    msg = js.toString();
                }
                label.setContents("<span style='color:red'>Failed to create Node Source :<br>" + msg + "</span>");
                LogModel.getInstance()
                        .logImportantMessage("Failed to create nodesource " + nameItem.getValueAsString() + ": " + msg);
                layout.scrollToTop();
            }
            infraLabel.hide();
            infraForm.show();
            for (IButton button : buttonsList) {
                button.setDisabled(false);
            }
        }));
        infraForm.submitForm();

        for (IButton button : buttonsList) {
            button.setDisabled(true);
        }

        infraLabel.setContents("Node Source creation requested...");
        infraLabel.show();
        infraForm.hide();
    }

}
