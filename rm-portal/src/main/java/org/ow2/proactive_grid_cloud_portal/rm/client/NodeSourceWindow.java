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
import java.util.LinkedList;
import java.util.List;

import org.ow2.proactive_grid_cloud_portal.common.client.CredentialsWindow;
import org.ow2.proactive_grid_cloud_portal.common.client.Images;
import org.ow2.proactive_grid_cloud_portal.common.client.JSUtil;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LogModel;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LoginModel;

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
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.UploadItem;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.layout.VStack;


public abstract class NodeSourceWindow {

    protected RMController controller;

    protected SelectItem infrastructureSelectItem, policySelectItem;

    protected String previousSelectedInfrastructure = null, previousSelectedPolicy = null;

    protected Window window;

    private String windowTitle;

    private String waitingMessage;

    protected NodeSourceWindow(RMController controller, String windowTitle, String waitingMessage) {
        this.controller = controller;
        this.windowTitle = windowTitle;
        this.waitingMessage = waitingMessage;
    }

    public void show() {
        this.window.show();
    }

    public void destroy() {
        this.window.destroy();
    }

    protected abstract boolean isNodeSourceEdited();

    protected abstract void populateFormValues(Label windowLabel, DynamicForm windowForm, TextItem nodeSourceNameItem,
            CheckboxItem nodesRecoverableItem);

    protected String getPluginShortName(PluginDescriptor plugin) {

        return plugin.getPluginName().substring(plugin.getPluginName().lastIndexOf('.') + 1);
    }

    protected ArrayList<FormItem> prepareFormItems() {

        infrastructureSelectItem = new SelectItem("infra", "Infrastructure");
        infrastructureSelectItem.setRequired(true);
        policySelectItem = new SelectItem("policy", "Policy");
        policySelectItem.setRequired(true);

        infrastructureSelectItem.setWidth(300);
        policySelectItem.setWidth(300);

        HiddenItem name = new HiddenItem("nsName");
        HiddenItem nodesRecoverable = new HiddenItem("nodesRecoverable");
        HiddenItem deploy = new HiddenItem("deploy");
        HiddenItem nodeSourceEdited = new HiddenItem("nodeSourceEdited");
        HiddenItem callback = new HiddenItem("nsCallback");
        HiddenItem session = new HiddenItem("sessionId");

        ArrayList<FormItem> formItems = new ArrayList<>();

        formItems.add(name);
        formItems.add(nodesRecoverable);
        formItems.add(deploy);
        formItems.add(nodeSourceEdited);
        formItems.add(callback);
        formItems.add(session);

        return formItems;
    }

    protected ArrayList<FormItem> getPrefilledFormItems(PluginDescriptor plugin) {

        List<PluginDescriptor.Field> pluginFields = plugin.getConfigurableFields();
        ArrayList<FormItem> formItems = new ArrayList<>(pluginFields.size());

        FormItem formItem;
        for (PluginDescriptor.Field pluginField : pluginFields) {

            if (pluginField.isPassword()) {

                formItem = new PasswordItem(plugin.getPluginName() + pluginField.getName(), pluginField.getName());

            } else if (pluginField.isFile() || pluginField.isCredential()) {

                formItem = new UploadItem(plugin.getPluginName() + pluginField.getName(), pluginField.getName());

                if (pluginField.isCredential()) {

                    PickerIcon createCredentialsPicker = new PickerIcon(new PickerIcon.Picker(Images.instance.key_16()
                                                                                                             .getSafeUri()
                                                                                                             .asString()),
                                                                        formItemIconClickEvent -> {
                                                                            CredentialsWindow win = new CredentialsWindow();
                                                                            win.show();
                                                                        });
                    createCredentialsPicker.setPrompt("Create a Credential file");
                    createCredentialsPicker.setWidth(16);
                    createCredentialsPicker.setHeight(16);
                    createCredentialsPicker.setAttribute("hspace", 6);

                    formItem.setIcons(createCredentialsPicker);
                }
            } else {

                formItem = new TextItem(plugin.getPluginName() + pluginField.getName(), pluginField.getName());
            }

            formItem.setValue(pluginField.getValue());
            formItem.setWidth(250);
            formItem.setHint("<nobr>" + pluginField.getDescription() + "</nobr>");

            formItems.add(formItem);
        }
        return formItems;
    }

    // TODO Method to use when the server is ready to deal with
    // password/credential/file fields that are not treated like file
    // parameters
    protected ArrayList<FormItem> getPrefilledOnlyTextFormItems(PluginDescriptor plugin) {

        List<PluginDescriptor.Field> pluginFields = plugin.getConfigurableFields();
        ArrayList<FormItem> formItems = new ArrayList<>(pluginFields.size());

        for (PluginDescriptor.Field f : pluginFields) {

            FormItem formItem = new TextItem(plugin.getPluginName() + f.getName(), f.getName());

            formItem.setValue(f.getValue());
            formItem.setWidth(250);
            formItem.setHint("<nobr>" + f.getDescription() + "</nobr>");

            formItems.add(formItem);
        }

        return formItems;
    }

    protected void resetFormForPolicySelectChange(HashMap<String, List<FormItem>> allFormItemsPerPlugin) {

        if (infrastructureSelectItem.getValueAsString() == null) {
            return;
        }

        String policyPluginName = policySelectItem.getValueAsString();
        if (previousSelectedPolicy != null) {
            for (FormItem formItem : allFormItemsPerPlugin.get(previousSelectedPolicy)) {
                formItem.hide();
            }
        }

        for (FormItem formItem : allFormItemsPerPlugin.get(policyPluginName)) {
            formItem.show();
        }

        if (previousSelectedPolicy == null) {
            previousSelectedPolicy = policyPluginName;
            resetFormForInfrastructureSelectChange(allFormItemsPerPlugin);
        } else {
            previousSelectedPolicy = policyPluginName;
        }
    }

    protected void resetFormForInfrastructureSelectChange(HashMap<String, List<FormItem>> allFormItemsPerPlugin) {

        if (policySelectItem.getValueAsString() == null) {
            return;
        }

        String infrastructurePluginName = infrastructureSelectItem.getValueAsString();
        if (previousSelectedInfrastructure != null) {
            for (FormItem formItem : allFormItemsPerPlugin.get(previousSelectedInfrastructure)) {
                formItem.hide();
            }
        }

        for (FormItem formItem : allFormItemsPerPlugin.get(infrastructurePluginName)) {
            formItem.show();
        }

        if (previousSelectedInfrastructure == null) {
            previousSelectedInfrastructure = infrastructurePluginName;
            resetFormForPolicySelectChange(allFormItemsPerPlugin);
        } else {
            previousSelectedInfrastructure = infrastructurePluginName;
        }
    }

    protected void buildForm() {

        final VLayout nodeSourceWindowLayout = new VLayout();
        nodeSourceWindowLayout.setMargin(5);

        final VStack nodeSourcePluginsLayout = new VStack();
        nodeSourcePluginsLayout.setHeight(26);

        final Label nodeSourcePluginsWaitingLabel = new Label(this.waitingMessage);
        nodeSourcePluginsWaitingLabel.setIcon("loading.gif");
        nodeSourcePluginsWaitingLabel.setHeight(26);
        nodeSourcePluginsWaitingLabel.setAlign(Alignment.CENTER);
        nodeSourcePluginsLayout.addMember(nodeSourcePluginsWaitingLabel);

        final DynamicForm nodeSourcePluginsForm = new DynamicForm();
        nodeSourcePluginsForm.setEncoding(Encoding.MULTIPART);
        nodeSourcePluginsForm.setMethod(FormMethod.POST);
        nodeSourcePluginsForm.setAction(GWT.getModuleBaseURL() + "createnodesource");
        nodeSourcePluginsForm.setTarget("__hiddenFrame");

        nodeSourcePluginsLayout.addMember(nodeSourcePluginsForm);

        final Label nodeSourceWindowLabel = new Label("A Node Source is a combination of an Infrastructure, which defines how resources" +
                                                      " will be acquired, and a Policy, that dictates when resources can be acquired.");
        nodeSourceWindowLabel.setHeight(40);

        final TextItem nodeSourceNameItem = new TextItem("nsName", "Name");
        DynamicForm nodeSourceWindowForm = new DynamicForm();

        CheckboxItem nodesRecoverableItem = new CheckboxItem("nodesRecoverable", "Nodes Recoverable");
        nodesRecoverableItem.setTooltip("Defines whether the nodes of this node source can be recovered after a crash of the Resource Manager");
        nodeSourceWindowForm.setFields(nodeSourceNameItem, nodesRecoverableItem);

        this.populateFormValues(nodeSourcePluginsWaitingLabel,
                                nodeSourcePluginsForm,
                                nodeSourceNameItem,
                                nodesRecoverableItem);

        HLayout buttonsLayout = new HLayout();

        buttonsLayout.setWidth100();
        buttonsLayout.setHeight(22);
        buttonsLayout.setMargin(5);
        buttonsLayout.setAlign(Alignment.RIGHT);
        buttonsLayout.setMembersMargin(5);

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

        List<IButton> buttonList = new LinkedList<>();
        buttonList.add(createAndDeployNodeSourceButton);
        buttonList.add(createOnlyNodeSourceButton);
        buttonList.add(cancelButton);

        createAndDeployNodeSourceButton.addClickHandler(clickEvent -> prepareCreateAndDeployFormAndSubmit(nodeSourceWindowLayout,
                                                                                                          nodeSourcePluginsWaitingLabel,
                                                                                                          nodeSourcePluginsForm,
                                                                                                          nodeSourceWindowLabel,
                                                                                                          nodeSourceNameItem,
                                                                                                          nodesRecoverableItem,
                                                                                                          buttonList,
                                                                                                          isNodeSourceEdited()));

        createOnlyNodeSourceButton.addClickHandler(clickEvent -> prepareCreateOnlyFormAndSubmit(nodeSourceWindowLayout,
                                                                                                nodeSourcePluginsWaitingLabel,
                                                                                                nodeSourcePluginsForm,
                                                                                                nodeSourceWindowLabel,
                                                                                                nodeSourceNameItem,
                                                                                                nodesRecoverableItem,
                                                                                                buttonList,
                                                                                                isNodeSourceEdited()));
        cancelButton.addClickHandler(clickEvent -> window.hide());
        buttonsLayout.setMembers(createAndDeployNodeSourceButton, createOnlyNodeSourceButton, cancelButton);

        VLayout scrollLayout = new VLayout();
        scrollLayout.setHeight100();
        scrollLayout.setWidth100();
        scrollLayout.setMembers(nodeSourcePluginsLayout);
        scrollLayout.setOverflow(Overflow.AUTO);
        scrollLayout.setBorder("1px solid #ddd");
        scrollLayout.setBackgroundColor("#fafafa");

        nodeSourceWindowLayout.addMember(nodeSourceWindowLabel);
        nodeSourceWindowLayout.addMember(nodeSourceWindowForm);
        nodeSourceWindowLayout.addMember(scrollLayout);
        nodeSourceWindowLayout.addMember(buttonsLayout);

        int winWidth = com.google.gwt.user.client.Window.getClientWidth() * 80 / 100;
        int winHeight = com.google.gwt.user.client.Window.getClientHeight() * 80 / 100;
        winWidth = Math.min(1000, winWidth);
        winHeight = Math.min(1000, winHeight);

        this.window = new Window();
        this.window.setTitle(this.windowTitle);
        this.window.setShowMinimizeButton(false);
        this.window.setIsModal(true);
        this.window.setShowModalMask(true);
        this.window.addItem(nodeSourceWindowLayout);
        this.window.setWidth(winWidth);
        this.window.setHeight(winHeight);
        this.window.setCanDragResize(true);
        this.window.setCanDragReposition(true);
        this.window.centerInPage();
    }

    private void prepareCreateAndDeployFormAndSubmit(VLayout nodeSourceWindowLayout,
            Label nodeSourcePluginsWaitingLabel, DynamicForm nodeSourcePluginsForm, Label nodeSourceWindowLabel,
            TextItem nodeSourceNameItem, CheckboxItem nodesRecoverableItem, List<IButton> buttonList,
            boolean nodeSourceEdited) {

        nodeSourcePluginsForm.setValue("deploy", Boolean.TRUE.toString());

        prepareCreateOnlyFormAndSubmit(nodeSourceWindowLayout,
                                       nodeSourcePluginsWaitingLabel,
                                       nodeSourcePluginsForm,
                                       nodeSourceWindowLabel,
                                       nodeSourceNameItem,
                                       nodesRecoverableItem,
                                       buttonList,
                                       nodeSourceEdited);
    }

    private void prepareCreateOnlyFormAndSubmit(VLayout nodeSourceWindowLayout, Label nodeSourcePluginsWaitingLabel,
            DynamicForm nodeSourcePluginsForm, Label nodeSourceWindowLabel, TextItem nodeSourceNameItem,
            CheckboxItem nodesRecoverableItem, List<IButton> buttonList, boolean nodeSourceEdited) {

        nodeSourcePluginsForm.setValue("infra", infrastructureSelectItem.getValueAsString());
        nodeSourcePluginsForm.setValue("nsName", nodeSourceNameItem.getValueAsString());
        nodeSourcePluginsForm.setValue("nodesRecoverable", nodesRecoverableItem.getValueAsBoolean().toString());
        nodeSourcePluginsForm.setValue("policy", policySelectItem.getValueAsString());
        nodeSourcePluginsForm.setValue("sessionId", LoginModel.getInstance().getSessionId());
        nodeSourcePluginsForm.setValue("nodeSourceEdited", Boolean.toString(nodeSourceEdited));
        nodeSourcePluginsForm.setCanSubmit(true);

        /*
         * this smartGWT form looks nice but cannot do callbacks ;
         * we register a native JS function to the document, send it to
         * the servlet so that it writes it back when returning
         * when the browser reads the return value and interprets it as JS,
         * the callback is called
         */
        nodeSourcePluginsForm.setValue("nsCallback", JSUtil.register(javascriptObject -> {

            JSONObject jsonCallback = new JSONObject(javascriptObject);

            if (jsonCallback.containsKey("result") && jsonCallback.get("result").isBoolean().booleanValue()) {

                window.hide();
                LogModel.getInstance()
                        .logMessage("Successfully created nodesource: " + nodeSourceNameItem.getValueAsString());

            } else {

                handleNodeSourceCreationError(nodeSourceWindowLayout,
                                              nodeSourceWindowLabel,
                                              nodeSourceNameItem,
                                              jsonCallback);
            }

            nodeSourcePluginsWaitingLabel.hide();
            nodeSourcePluginsForm.show();

            for (IButton button : buttonList) {
                button.setDisabled(false);
            }
        }));

        nodeSourcePluginsForm.submitForm();

        for (IButton button : buttonList) {
            button.setDisabled(true);
        }

        nodeSourcePluginsWaitingLabel.setContents("Node Source creation requested...");
        nodeSourcePluginsWaitingLabel.show();
        nodeSourcePluginsForm.hide();
    }

    private void handleNodeSourceCreationError(VLayout nodeSourceWindowLayout, Label nodeSourceWindowLabel,
            TextItem nodeSourceNameItem, JSONObject jsonCallback) {

        String msg;
        if (jsonCallback.get("errorMessage").isString() != null) {
            msg = jsonCallback.get("errorMessage").isString().stringValue();
        } else {
            msg = jsonCallback.toString();
        }

        nodeSourceWindowLabel.setContents("<span style='color:red'>Failed to create Node Source :<br>" + msg +
                                          "</span>");
        LogModel.getInstance().logImportantMessage("Failed to create nodesource " +
                                                   nodeSourceNameItem.getValueAsString() + ": " + msg);
        nodeSourceWindowLayout.scrollToTop();
    }

}
