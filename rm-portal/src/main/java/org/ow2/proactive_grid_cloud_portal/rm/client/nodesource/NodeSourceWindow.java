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

import static org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.edition.InlineItemModificationCreator.EDIT_FORM_ITEM_SUFFIX;
import static org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.edition.InlineItemModificationCreator.EDIT_OR_UPLOAD_FORM_ITEM_SUFFIX;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.ow2.proactive_grid_cloud_portal.common.client.CredentialsWindow;
import org.ow2.proactive_grid_cloud_portal.common.client.Images;
import org.ow2.proactive_grid_cloud_portal.common.client.JSUtil;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LogModel;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LoginModel;
import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSourceAction;
import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSourceConfiguration;
import org.ow2.proactive_grid_cloud_portal.rm.client.PluginDescriptor;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMController;
import org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.serialization.ImportException;
import org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.serialization.ImportNodeSourceLayout;
import org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.serialization.NodeSourceConfigurationParser;

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
import com.smartgwt.client.widgets.form.fields.SpacerItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.layout.VStack;


public abstract class NodeSourceWindow {

    protected static final String INFRASTRUCTURE_FORM_KEY = "infra";

    protected static final String POLICY_FORM_KEY = "policy";

    private static final String NS_NAME_FORM_KEY = "nsName";

    private static final String NODES_RECOVERABLE_FORM_KEY = "nodesRecoverable";

    private static final String DEPLOY_FORM_KEY = "deploy";

    private static final String NODE_SOURCE_ACTION_FORM_KEY = "nodeSourceAction";

    private static final String SESSION_ID_FORM_KEY = "sessionId";

    private static final String NS_CALLBACK_FORM_KEY = "nsCallback";

    protected RMController controller;

    protected Label nodeSourceWindowLabel;

    protected DynamicForm nodeSourcePluginsForm;

    protected Label nodeSourcePluginsWaitingLabel;

    protected CheckboxItem nodesRecoverableCheckbox;

    protected TextItem nodeSourceNameText;

    protected SelectItem infrastructureSelectItem;

    protected SelectItem policySelectItem;

    protected String previousSelectedInfrastructure;

    protected String previousSelectedPolicy;

    protected List<FormItem> allFormItems;

    protected Map<String, List<FormItem>> allFormItemsPerPlugin;

    protected String focusedInfrastructurePluginName;

    protected String focusedPolicyPluginName;

    protected boolean createdFromImport;

    protected IButton deployNowButton;

    protected IButton saveAndKeepUndeployedButton;

    protected IButton cancelButton;

    protected IButton applyModificationsButton;

    protected Window window;

    private String windowTitle;

    private String waitingMessage;

    protected NodeSourceWindow(RMController controller, String windowTitle, String waitingMessage) {
        this.controller = controller;
        this.windowTitle = windowTitle;
        this.createdFromImport = false;
        this.waitingMessage = waitingMessage;
        this.allFormItemsPerPlugin = new HashMap<>();
    }

    public void show() {
        this.window.show();
    }

    public void destroy() {
        this.window.destroy();
    }

    public List<FormItem> getFormItemsOfPlugin(String pluginName) {
        return this.allFormItemsPerPlugin.get(pluginName);
    }

    protected abstract NodeSourceAction getNodeSourceAction();

    protected abstract void populateFormValues();

    protected abstract List<FormItem> handleNonTextualPluginField(PluginDescriptor plugin,
            PluginDescriptor.Field pluginField);

    protected abstract void addButtonsToButtonsLayout(HLayout buttonsLayout);

    protected String getPluginShortName(PluginDescriptor plugin) {

        return plugin.getPluginName().substring(plugin.getPluginName().lastIndexOf('.') + 1);
    }

    protected List<FormItem> prepareFormItems() {

        this.infrastructureSelectItem = new SelectItem(INFRASTRUCTURE_FORM_KEY, "Infrastructure");
        this.infrastructureSelectItem.setRequired(true);
        this.policySelectItem = new SelectItem(POLICY_FORM_KEY, "Policy");
        this.policySelectItem.setRequired(true);

        this.infrastructureSelectItem.setWidth(300);
        this.policySelectItem.setWidth(300);

        HiddenItem name = new HiddenItem(NS_NAME_FORM_KEY);
        HiddenItem nodesRecoverable = new HiddenItem(NODES_RECOVERABLE_FORM_KEY);
        HiddenItem deploy = new HiddenItem(DEPLOY_FORM_KEY);
        HiddenItem nodeSourceAction = new HiddenItem(NODE_SOURCE_ACTION_FORM_KEY);
        HiddenItem callback = new HiddenItem(NS_CALLBACK_FORM_KEY);
        HiddenItem session = new HiddenItem(SESSION_ID_FORM_KEY);

        ArrayList<FormItem> formItems = new ArrayList<>();

        formItems.add(name);
        formItems.add(nodesRecoverable);
        formItems.add(deploy);
        formItems.add(nodeSourceAction);
        formItems.add(callback);
        formItems.add(session);

        return formItems;
    }

    protected ArrayList<FormItem> getPrefilledFormItems(PluginDescriptor plugin) {

        List<PluginDescriptor.Field> pluginFields = plugin.getConfigurableFields();
        ArrayList<FormItem> formItems = new ArrayList<>(pluginFields.size());

        List<FormItem> formItemsForField = new LinkedList<>();
        for (PluginDescriptor.Field pluginField : pluginFields) {

            if (pluginField.isPassword()) {

                formItemsForField.add(new PasswordItem(plugin.getPluginName() + pluginField.getName(),
                                                       pluginField.getName()));

            } else if (pluginField.isFile() || pluginField.isCredential()) {

                formItemsForField.addAll(handleNonTextualPluginField(plugin, pluginField));

            } else {

                formItemsForField.add(new TextItem(plugin.getPluginName() + pluginField.getName(),
                                                   pluginField.getName()));
            }

            formItemsForField.forEach(formItem -> {
                formItem.setValue(pluginField.getValue());
                formItem.setWidth(250);
                if (!formItem.getName().endsWith(EDIT_OR_UPLOAD_FORM_ITEM_SUFFIX) &&
                    !formItem.getName().endsWith(EDIT_FORM_ITEM_SUFFIX)) {
                    formItem.setHint("<nobr>" + pluginField.getDescription() + "</nobr>");
                }
            });

            formItems.addAll(formItemsForField);
            formItemsForField.clear();
        }

        return formItems;
    }

    protected void resetFormForPolicySelectChange() {

        if (this.infrastructureSelectItem.getValueAsString() == null) {
            return;
        }

        String policyPluginName = this.policySelectItem.getValueAsString();
        if (this.previousSelectedPolicy != null) {
            for (FormItem formItem : this.allFormItemsPerPlugin.get(this.previousSelectedPolicy)) {
                formItem.hide();
            }
        }

        for (FormItem formItem : this.allFormItemsPerPlugin.get(policyPluginName)) {
            formItem.show();
        }

        if (this.previousSelectedPolicy == null) {
            this.previousSelectedPolicy = policyPluginName;
            resetFormForInfrastructureSelectChange();
        } else {
            this.previousSelectedPolicy = policyPluginName;
        }
    }

    protected void resetFormForInfrastructureSelectChange() {

        if (this.policySelectItem.getValueAsString() == null) {
            return;
        }

        String infrastructurePluginName = this.infrastructureSelectItem.getValueAsString();
        if (this.previousSelectedInfrastructure != null) {
            for (FormItem formItem : this.allFormItemsPerPlugin.get(this.previousSelectedInfrastructure)) {
                formItem.hide();
            }
        }

        for (FormItem formItem : this.allFormItemsPerPlugin.get(infrastructurePluginName)) {
            formItem.show();
        }

        if (this.previousSelectedInfrastructure == null) {
            this.previousSelectedInfrastructure = infrastructurePluginName;
            resetFormForPolicySelectChange();
        } else {
            this.previousSelectedInfrastructure = infrastructurePluginName;
        }
    }

    protected void buildForm() {

        VLayout nodeSourceWindowLayout = new VLayout();
        nodeSourceWindowLayout.setMargin(5);
        nodeSourceWindowLayout.setMembersMargin(5);

        HLayout nodeSourceWindowSubLayoutTop = new HLayout();
        HLayout nodeSourceWindowSubLayoutBottom = new HLayout();

        VStack nodeSourcePluginsLayout = new VStack();
        nodeSourcePluginsLayout.setHeight(26);

        this.nodeSourcePluginsWaitingLabel = new Label(this.waitingMessage);
        this.nodeSourcePluginsWaitingLabel.setIcon("loading.gif");
        this.nodeSourcePluginsWaitingLabel.setHeight(26);
        this.nodeSourcePluginsWaitingLabel.setAlign(Alignment.CENTER);
        nodeSourcePluginsLayout.addMember(this.nodeSourcePluginsWaitingLabel);

        this.nodeSourcePluginsForm = new DynamicForm();
        this.nodeSourcePluginsForm.setEncoding(Encoding.MULTIPART);
        this.nodeSourcePluginsForm.setMethod(FormMethod.POST);
        this.nodeSourcePluginsForm.setAction(GWT.getModuleBaseURL() + "createnodesource");
        this.nodeSourcePluginsForm.setTarget("__hiddenFrame");
        this.nodeSourcePluginsForm.setTitleSuffix("");

        nodeSourcePluginsLayout.addMember(this.nodeSourcePluginsForm);

        this.nodeSourceWindowLabel = new Label("A Node Source is a combination of an Infrastructure, which defines how resources" +
                                               " will be acquired, and a Policy, that dictates when resources can be acquired.");
        this.nodeSourceWindowLabel.setHeight(40);

        this.nodeSourceNameText = new TextItem(NS_NAME_FORM_KEY, "Name");
        this.nodesRecoverableCheckbox = new CheckboxItem(NODES_RECOVERABLE_FORM_KEY, "Nodes Recoverable");
        this.nodesRecoverableCheckbox.setTooltip("Defines whether the nodes of this node source can be recovered after a crash of the Resource Manager");
        Layout importNodeSourceLayout = new ImportNodeSourceLayout(this);
        DynamicForm nodeSourceWindowForm = new DynamicForm();
        nodeSourceWindowForm.setWidth100();
        nodeSourceWindowForm.setFields(this.nodeSourceNameText, this.nodesRecoverableCheckbox);
        nodeSourceWindowForm.setTitleSuffix("");

        this.populateFormValues();

        HLayout buttonsLayout = new HLayout();

        buttonsLayout.setWidth100();
        buttonsLayout.setHeight(22);
        buttonsLayout.setMargin(5);
        buttonsLayout.setAlign(Alignment.RIGHT);
        buttonsLayout.setMembersMargin(5);

        this.applyModificationsButton = new IButton("Apply Modifications");
        this.applyModificationsButton.setWidth(this.applyModificationsButton.getWidth() * 2);
        this.applyModificationsButton.setIcon(Images.instance.ok_16().getSafeUri().asString());
        this.applyModificationsButton.setShowDisabledIcon(false);

        this.deployNowButton = new IButton("Deploy Now");
        this.deployNowButton.setIcon(Images.instance.ok_16().getSafeUri().asString());
        this.deployNowButton.setShowDisabledIcon(false);

        this.saveAndKeepUndeployedButton = new IButton("Save and Keep Undeployed");
        this.saveAndKeepUndeployedButton.setWidth(this.deployNowButton.getWidth() * 2);
        this.saveAndKeepUndeployedButton.setIcon(Images.instance.ok_16().getSafeUri().asString());
        this.saveAndKeepUndeployedButton.setShowDisabledIcon(false);

        this.cancelButton = new IButton("Cancel");
        this.cancelButton.setIcon(Images.instance.cancel_16().getSafeUri().asString());
        this.cancelButton.setShowDisabledIcon(false);

        List<IButton> buttonList = new LinkedList<>();
        buttonList.add(this.applyModificationsButton);
        buttonList.add(this.deployNowButton);
        buttonList.add(this.saveAndKeepUndeployedButton);
        buttonList.add(this.cancelButton);

        this.applyModificationsButton.addClickHandler(clickEvent -> applyModificationsToNodeSource(nodeSourceWindowLayout,
                                                                                                   buttonList,
                                                                                                   getNodeSourceAction()));

        this.deployNowButton.addClickHandler(clickEvent -> saveAndDeployNodeSource(nodeSourceWindowLayout,
                                                                                   buttonList,
                                                                                   getNodeSourceAction()));

        this.saveAndKeepUndeployedButton.addClickHandler(clickEvent -> saveNodeSource(nodeSourceWindowLayout,
                                                                                      buttonList,
                                                                                      getNodeSourceAction()));
        this.cancelButton.addClickHandler(clickEvent -> window.hide());

        addButtonsToButtonsLayout(buttonsLayout);

        VLayout scrollLayout = new VLayout();
        scrollLayout.setHeight100();
        scrollLayout.setWidth100();
        scrollLayout.setMembers(nodeSourcePluginsLayout);
        scrollLayout.setOverflow(Overflow.AUTO);
        scrollLayout.setBorder("1px solid #ddd");
        scrollLayout.setBackgroundColor("#fafafa");

        nodeSourceWindowLayout.addMember(this.nodeSourceWindowLabel);
        nodeSourceWindowSubLayoutTop.addMember(nodeSourceWindowForm);
        nodeSourceWindowSubLayoutTop.addMember(importNodeSourceLayout);
        nodeSourceWindowLayout.addMember(nodeSourceWindowSubLayoutTop);
        nodeSourceWindowSubLayoutBottom.setHeight100();
        nodeSourceWindowSubLayoutBottom.setWidth100();
        nodeSourceWindowSubLayoutBottom.addMember(scrollLayout);
        nodeSourceWindowLayout.addMember(nodeSourceWindowSubLayoutBottom);
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

    protected DynamicForm getNodeSourcePluginsForm() {
        return this.nodeSourcePluginsForm;
    }

    protected CheckboxItem getNodesRecoverableCheckbox() {
        return this.nodesRecoverableCheckbox;
    }

    protected Label getNodeSourceWindowLabel() {
        return this.nodeSourceWindowLabel;
    }

    /**
     * Allow sub classes to rework the form items that have been added to the
     * form.
     */
    protected List<FormItem> modifyFormItemsAfterCreation(PluginDescriptor focusedInfrastructurePlugin,
            PluginDescriptor focusedPolicyPlugin) {

        for (Map.Entry<String, List<FormItem>> entry : this.allFormItemsPerPlugin.entrySet()) {

            hideNotFocusedFormItem(focusedInfrastructurePlugin, focusedPolicyPlugin, entry.getKey(), entry.getValue());
        }

        return this.allFormItems;
    }

    private void hideNotFocusedFormItem(PluginDescriptor focusedInfrastructurePlugin,
            PluginDescriptor focusedPolicyPlugin, String pluginName, List<FormItem> formItemsForPlugin) {

        if (!pluginName.equals(focusedInfrastructurePlugin.getPluginName()) &&
            !pluginName.equals(focusedPolicyPlugin.getPluginName())) {

            for (FormItem formItem : formItemsForPlugin) {
                formItem.hide();
            }
        }
    }

    /**
     * Allow sub classes to alter the node source name and recoverable items.
     */
    public void manageNodeSourceWindowItems() {

    }

    /**
     * Allow sub classes to redefine a custom behavior regarding what to do,
     * regarding policies, in addition to the focused policy plugin.
     */
    protected void handleAdditionalPolicyFormItems(Map<String, String> selectItemValues,
            PluginDescriptor focusedPolicyPlugin) {
        Map<String, PluginDescriptor> allSupportedPolicies = this.controller.getModel().getSupportedPolicies();
        addPluginValuesToAllFormItemOtherThanFocused(selectItemValues, focusedPolicyPlugin, allSupportedPolicies);
    }

    /**
     * Allow sub classes to redefine a custom behavior regarding what to do,
     * regarding infrastructures, in addition to the focused infrastructure
     * plugin.
     */
    protected void handleAdditionalInfrastructureFormItems(Map<String, String> selectItemValues,
            PluginDescriptor focusedInfrastructurePlugin) {
        Map<String, PluginDescriptor> allSupportedInfrastructures = this.controller.getModel()
                                                                                   .getSupportedInfrastructures();
        addPluginValuesToAllFormItemOtherThanFocused(selectItemValues,
                                                     focusedInfrastructurePlugin,
                                                     allSupportedInfrastructures);
    }

    protected void fillFocusedPluginValues(LinkedHashMap<String, String> selectItemValues,
            PluginDescriptor focusedPlugin) {
        String pluginShortName = getPluginShortName(focusedPlugin);
        selectItemValues.put(focusedPlugin.getPluginName(), pluginShortName);

        List<FormItem> pluginFormItems = getPrefilledFormItems(focusedPlugin);
        this.allFormItems.addAll(pluginFormItems);
        this.allFormItemsPerPlugin.put(focusedPlugin.getPluginName(), pluginFormItems);
    }

    protected void addPluginValuesToAllFormItemOtherThanFocused(Map<String, String> selectItemValues,
            PluginDescriptor focusedPlugin, Map<String, PluginDescriptor> allPluginDescriptors) {
        for (Map.Entry<String, PluginDescriptor> entry : allPluginDescriptors.entrySet()) {
            PluginDescriptor plugin = entry.getValue();
            if (!plugin.getPluginName().equals(focusedPlugin.getPluginName())) {

                selectItemValues.put(plugin.getPluginName(), getPluginShortName(plugin));
                List<FormItem> currentPluginFormItems = getPrefilledFormItems(plugin);
                this.allFormItems.addAll(currentPluginFormItems);
                this.allFormItemsPerPlugin.put(plugin.getPluginName(), currentPluginFormItems);
            }
        }
    }

    private void applyModificationsToNodeSource(VLayout nodeSourceWindowLayout, List<IButton> buttonList,
            NodeSourceAction nodeSourceAction) {

        this.nodeSourcePluginsForm.setValue(DEPLOY_FORM_KEY, Boolean.FALSE.toString());

        saveNodeSource(nodeSourceWindowLayout, buttonList, nodeSourceAction);
    }

    private void saveAndDeployNodeSource(VLayout nodeSourceWindowLayout, List<IButton> buttonList,
            NodeSourceAction nodeSourceAction) {

        this.nodeSourcePluginsForm.setValue(DEPLOY_FORM_KEY, Boolean.TRUE.toString());

        saveNodeSource(nodeSourceWindowLayout, buttonList, nodeSourceAction);
    }

    private void saveNodeSource(VLayout nodeSourceWindowLayout, List<IButton> buttonList,
            NodeSourceAction nodeSourceAction) {

        this.nodeSourcePluginsForm.setValue(INFRASTRUCTURE_FORM_KEY, this.infrastructureSelectItem.getValueAsString());
        this.nodeSourcePluginsForm.setValue(NS_NAME_FORM_KEY, this.nodeSourceNameText.getValueAsString());
        this.nodeSourcePluginsForm.setValue(NODES_RECOVERABLE_FORM_KEY,
                                            this.nodesRecoverableCheckbox.getValueAsBoolean().toString());
        this.nodeSourcePluginsForm.setValue(POLICY_FORM_KEY, this.policySelectItem.getValueAsString());
        this.nodeSourcePluginsForm.setValue(SESSION_ID_FORM_KEY, LoginModel.getInstance().getSessionId());
        this.nodeSourcePluginsForm.setValue(NODE_SOURCE_ACTION_FORM_KEY, nodeSourceAction.getActionDescription());
        this.nodeSourcePluginsForm.setCanSubmit(true);

        this.nodeSourcePluginsForm.setValue(NS_CALLBACK_FORM_KEY, JSUtil.register(javascriptObject -> {

            JSONObject jsonCallback = new JSONObject(javascriptObject);

            if (jsonCallback.containsKey("result") && jsonCallback.get("result").isBoolean().booleanValue()) {

                this.window.hide();
                LogModel.getInstance().logMessage("Successfully applied action to Node Source: " +
                                                  this.nodeSourceNameText.getValueAsString());

            } else {

                handleNodeSourceCreationError(nodeSourceWindowLayout, this.nodeSourceNameText, jsonCallback);
            }

            this.nodeSourcePluginsWaitingLabel.hide();
            this.nodeSourcePluginsForm.show();

            for (IButton button : buttonList) {
                button.setDisabled(false);
            }
        }));

        this.nodeSourcePluginsForm.submitForm();

        for (IButton button : buttonList) {
            button.setDisabled(true);
        }

        this.nodeSourcePluginsWaitingLabel.setContents("Node Source action requested...");
        this.nodeSourcePluginsWaitingLabel.show();
        this.nodeSourcePluginsForm.hide();
    }

    private void handleNodeSourceCreationError(VLayout nodeSourceWindowLayout, TextItem nodeSourceNameItem,
            JSONObject jsonCallback) {

        String msg;
        if (jsonCallback.get("errorMessage").isString() != null) {
            msg = jsonCallback.get("errorMessage").isString().stringValue();
        } else {
            msg = jsonCallback.toString();
        }

        this.nodeSourceWindowLabel.setContents("<span style='color:red'>Failed to apply action to Node Source :<br>" +
                                               msg + "</span>");
        LogModel.getInstance().logImportantMessage("Failed to apply action to Node Source " +
                                                   nodeSourceNameItem.getValueAsString() + ": " + msg);
        nodeSourceWindowLayout.scrollToTop();
    }

    public void addCredentialsPickerIcon(PluginDescriptor.Field pluginField, List<FormItem> formItems,
            FormItem formItem) {
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

        formItems.add(formItem);
    }

    public String getFocusedInfrastructurePluginName() {
        return focusedInfrastructurePluginName;
    }

    public String getFocusedPolicyPluginName() {
        return focusedPolicyPluginName;
    }

    public void importNodeSourceFromJson(String importedNodeSourceJsonString) {
        this.nodeSourcePluginsForm.reset();
        NodeSourceWindow.this.createdFromImport = true;
        NodeSourceConfiguration nodeSourceConfiguration;
        try {
            nodeSourceConfiguration = new NodeSourceConfigurationParser(NodeSourceWindow.this.controller).parseNodeSourceConfiguration(importedNodeSourceJsonString);
            this.nodeSourceNameText.setDefaultValue(nodeSourceConfiguration.getNodeSourceName());
            this.nodesRecoverableCheckbox.setValue(nodeSourceConfiguration.getNodesRecoverable());
            manageNodeSourceWindowItems();

            LinkedHashMap<String, String> selectItemValues = new LinkedHashMap<>();
            NodeSourceWindow.this.allFormItems = prepareFormItems();

            PluginDescriptor focusedInfrastructurePlugin = nodeSourceConfiguration.getInfrastructurePluginDescriptor();
            NodeSourceWindow.this.focusedInfrastructurePluginName = focusedInfrastructurePlugin.getPluginName();
            NodeSourceWindow.this.allFormItems.add(NodeSourceWindow.this.infrastructureSelectItem);
            fillFocusedPluginValues(selectItemValues, focusedInfrastructurePlugin);
            handleAdditionalInfrastructureFormItems(selectItemValues, focusedInfrastructurePlugin);
            NodeSourceWindow.this.infrastructureSelectItem.setValueMap(selectItemValues);
            NodeSourceWindow.this.previousSelectedInfrastructure = focusedInfrastructurePlugin.getPluginName();
            NodeSourceWindow.this.infrastructureSelectItem.setValue(NodeSourceWindow.this.previousSelectedInfrastructure);

            NodeSourceWindow.this.allFormItems.add(new SpacerItem());
            selectItemValues.clear();

            PluginDescriptor focusedPolicyPlugin = nodeSourceConfiguration.getPolicyPluginDescriptor();
            NodeSourceWindow.this.focusedPolicyPluginName = focusedPolicyPlugin.getPluginName();
            NodeSourceWindow.this.allFormItems.add(NodeSourceWindow.this.policySelectItem);
            fillFocusedPluginValues(selectItemValues, focusedPolicyPlugin);
            handleAdditionalPolicyFormItems(selectItemValues, focusedPolicyPlugin);
            NodeSourceWindow.this.policySelectItem.setValueMap(selectItemValues);
            NodeSourceWindow.this.previousSelectedPolicy = focusedPolicyPlugin.getPluginName();
            NodeSourceWindow.this.policySelectItem.setValue(NodeSourceWindow.this.previousSelectedPolicy);

            NodeSourceWindow.this.infrastructureSelectItem.addChangedHandler(changedEvent -> resetFormForInfrastructureSelectChange());
            NodeSourceWindow.this.policySelectItem.addChangedHandler(changedEvent -> resetFormForPolicySelectChange());

            NodeSourceWindow.this.allFormItems = modifyFormItemsAfterCreation(focusedInfrastructurePlugin,
                                                                              focusedPolicyPlugin);

            this.nodeSourcePluginsForm.setFields(NodeSourceWindow.this.allFormItems.toArray(new FormItem[NodeSourceWindow.this.allFormItems.size()]));
            this.nodeSourcePluginsForm.show();
        } catch (ImportException e) {
            this.nodeSourceWindowLabel.setContents("<span style='color:red'>Failed to import Node Source :<br>" +
                                                   e.getMessage() + "</span>");
        } finally {
            NodeSourceWindow.this.createdFromImport = false;
        }
    }

}
