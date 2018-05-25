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
package org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.window;

import static org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.InlineItemModificationCreator.EDIT_FORM_ITEM_SUFFIX;
import static org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.InlineItemModificationCreator.EDIT_OR_UPLOAD_FORM_ITEM_SUFFIX;

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
import org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.ImportException;
import org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.NodeSourceConfigurationParser;
import org.ow2.proactive_grid_cloud_portal.rm.server.ImportNodeSourceServlet;

import com.google.gwt.core.client.GWT;
import com.google.gwt.json.client.JSONObject;
import com.google.gwt.user.client.ui.FileUpload;
import com.google.gwt.user.client.ui.FormPanel;
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
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.layout.VStack;


public abstract class NodeSourceWindow {

    private static final String NS_NAME_FORM_KEY = "nsName";

    private static final String NODES_RECOVERABLE_FORM_KEY = "nodesRecoverable";

    public static final String DEPLOY_FORM_KEY = "deploy";

    public static final String NODE_SOURCE_ACTION_FORM_KEY = "nodeSourceAction";

    public static final String INFRASTRUCTURE_FORM_KEY = "infra";

    public static final String POLICY_FORM_KEY = "policy";

    public static final String SESSION_ID_FORM_KEY = "sessionId";

    public static final String NS_CALLBACK_FORM_KEY = "nsCallback";

    protected RMController controller;

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

    protected abstract void populateFormValues(Label windowLabel, DynamicForm windowForm, TextItem nodeSourceNameItem,
            CheckboxItem nodesRecoverableItem);

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

        VStack nodeSourcePluginsLayout = new VStack();
        nodeSourcePluginsLayout.setHeight(26);

        Label nodeSourcePluginsWaitingLabel = new Label(this.waitingMessage);
        nodeSourcePluginsWaitingLabel.setIcon("loading.gif");
        nodeSourcePluginsWaitingLabel.setHeight(26);
        nodeSourcePluginsWaitingLabel.setAlign(Alignment.CENTER);
        nodeSourcePluginsLayout.addMember(nodeSourcePluginsWaitingLabel);

        DynamicForm nodeSourcePluginsForm = new DynamicForm();
        nodeSourcePluginsForm.setEncoding(Encoding.MULTIPART);
        nodeSourcePluginsForm.setMethod(FormMethod.POST);
        nodeSourcePluginsForm.setAction(GWT.getModuleBaseURL() + "createnodesource");
        nodeSourcePluginsForm.setTarget("__hiddenFrame");
        nodeSourcePluginsForm.setTitleSuffix("");

        nodeSourcePluginsLayout.addMember(nodeSourcePluginsForm);

        Label nodeSourceWindowLabel = new Label("A Node Source is a combination of an Infrastructure, which defines how resources" +
                                                " will be acquired, and a Policy, that dictates when resources can be acquired.");
        nodeSourceWindowLabel.setHeight(40);

        TextItem nodeSourceNameItem = new TextItem(NS_NAME_FORM_KEY, "Name");
        CheckboxItem nodesRecoverableItem = new CheckboxItem(NODES_RECOVERABLE_FORM_KEY, "Nodes Recoverable");
        nodesRecoverableItem.setTooltip("Defines whether the nodes of this node source can be recovered after a crash of the Resource Manager");

        Label importNodeSourceLabel = new Label("Import Node Source");
        FileUpload fileUpload = new FileUpload();
        fileUpload.setName("ImportNS");
        final FormPanel importNodeSourceFormPanel = new ImportNodeSourcePanelBuilder(nodeSourceWindowLabel,
                                                                                     nodeSourcePluginsForm,
                                                                                     nodeSourceNameItem,
                                                                                     nodesRecoverableItem,
                                                                                     fileUpload).build();

        HLayout importNodeSourceLayout = new HLayout();
        importNodeSourceLayout.setLayoutAlign(Alignment.RIGHT);
        importNodeSourceLayout.addMember(importNodeSourceLabel);
        importNodeSourceLayout.addMember(importNodeSourceFormPanel);

        DynamicForm nodeSourceWindowForm = new DynamicForm();
        nodeSourceWindowForm.setFields(nodeSourceNameItem, nodesRecoverableItem);
        nodeSourceWindowForm.setTitleSuffix("");

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
                                                                                                   nodeSourcePluginsWaitingLabel,
                                                                                                   nodeSourcePluginsForm,
                                                                                                   nodeSourceWindowLabel,
                                                                                                   nodeSourceNameItem,
                                                                                                   nodesRecoverableItem,
                                                                                                   buttonList,
                                                                                                   getNodeSourceAction()));

        this.deployNowButton.addClickHandler(clickEvent -> saveAndDeployNodeSource(nodeSourceWindowLayout,
                                                                                   nodeSourcePluginsWaitingLabel,
                                                                                   nodeSourcePluginsForm,
                                                                                   nodeSourceWindowLabel,
                                                                                   nodeSourceNameItem,
                                                                                   nodesRecoverableItem,
                                                                                   buttonList,
                                                                                   getNodeSourceAction()));

        this.saveAndKeepUndeployedButton.addClickHandler(clickEvent -> saveNodeSource(nodeSourceWindowLayout,
                                                                                      nodeSourcePluginsWaitingLabel,
                                                                                      nodeSourcePluginsForm,
                                                                                      nodeSourceWindowLabel,
                                                                                      nodeSourceNameItem,
                                                                                      nodesRecoverableItem,
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

        nodeSourceWindowLayout.addMember(nodeSourceWindowLabel);
        nodeSourceWindowLayout.addMember(importNodeSourceLayout);
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
    protected void manageNodeSourceWindowItems(TextItem nodeSourceNameItem, CheckboxItem nodesRecoverableItem) {

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

    private void applyModificationsToNodeSource(VLayout nodeSourceWindowLayout, Label nodeSourcePluginsWaitingLabel,
            DynamicForm nodeSourcePluginsForm, Label nodeSourceWindowLabel, TextItem nodeSourceNameItem,
            CheckboxItem nodesRecoverableItem, List<IButton> buttonList, NodeSourceAction nodeSourceAction) {

        nodeSourcePluginsForm.setValue(DEPLOY_FORM_KEY, Boolean.FALSE.toString());

        saveNodeSource(nodeSourceWindowLayout,
                       nodeSourcePluginsWaitingLabel,
                       nodeSourcePluginsForm,
                       nodeSourceWindowLabel,
                       nodeSourceNameItem,
                       nodesRecoverableItem,
                       buttonList,
                       nodeSourceAction);
    }

    private void saveAndDeployNodeSource(VLayout nodeSourceWindowLayout, Label nodeSourcePluginsWaitingLabel,
            DynamicForm nodeSourcePluginsForm, Label nodeSourceWindowLabel, TextItem nodeSourceNameItem,
            CheckboxItem nodesRecoverableItem, List<IButton> buttonList, NodeSourceAction nodeSourceAction) {

        nodeSourcePluginsForm.setValue(DEPLOY_FORM_KEY, Boolean.TRUE.toString());

        saveNodeSource(nodeSourceWindowLayout,
                       nodeSourcePluginsWaitingLabel,
                       nodeSourcePluginsForm,
                       nodeSourceWindowLabel,
                       nodeSourceNameItem,
                       nodesRecoverableItem,
                       buttonList,
                       nodeSourceAction);
    }

    private void saveNodeSource(VLayout nodeSourceWindowLayout, Label nodeSourcePluginsWaitingLabel,
            DynamicForm nodeSourcePluginsForm, Label nodeSourceWindowLabel, TextItem nodeSourceNameItem,
            CheckboxItem nodesRecoverableItem, List<IButton> buttonList, NodeSourceAction nodeSourceAction) {

        nodeSourcePluginsForm.setValue(INFRASTRUCTURE_FORM_KEY, this.infrastructureSelectItem.getValueAsString());
        nodeSourcePluginsForm.setValue(NS_NAME_FORM_KEY, nodeSourceNameItem.getValueAsString());
        nodeSourcePluginsForm.setValue(NODES_RECOVERABLE_FORM_KEY, nodesRecoverableItem.getValueAsBoolean().toString());
        nodeSourcePluginsForm.setValue(POLICY_FORM_KEY, this.policySelectItem.getValueAsString());
        nodeSourcePluginsForm.setValue(SESSION_ID_FORM_KEY, LoginModel.getInstance().getSessionId());
        nodeSourcePluginsForm.setValue(NODE_SOURCE_ACTION_FORM_KEY, nodeSourceAction.getActionDescription());
        nodeSourcePluginsForm.setCanSubmit(true);

        nodeSourcePluginsForm.setValue(NS_CALLBACK_FORM_KEY, JSUtil.register(javascriptObject -> {

            JSONObject jsonCallback = new JSONObject(javascriptObject);

            if (jsonCallback.containsKey("result") && jsonCallback.get("result").isBoolean().booleanValue()) {

                this.window.hide();
                LogModel.getInstance().logMessage("Successfully applied action to Node Source: " +
                                                  nodeSourceNameItem.getValueAsString());

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

        nodeSourcePluginsWaitingLabel.setContents("Node Source action requested...");
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

        nodeSourceWindowLabel.setContents("<span style='color:red'>Failed to apply action to Node Source :<br>" + msg +
                                          "</span>");
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

    private class ImportNodeSourcePanelBuilder {

        private Label nodeSourceWindowLabel;

        private DynamicForm nodeSourcePluginsForm;

        private TextItem nodeSourceNameItem;

        private CheckboxItem nodesRecoverableItem;

        private FileUpload fileUpload;

        public ImportNodeSourcePanelBuilder(Label nodeSourceWindowLabel, DynamicForm nodeSourcePluginsForm,
                TextItem nodeSourceNameItem, CheckboxItem nodesRecoverableItem, FileUpload fileUpload) {
            this.nodeSourceWindowLabel = nodeSourceWindowLabel;
            this.nodeSourcePluginsForm = nodeSourcePluginsForm;
            this.nodeSourceNameItem = nodeSourceNameItem;
            this.nodesRecoverableItem = nodesRecoverableItem;
            this.fileUpload = fileUpload;
        }

        public FormPanel build() {

            final FormPanel importNodeSourceFormPanel = new FormPanel();
            importNodeSourceFormPanel.setEncoding(FormPanel.ENCODING_MULTIPART);
            importNodeSourceFormPanel.setMethod(FormPanel.METHOD_POST);
            importNodeSourceFormPanel.setAction(GWT.getModuleBaseURL() + ImportNodeSourceServlet.SERVLET_MAPPING);
            importNodeSourceFormPanel.addSubmitCompleteHandler(this::handleNodeSourceImport);

            fileUpload.addChangeHandler(onImport -> {
                if (!fileUpload.getFilename().isEmpty()) {
                    importNodeSourceFormPanel.submit();
                }
            });
            importNodeSourceFormPanel.add(fileUpload);
            return importNodeSourceFormPanel;
        }

        private void handleNodeSourceImport(FormPanel.SubmitCompleteEvent importCompleteEvent) {

            this.nodeSourcePluginsForm.reset();
            NodeSourceWindow.this.createdFromImport = true;
            String importedNodeSourceJsonString = importCompleteEvent.getResults();

            NodeSourceConfiguration nodeSourceConfiguration;

            try {
                nodeSourceConfiguration = new NodeSourceConfigurationParser(NodeSourceWindow.this.controller).parseNodeSourceConfiguration(importedNodeSourceJsonString);

                nodeSourceNameItem.setDefaultValue(nodeSourceConfiguration.getNodeSourceName());
                nodesRecoverableItem.setValue(nodeSourceConfiguration.getNodesRecoverable());

                manageNodeSourceWindowItems(nodeSourceNameItem, nodesRecoverableItem);

                LinkedHashMap<String, String> selectItemValues = new LinkedHashMap<>();

                NodeSourceWindow.this.allFormItems = prepareFormItems();

                PluginDescriptor focusedInfrastructurePlugin = nodeSourceConfiguration.getInfrastructurePluginDescriptor();
                NodeSourceWindow.this.focusedInfrastructurePluginName = focusedInfrastructurePlugin.getPluginName();
                NodeSourceWindow.this.allFormItems.add(NodeSourceWindow.this.infrastructureSelectItem);
                fillFocusedPluginValues(selectItemValues, focusedInfrastructurePlugin);
                handleAdditionalInfrastructureFormItems(selectItemValues, focusedInfrastructurePlugin);
                NodeSourceWindow.this.infrastructureSelectItem.setValueMap(selectItemValues);
                NodeSourceWindow.this.infrastructureSelectItem.setDefaultToFirstOption(true);
                NodeSourceWindow.this.previousSelectedInfrastructure = focusedInfrastructurePlugin.getPluginName();

                NodeSourceWindow.this.allFormItems.add(new SpacerItem());
                selectItemValues.clear();

                PluginDescriptor focusedPolicyPlugin = nodeSourceConfiguration.getPolicyPluginDescriptor();
                NodeSourceWindow.this.focusedPolicyPluginName = focusedPolicyPlugin.getPluginName();
                NodeSourceWindow.this.allFormItems.add(NodeSourceWindow.this.policySelectItem);
                fillFocusedPluginValues(selectItemValues, focusedPolicyPlugin);
                handleAdditionalPolicyFormItems(selectItemValues, focusedPolicyPlugin);
                NodeSourceWindow.this.policySelectItem.setValueMap(selectItemValues);
                NodeSourceWindow.this.policySelectItem.setDefaultToFirstOption(true);
                NodeSourceWindow.this.previousSelectedPolicy = focusedPolicyPlugin.getPluginName();

                NodeSourceWindow.this.infrastructureSelectItem.addChangedHandler(changedEvent -> resetFormForInfrastructureSelectChange());
                NodeSourceWindow.this.policySelectItem.addChangedHandler(changedEvent -> resetFormForPolicySelectChange());

                NodeSourceWindow.this.allFormItems = modifyFormItemsAfterCreation(focusedInfrastructurePlugin,
                                                                                  focusedPolicyPlugin);

                nodeSourcePluginsForm.setFields(NodeSourceWindow.this.allFormItems.toArray(new FormItem[NodeSourceWindow.this.allFormItems.size()]));
                nodeSourcePluginsForm.show();
            } catch (ImportException e) {
                nodeSourceWindowLabel.setContents("<span style='color:red'>Failed to import Node Source :<br>" +
                                                  e.getMessage() + "</span>");
            } finally {
                NodeSourceWindow.this.createdFromImport = false;
            }
        }

    }

}
