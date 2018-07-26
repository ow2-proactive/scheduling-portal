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
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.ow2.proactive_grid_cloud_portal.common.client.CredentialsWindow;
import org.ow2.proactive_grid_cloud_portal.common.client.Images;
import org.ow2.proactive_grid_cloud_portal.common.client.JSUtil;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LogModel;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LoginModel;
import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSourceConfiguration;
import org.ow2.proactive_grid_cloud_portal.rm.client.PluginDescriptor;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMController;
import org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.serialization.NodeSourceConfigurationParser;
import org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.serialization.load.ImportInfrastructureLayout;
import org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.serialization.load.ImportNodeSourceLayout;
import org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.serialization.load.ImportPolicyLayout;
import org.ow2.proactive_grid_cloud_portal.rm.shared.NodeSourceAction;

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
import com.smartgwt.client.widgets.form.fields.RowSpacerItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SpacerItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.LayoutSpacer;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.layout.VStack;


public abstract class NodeSourceWindow {

    private static final String WINDOW_HEADER = "A Node Source is a combination of an Infrastructure, which defines how resources" +
                                                " will be acquired, and a Policy, that dictates when resources can be acquired.";

    protected static final String INFRASTRUCTURE_FORM_KEY = "infra";

    protected static final String POLICY_FORM_KEY = "policy";

    private static final String NS_NAME_FORM_KEY = "nsName";

    private static final String NODES_RECOVERABLE_FORM_KEY = "nodesRecoverable";

    private static final String DEPLOY_FORM_KEY = "deploy";

    private static final String NODE_SOURCE_ACTION_FORM_KEY = "nodeSourceAction";

    private static final String SESSION_ID_FORM_KEY = "sessionId";

    private static final String NS_CALLBACK_FORM_KEY = "nsCallback";

    public SelectItem infrastructureSelectItem;

    public SelectItem policySelectItem;

    protected RMController controller;

    protected CheckboxItem nodesRecoverableCheckbox;

    protected TextItem nodeSourceNameText;

    protected Label generalParametersLabel;

    protected boolean createdFromImport;

    protected IButton deployNowButton;

    protected IButton saveAndKeepUndeployedButton;

    protected IButton cancelButton;

    protected IButton applyModificationsButton;

    protected Window window;

    protected DynamicForm nodeSourcePluginsForm;

    /**
     * All items of the node source form are held in this map. The key is
     * either the name of the FormItem if there is only one element in the
     * list, or the name of the plugin to which the form items belong, given
     * by PluginDescriptor#getPluginName. Note that this map must be ordered
     * to achieve a correct display.
     */
    private Map<String, List<FormItem>> formItemsByName;

    private Label nodeSourceWindowLabel;

    private Label nodeSourcePluginsWaitingLabel;

    private String previousSelectedInfrastructure;

    private String previousSelectedPolicy;

    private String windowTitle;

    private String waitingMessage;

    protected NodeSourceWindow(RMController controller, String windowTitle, String waitingMessage) {
        this.controller = controller;
        this.windowTitle = windowTitle;
        this.createdFromImport = false;
        this.waitingMessage = waitingMessage;
        this.formItemsByName = new LinkedHashMap<>();
    }

    protected abstract NodeSourceAction getNodeSourceAction();

    protected abstract List<FormItem> handleNonTextualPluginField(PluginDescriptor plugin,
            PluginDescriptor.Field pluginField);

    protected abstract void modifyFormItemsAfterCreation();

    protected abstract void addButtonsToButtonsLayout(HLayout buttonsLayout);

    protected void buildForm() {
        VLayout nodeSourceWindowLayout = new VLayout();
        nodeSourceWindowLayout.setMargin(5);
        nodeSourceWindowLayout.setMembersMargin(5);
        HLayout nodeSourceWindowSubLayoutTop = new HLayout(5);
        HLayout nodeSourceWindowSubLayoutBottom = new HLayout(5);

        VStack nodeSourcePluginsLayout = new VStack();
        nodeSourcePluginsLayout.setHeight(26);
        this.generalParametersLabel = new Label("General Parameters :");
        this.generalParametersLabel.setStyleName("generalParametersStyle");
        this.generalParametersLabel.setHeight("20px");
        this.generalParametersLabel.setMargin(5);

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

        this.nodeSourceWindowLabel = new Label(WINDOW_HEADER);
        this.nodeSourceWindowLabel.setHeight(40);

        VLayout createNodeSourceLayout = new VLayout();
        Label createNodeSourceLabel = new Label("Create Node Source");
        createNodeSourceLabel.setHeight("20px");
        createNodeSourceLayout.addMember(createNodeSourceLabel);
        createNodeSourceLayout.setPadding(10);
        createNodeSourceLayout.setWidth("75%");
        this.nodeSourceNameText = new TextItem(NS_NAME_FORM_KEY, "Name");
        Layout importNodeSourceLayout = new ImportNodeSourceLayout(this,
                                                                   "or Import Node Source",
                                                                   getNodeSourceAction());
        DynamicForm nodeSourceWindowForm = new DynamicForm();
        nodeSourceWindowForm.setWidth100();
        nodeSourceWindowForm.setHeight("50px");
        nodeSourceWindowForm.setFields(this.nodeSourceNameText);
        nodeSourceWindowForm.setTitleSuffix("");
        createNodeSourceLayout.addMember(nodeSourceWindowForm);

        this.populateFormValues();

        HLayout buttonsLayout = new HLayout();
        buttonsLayout.setWidth100();
        buttonsLayout.setHeight(22);
        buttonsLayout.setMargin(5);
        buttonsLayout.setAlign(Alignment.RIGHT);
        buttonsLayout.setMembersMargin(5);
        createButtons(nodeSourceWindowLayout);
        addButtonsToButtonsLayout(buttonsLayout);

        VLayout scrollLayout = new VLayout();
        scrollLayout.setHeight100();
        scrollLayout.setWidth100();
        scrollLayout.setMembers(generalParametersLabel, nodeSourcePluginsLayout);
        scrollLayout.setOverflow(Overflow.AUTO);
        scrollLayout.setBorder("1px solid #ddd");
        scrollLayout.setBackgroundColor("#fafafa");
        VLayout importLayout = new VLayout();
        importLayout.setMembers(new LayoutSpacer("100%", "15%"),
                                new ImportInfrastructureLayout(this, "Import Infrastructure", getNodeSourceAction()),
                                new LayoutSpacer("100%", "20%"),
                                new ImportPolicyLayout(this, "Import Policy", getNodeSourceAction()),
                                new LayoutSpacer("100%", "45%"));

        nodeSourceWindowLayout.addMember(this.nodeSourceWindowLabel);
        nodeSourceWindowSubLayoutTop.setMembers(createNodeSourceLayout, importNodeSourceLayout);
        nodeSourceWindowSubLayoutBottom.setHeight100();
        nodeSourceWindowSubLayoutBottom.setWidth100();
        nodeSourceWindowSubLayoutBottom.setMembers(scrollLayout, importLayout);

        nodeSourceWindowLayout.setMembers(this.nodeSourceWindowLabel,
                                          nodeSourceWindowSubLayoutTop,
                                          nodeSourceWindowSubLayoutBottom,
                                          buttonsLayout);

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

    private void createButtons(VLayout nodeSourceWindowLayout) {
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
    }

    private void populateFormValues() {
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

            modifyFormItemsAfterCreation();

            this.nodeSourcePluginsForm.setFields(this.formItemsByName.values()
                                                                     .stream()
                                                                     .flatMap(Collection::stream)
                                                                     .toArray(FormItem[]::new));
            this.nodeSourcePluginsWaitingLabel.hide();
            this.nodeSourcePluginsForm.show();
            hideAllPluginFormItems();
        }, this.window::hide);
    }

    private void resetFormForPolicySelectChange() {
        String policyPluginName = this.policySelectItem.getValueAsString();
        if (this.previousSelectedPolicy != null) {
            for (FormItem formItem : this.formItemsByName.get(this.previousSelectedPolicy)) {
                formItem.hide();
            }
        }
        for (FormItem formItem : this.formItemsByName.get(policyPluginName)) {
            formItem.show();
        }
        this.previousSelectedPolicy = policyPluginName;
    }

    private void resetFormForInfrastructureSelectChange() {
        if (this.previousSelectedInfrastructure != null) {
            for (FormItem formItem : this.formItemsByName.get(this.previousSelectedInfrastructure)) {
                formItem.hide();
            }
        }
        String infrastructurePluginName = this.infrastructureSelectItem.getValueAsString();
        for (FormItem formItem : this.formItemsByName.get(infrastructurePluginName)) {
            formItem.show();
        }
        this.previousSelectedInfrastructure = infrastructurePluginName;
    }

    private void hideAllPluginFormItems() {
        for (List<FormItem> li : this.formItemsByName.values()) {
            // if there are more than one element in the list, it means that
            // those form items belong to a specific plugin
            if (li.size() > 1) {
                for (FormItem it : li) {
                    it.hide();
                }
            }
        }
    }

    private void prepareFormItems() {
        this.infrastructureSelectItem = new SelectItem(INFRASTRUCTURE_FORM_KEY, "Infrastructure");
        this.infrastructureSelectItem.setRequired(true);
        this.infrastructureSelectItem.setWidth(300);
        this.policySelectItem = new SelectItem(POLICY_FORM_KEY, "Policy");
        this.policySelectItem.setRequired(true);
        this.policySelectItem.setWidth(300);

        HiddenItem name = new HiddenItem(NS_NAME_FORM_KEY);
        HiddenItem deploy = new HiddenItem(DEPLOY_FORM_KEY);
        HiddenItem nodeSourceAction = new HiddenItem(NODE_SOURCE_ACTION_FORM_KEY);
        HiddenItem callback = new HiddenItem(NS_CALLBACK_FORM_KEY);
        HiddenItem session = new HiddenItem(SESSION_ID_FORM_KEY);

        this.formItemsByName.put(NS_NAME_FORM_KEY, Collections.singletonList(name));
        this.formItemsByName.put(DEPLOY_FORM_KEY, Collections.singletonList(deploy));
        this.formItemsByName.put(NODE_SOURCE_ACTION_FORM_KEY, Collections.singletonList(nodeSourceAction));
        this.formItemsByName.put(NS_CALLBACK_FORM_KEY, Collections.singletonList(callback));
        this.formItemsByName.put(SESSION_ID_FORM_KEY, Collections.singletonList(session));

        this.nodesRecoverableCheckbox = new CheckboxItem(NODES_RECOVERABLE_FORM_KEY, "Nodes Recoverable");
        this.nodesRecoverableCheckbox.setTooltip("Defines whether the nodes of this node source can be recovered after a crash of the Resource Manager");
        this.formItemsByName.put(NODES_RECOVERABLE_FORM_KEY, Collections.singletonList(this.nodesRecoverableCheckbox));
        this.formItemsByName.put("generalParametersSpacer", Collections.singletonList(new RowSpacerItem()));
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
            List<FormItem> currentPluginFormItems = getPrefilledFormItems(pluginDescriptor);
            this.formItemsByName.put(pluginDescriptor.getPluginName(), currentPluginFormItems);
        }
    }

    private List<FormItem> getPrefilledFormItems(PluginDescriptor plugin) {
        List<PluginDescriptor.Field> pluginFields = plugin.getConfigurableFields();
        List<FormItem> allFormItems = new ArrayList<>(pluginFields.size());
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
            allFormItems.addAll(formItemsForField);
            formItemsForField.clear();
        }
        return allFormItems;
    }

    private String getPluginShortName(PluginDescriptor plugin) {
        return plugin.getPluginName().substring(plugin.getPluginName().lastIndexOf('.') + 1);
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
        String nodeSourceName = this.nodeSourceNameText.getValueAsString();
        Boolean nodesRecoverable = this.nodesRecoverableCheckbox.getValueAsBoolean();
        String actionDescription = nodeSourceAction.getActionDescription();

        this.nodeSourcePluginsForm.setValue(INFRASTRUCTURE_FORM_KEY, this.infrastructureSelectItem.getValueAsString());
        this.nodeSourcePluginsForm.setValue(NS_NAME_FORM_KEY, nodeSourceName);
        this.nodeSourcePluginsForm.setValue(NODES_RECOVERABLE_FORM_KEY, nodesRecoverable.toString());
        this.nodeSourcePluginsForm.setValue(POLICY_FORM_KEY, this.policySelectItem.getValueAsString());
        this.nodeSourcePluginsForm.setValue(SESSION_ID_FORM_KEY, LoginModel.getInstance().getSessionId());
        this.nodeSourcePluginsForm.setValue(NODE_SOURCE_ACTION_FORM_KEY, actionDescription);
        this.nodeSourcePluginsForm.setValue(NS_CALLBACK_FORM_KEY, JSUtil.register(javascriptObject -> {
            JSONObject jsonCallback = new JSONObject(javascriptObject);
            if (jsonCallback.containsKey("result") && jsonCallback.get("result").isBoolean().booleanValue()) {
                this.window.hide();
                LogModel.getInstance().logImportantMessage("Successfully applied '" + actionDescription +
                                                           "' action to Node Source: " + nodeSourceName);
            } else {
                modifyFormItemsAfterCreation();
                handleNodeSourceCreationError(nodeSourceWindowLayout, jsonCallback);
            }
            this.nodeSourcePluginsWaitingLabel.hide();
            this.nodeSourcePluginsForm.show();
            this.nodesRecoverableCheckbox.setValue(nodesRecoverable);
            for (IButton button : buttonList) {
                button.setDisabled(false);
            }
        }));

        Arrays.stream(this.nodeSourcePluginsForm.getFields()).forEach(FormItem::enable);
        this.nodeSourcePluginsForm.setCanSubmit(true);
        this.nodeSourcePluginsForm.submitForm();

        for (IButton button : buttonList) {
            button.setDisabled(true);
        }

        this.nodeSourcePluginsWaitingLabel.setContents("Node Source action requested...");
        this.nodeSourcePluginsWaitingLabel.show();
        this.nodeSourcePluginsForm.hide();
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

    public void importNodeSourceFromJson(String importedNodeSourceJsonString) {
        NodeSourceConfiguration nodeSourceConfiguration;
        try {
            nodeSourceConfiguration = new NodeSourceConfigurationParser().parseNodeSourceConfiguration(importedNodeSourceJsonString);
            this.nodeSourceNameText.setValue(nodeSourceConfiguration.getNodeSourceName());
            this.nodesRecoverableCheckbox.setValue(nodeSourceConfiguration.getNodesRecoverable());
            fillPluginFormItems(nodeSourceConfiguration);
            modifyFormItemsAfterCreation();
        } catch (Exception e) {
            setNodeSourceWindowLabelWithError("Failed to import Node Source", e);
        }
    }

    public void setCreatedFromImport() {
        this.createdFromImport = true;
    }

    public void resetCreatedFromImport() {
        this.createdFromImport = false;
    }

    protected void fillPluginFormItems(NodeSourceConfiguration nodeSourceConfiguration) {
        List<FormItem> allNodeSourcePluginsFormItems = Arrays.stream(this.nodeSourcePluginsForm.getFields())
                                                             .collect(Collectors.toList());
        replaceInfrastructureItemsInItemList(nodeSourceConfiguration.getInfrastructurePluginDescriptor(),
                                             allNodeSourcePluginsFormItems);
        replacePolicyItemsInItemList(nodeSourceConfiguration.getPolicyPluginDescriptor(),
                                     allNodeSourcePluginsFormItems);
        this.nodeSourcePluginsForm.setFields(allNodeSourcePluginsFormItems.toArray(new FormItem[0]));
    }

    public void replacePolicyItems(PluginDescriptor policyPluginDescriptor) {
        List<FormItem> allNodeSourcePluginsFormItems = Arrays.stream(this.nodeSourcePluginsForm.getFields())
                                                             .collect(Collectors.toList());
        replacePolicyItemsInItemList(policyPluginDescriptor, allNodeSourcePluginsFormItems);
        this.nodeSourcePluginsForm.setFields(allNodeSourcePluginsFormItems.toArray(new FormItem[0]));
    }

    private void replacePolicyItemsInItemList(PluginDescriptor policyPluginDescriptor,
            List<FormItem> allNodeSourcePluginsFormItems) {
        validatePolicyNameOrFail(policyPluginDescriptor.getPluginName());
        allNodeSourcePluginsFormItems.stream()
                                     .filter(formItem -> formItem.getName()
                                                                 .startsWith(policyPluginDescriptor.getPluginName()))
                                     .forEach(FormItem::clearValue);
        allNodeSourcePluginsFormItems.removeIf(formItem -> formItem.getName()
                                                                   .startsWith(policyPluginDescriptor.getPluginName()));

        this.previousSelectedPolicy = this.policySelectItem.getValueAsString();
        this.policySelectItem.setValue(policyPluginDescriptor.getPluginName());
        List<FormItem> prefilledFormItems = getPrefilledFormItems(policyPluginDescriptor);
        allNodeSourcePluginsFormItems.addAll(findFormItemIndexByName(POLICY_FORM_KEY, allNodeSourcePluginsFormItems) +
                                             1, prefilledFormItems);
        this.formItemsByName.put(policyPluginDescriptor.getPluginName(), prefilledFormItems);
        resetFormForPolicySelectChange();
    }

    private void validatePolicyNameOrFail(String policyPluginName) {
        if (this.controller.getModel()
                           .getSupportedPolicies()
                           .values()
                           .stream()
                           .noneMatch(pluginDescriptor -> pluginDescriptor.getPluginName().equals(policyPluginName))) {
            throw new IllegalArgumentException("Policy " + policyPluginName + " does not exist");
        }
    }

    public void replaceInfrastructureItems(PluginDescriptor infrastructurePluginDescriptor) {
        List<FormItem> allNodeSourcePluginsFormItems = Arrays.stream(this.nodeSourcePluginsForm.getFields())
                                                             .collect(Collectors.toList());
        replaceInfrastructureItemsInItemList(infrastructurePluginDescriptor, allNodeSourcePluginsFormItems);
        this.nodeSourcePluginsForm.setFields(allNodeSourcePluginsFormItems.toArray(new FormItem[0]));
    }

    private void replaceInfrastructureItemsInItemList(PluginDescriptor infrastructurePluginDescriptor,
            List<FormItem> allNodeSourcePluginsFormItems) {
        validateInfrastructureNameOrFail(infrastructurePluginDescriptor.getPluginName());
        allNodeSourcePluginsFormItems.stream()
                                     .filter(formItem -> formItem.getName()
                                                                 .startsWith(infrastructurePluginDescriptor.getPluginName()))
                                     .forEach(FormItem::clearValue);
        allNodeSourcePluginsFormItems.removeIf(formItem -> formItem.getName()
                                                                   .startsWith(infrastructurePluginDescriptor.getPluginName()));

        this.previousSelectedInfrastructure = this.infrastructureSelectItem.getValueAsString();
        this.infrastructureSelectItem.setValue(infrastructurePluginDescriptor.getPluginName());
        List<FormItem> prefilledFormItems = getPrefilledFormItems(infrastructurePluginDescriptor);
        allNodeSourcePluginsFormItems.addAll(findFormItemIndexByName(INFRASTRUCTURE_FORM_KEY,
                                                                     allNodeSourcePluginsFormItems) +
                                             1, prefilledFormItems);
        this.formItemsByName.put(infrastructurePluginDescriptor.getPluginName(), prefilledFormItems);
        resetFormForInfrastructureSelectChange();
    }

    private void validateInfrastructureNameOrFail(String infrastructurePluginName) {
        if (this.controller.getModel()
                           .getSupportedInfrastructures()
                           .values()
                           .stream()
                           .noneMatch(pluginDescriptor -> pluginDescriptor.getPluginName()
                                                                          .equals(infrastructurePluginName))) {
            throw new IllegalArgumentException("Infrastructure " + infrastructurePluginName + " does not exist");
        }
    }

    private int findFormItemIndexByName(String formItemName, List<FormItem> allNodeSourcePluginsFormItems) {
        for (int i = 0; i < allNodeSourcePluginsFormItems.size(); i++) {
            if (allNodeSourcePluginsFormItems.get(i).getName().equals(formItemName)) {
                return i;
            }
        }
        throw new IllegalArgumentException("Form item with name " + formItemName + " does not exist");
    }

    private void handleNodeSourceCreationError(VLayout nodeSourceWindowLayout, JSONObject jsonCallback) {
        String msg;
        if (jsonCallback.get("errorMessage").isString() != null) {
            msg = jsonCallback.get("errorMessage").isString().stringValue();
        } else {
            msg = jsonCallback.toString();
        }
        setNodeSourceWindowLabelWithError("Failed to apply action to Node Source", new IllegalStateException(msg));
        nodeSourceWindowLayout.scrollToTop();
    }

    public void setNormalNodeSourceWindowLabel() {
        this.nodeSourceWindowLabel.setContents(WINDOW_HEADER);
    }

    public void setNodeSourceWindowLabelWithError(String errorMessage, Throwable e) {
        LogModel.getInstance().logImportantMessage(errorMessage + ": " + e.getMessage());
        this.nodeSourceWindowLabel.setContents("<span style='color:red'>" + errorMessage + " :<br>" + e.getMessage() +
                                               "</span>");
    }

    public void show() {
        this.window.show();
    }

    public void destroy() {
        this.window.destroy();
    }

}
