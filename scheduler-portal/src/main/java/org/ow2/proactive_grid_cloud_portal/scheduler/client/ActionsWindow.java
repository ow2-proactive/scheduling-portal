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
package org.ow2.proactive_grid_cloud_portal.scheduler.client;

import static org.ow2.proactive_grid_cloud_portal.common.client.json.JSONUtils.parseJSON;

import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.Map;

import org.ow2.proactive_grid_cloud_portal.common.client.Images;
import org.ow2.proactive_grid_cloud_portal.common.client.json.JSONException;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LogModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.ActionsController;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.JobVariable;

import com.google.gwt.i18n.client.HasDirection;
import com.google.gwt.json.client.JSONBoolean;
import com.google.gwt.json.client.JSONObject;
import com.google.gwt.json.client.JSONString;
import com.google.gwt.json.client.JSONValue;
import com.google.gwt.user.client.ui.CheckBox;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.BlurbItem;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;


/**
 * Popup Window for sending signal with variables
 *
 */
public class ActionsWindow {

    private static final int WINDOW_WIDTH = 790;

    private final ActionsController actionsController;

    private final String signalName;

    private final Map<String, Map<String, String>> signalVariables;

    private final String jobId;

    private Window window;

    private boolean isAdvanced;

    private Map<String, JobVariable> variables;

    private int noOfFields;

    private VLayout varsLayout;

    private FormItem[] fields;

    public ActionsWindow(String signalName, Map<String, Map<String, String>> signalVariables, String jobId) {
        this.signalName = signalName;
        this.signalVariables = signalVariables;
        this.jobId = jobId;
        actionsController = new ActionsController(signalName, jobId);
        actionsController.setActionsWindow(this);
        readVariables();
        this.build();
    }

    private void readVariables() {
        variables = new LinkedHashMap<>();
        signalVariables.keySet().forEach(variableName -> {
            String name = signalVariables.get(variableName).get("name");
            String value = signalVariables.get(variableName).get("value");
            String model = signalVariables.get(variableName).get("model");
            String description = signalVariables.get(variableName).get("description");
            String group = signalVariables.get(variableName).get("group");
            boolean advanced = Boolean.parseBoolean(signalVariables.get(variableName).get("advanced"));
            boolean hidden = Boolean.parseBoolean(signalVariables.get(variableName).get("hidden"));
            variables.put(variableName, new JobVariable(name, value, model, description, group, advanced, hidden));
        });
    }

    public void show() {
        this.window.show();
    }

    public void destroy() {
        this.window.destroy();
    }

    private void build() {

        final IButton checkButton = new IButton("Check");
        checkButton.setIcon(Images.instance.ok_16().getSafeUri().asString());
        checkButton.addClickHandler(event -> actionsController.validateJobSignal(getUpdatedVariables(), false));
        final IButton confirmButton = new IButton("Confirm");
        confirmButton.setIcon(Images.instance.ok_16().getSafeUri().asString());
        confirmButton.addClickHandler(event -> actionsController.validateJobSignal(getUpdatedVariables(), true));
        final IButton cancelButton = new IButton("Cancel");
        cancelButton.setIcon(Images.instance.cancel_16().getSafeUri().asString());
        cancelButton.addClickHandler(event -> hide());

        HLayout buttonsLayout = new HLayout();
        buttonsLayout.setAlign(Alignment.RIGHT);
        buttonsLayout.setHeight(50);
        buttonsLayout.setHeight(checkButton.getHeight());
        buttonsLayout.setWidth100();
        buttonsLayout.setMembersMargin(5);
        buttonsLayout.setMargin(15);
        buttonsLayout.setMembers(cancelButton, checkButton, confirmButton);

        varsLayout = new VLayout();
        varsLayout.setGroupTitle("You are about to send signal " + signalName + " to job " + jobId);
        varsLayout.setIsGroup(true);
        varsLayout.setWidth100();
        varsLayout.setMargin(5);
        setVarsLayout();

        VLayout messageLayout = new VLayout();
        messageLayout.setIsGroup(true);
        messageLayout.setGroupTitle("Messages");
        messageLayout.setMargin(5);
        messageLayout.setWidth100();
        messageLayout.setHeight(65);
        Arrays.stream(messageLayout.getMembers()).forEach(messageLayout::removeMember);
        actionsController.setMessageLayout(messageLayout);

        VLayout rootPage = new VLayout();
        rootPage.setMargin(5);
        rootPage.setWidth100();
        rootPage.setHeight100();
        rootPage.setMembers(varsLayout, messageLayout, buttonsLayout);

        this.window = new Window();
        this.window.setTitle("Actions");
        this.window.setShowMinimizeButton(false);
        this.window.setIsModal(true);
        this.window.setShowModalMask(true);
        this.window.addItem(rootPage);
        this.window.setWidth(WINDOW_WIDTH);
        this.window.setHeight(getHeightByNumberOfVars() + 130);
        this.window.setMargin(25);
        this.window.setAutoSize(true);
        this.window.setCanDragResize(true);
        this.window.setIsModal(true);
        this.window.setShowModalMask(true);
        this.window.centerInPage();
    }

    public void hide() {
        window.hide();
    }

    private Map<String, String> getUpdatedVariables() {
        Map<String, String> updatedVariables = new LinkedHashMap<>();
        if (fields != null) {
            Arrays.stream(fields).forEach(field -> {
                if (variables.containsKey(field.getName())) {
                    updatedVariables.put(field.getName(), field.getValue().toString());
                }
            });
        } else {
            variables.keySet().forEach(variable -> updatedVariables.put(variables.get(variable).getName(),
                                                                        variables.get(variable).getValue()));
        }
        return updatedVariables;
    }

    private Layout createCheckBoxLayout() {
        Layout checkBoxLayout = new Layout();
        CheckBox advancedCheckBox = new CheckBox("Advanced", HasDirection.Direction.RTL);
        advancedCheckBox.setValue(isAdvanced);
        advancedCheckBox.addClickHandler(event -> {
            isAdvanced = advancedCheckBox.getValue();
            refreshVariables();
            setVarsLayout();
        });
        checkBoxLayout.setMargin(10);
        checkBoxLayout.addMember(advancedCheckBox);
        checkBoxLayout.setAlign(Alignment.RIGHT);
        return checkBoxLayout;
    }

    private void refreshVariables() {
        if (fields != null) {
            Arrays.stream(fields).forEach(field -> {
                if (variables.containsKey(field.getName())) {
                    variables.get(field.getName()).setValue(field.getValue().toString());
                }
            });
        }
    }

    public void setVarsLayout() {
        Arrays.stream(varsLayout.getMembers()).forEach(member -> varsLayout.removeMember(member));
        long noOfAdvancedVariables = variables.values().stream().filter(JobVariable::isAdvanced).count();
        if (noOfAdvancedVariables > 0) {
            Layout checkBoxLayout = createCheckBoxLayout();
            varsLayout.addMember(checkBoxLayout);
        }
        Map<String, Map<String, JobVariable>> variablesByGroup = getVariablesByGroup();
        if (!variablesByGroup.isEmpty()) {
            DynamicForm variablesVisualForm = new DynamicForm();
            variablesVisualForm.setNumCols(3);
            variablesVisualForm.setColWidths("25%", "50%", "25%");
            variablesVisualForm.setMargin(10);
            variablesVisualForm.setHeight(getHeightByNumberOfVars());
            variablesVisualForm.setOverflow(Overflow.AUTO);
            variablesVisualForm.setAlign(Alignment.CENTER);
            fields = new FormItem[variablesByGroup.size() * 2 + 1];
            noOfFields = 0;
            variablesByGroup.keySet().forEach(group -> initVariablesVisualForm(variablesByGroup.get(group), group));
            variablesVisualForm.setFields(fields);
            varsLayout.addMember(variablesVisualForm);
        }
        varsLayout.reflow();
    }

    private int getHeightByNumberOfVars() {
        return 40 + (30 * (int) variables.values()
                                         .stream()
                                         .filter(jobVariable -> !jobVariable.isHidden() &&
                                                                (isAdvanced || !jobVariable.isAdvanced()))
                                         .count());
    }

    private Map<String, Map<String, JobVariable>> getVariablesByGroup() {
        Map<String, Map<String, JobVariable>> finalVariablesByGroup = new LinkedHashMap<>();
        Map<String, Map<String, JobVariable>> variablesByGroup = new LinkedHashMap<>();
        Map<String, JobVariable> mainVariables = new LinkedHashMap<>();
        variables.forEach((key, value) -> {
            if (!value.isHidden() && (isAdvanced || !value.isAdvanced())) {
                String group = value.getGroup();
                if (group == null || group.isEmpty()) {
                    mainVariables.put(key, value);
                } else {
                    Map<String, JobVariable> variablesSameGroup = variablesByGroup.containsKey(group) ? variablesByGroup.get(group)
                                                                                                      : new LinkedHashMap<>();
                    variablesSameGroup.put(key, value);
                    variablesByGroup.put(group, variablesSameGroup);
                }
            }
        });
        if (!mainVariables.isEmpty()) {
            finalVariablesByGroup.put("Main Variables", mainVariables);
        }
        finalVariablesByGroup.putAll(variablesByGroup);
        return finalVariablesByGroup;
    }

    private void initVariablesVisualForm(Map<String, JobVariable> variablesByGroup, String group) {
        BlurbItem groupLabel = new BlurbItem();
        groupLabel.setDefaultValue("<u><b>" + group + "</b></u>");
        groupLabel.setStartRow(false);
        groupLabel.setEndRow(true);
        fields[noOfFields++] = groupLabel;
        for (Map.Entry<String, JobVariable> var : variablesByGroup.entrySet()) {
            TextItem variableItem = createVariableItem(var.getKey(), var.getValue().getValue());
            if (var.getValue().getDescription() != null && !var.getValue().getDescription().isEmpty()) {
                variableItem.setTooltip("<div class='tooltipStyle'>" + var.getValue().getDescription() + "</div>");
            }
            fields[noOfFields++] = variableItem;
            String model = var.getValue().getModel();
            BlurbItem modelItem = createModelItem(model);
            fields[noOfFields++] = modelItem;
        }
    }

    private TextItem createVariableItem(String name, String value) {
        TextItem t = new TextItem(name, name);
        t.setValue(value.replaceAll("ENC((.*))", "*******"));
        t.setWidth("100%");
        t.setStartRow(true);
        t.setEndRow(false);
        return t;
    }

    private BlurbItem createModelItem(String model) {
        BlurbItem modelLabel;
        if (model != null) {
            modelLabel = new BlurbItem();
            modelLabel.setDefaultValue(model);
        } else {
            modelLabel = new BlurbItem();
        }
        modelLabel.setStartRow(false);
        modelLabel.setEndRow(true);
        return modelLabel;
    }

    public void updateVariables(String jsonResult) {
        try {
            JSONObject updatedVariablesJson = parseJSON(jsonResult).isObject();
            if (updatedVariablesJson == null) {
                throw new JSONException("Expected JSON Object: " + jsonResult);
            }
            JSONObject obj = updatedVariablesJson.get("updatedVariables").isObject();
            if (obj != null) {
                for (String varName : obj.keySet()) {
                    JobVariable variable = variables.get(varName);
                    if (variable != null) {
                        JSONValue variableJsonValue = obj.get(varName);
                        JSONString variableJsonString = variableJsonValue.isString();
                        if (variableJsonString != null && !matchesCopyPattern(variable.getName())) {
                            variable.setValue(variableJsonString.stringValue());
                        }
                        updateAdvanced(updatedVariablesJson, varName, variable);
                        updateHidden(updatedVariablesJson, varName, variable);
                        updateGroup(updatedVariablesJson, varName, variable);
                        updateModel(updatedVariablesJson, varName, variable);
                        updateDescription(updatedVariablesJson, varName, variable);
                    }
                }
            }
        } catch (JSONException e) {
            LogModel.getInstance().logImportantMessage("Failed to parse JSON for updating signal variables");
        }
    }

    private boolean matchesCopyPattern(String variableName) {
        FormItem matchField = Arrays.stream(fields)
                                    .filter(field -> field.getName().equals(variableName))
                                    .findFirst()
                                    .orElse(null);
        if (matchField != null && matchField.getValue() != null) {
            String matchVariableName = extractVariableName(matchField.getValue().toString());
            return matchVariableName != null;
        }
        return false;
    }

    private String extractVariableName(String variablePatternCheck) {
        return variables.keySet()
                        .stream()
                        .filter(variableName -> variablePatternCheck.contains("$" + variableName) ||
                                                variablePatternCheck.contains("${" + variableName + "}"))
                        .findFirst()
                        .orElse(null);
    }

    private void updateDescription(JSONObject updatedVariablesJson, String varName, JobVariable variable) {
        JSONObject objDescriptionVariables = updatedVariablesJson.get("updatedDescriptions").isObject();
        JSONString variableDescriptionGroups = objDescriptionVariables.get(varName).isString();
        if (variableDescriptionGroups != null) {
            variable.setDescription(variableDescriptionGroups.stringValue());
        }
    }

    private void updateModel(JSONObject updatedVariablesJson, String varName, JobVariable variable) {
        JSONObject objModelVariables = updatedVariablesJson.get("updatedModels").isObject();
        JSONString variableJsonModel = objModelVariables.get(varName).isString();
        if (variableJsonModel != null) {
            variable.setModel(variableJsonModel.stringValue());
        }
    }

    private void updateGroup(JSONObject updatedVariablesJson, String varName, JobVariable variable) {
        JSONObject objGroupsVariables = updatedVariablesJson.get("updatedGroups").isObject();
        JSONString variableJsonGroups = objGroupsVariables.get(varName).isString();
        if (variableJsonGroups != null) {
            variable.setGroup(variableJsonGroups.stringValue());
        }
    }

    private void updateHidden(JSONObject updatedVariablesJson, String varName, JobVariable variable) {
        JSONObject objHiddenVariables = updatedVariablesJson.get("updatedHidden").isObject();
        JSONBoolean variableJsonHidden = objHiddenVariables.get(varName).isBoolean();
        if (variableJsonHidden != null) {
            variable.setHidden(variableJsonHidden.booleanValue());
        }
    }

    private void updateAdvanced(JSONObject updatedVariablesJson, String varName, JobVariable variable) {
        JSONObject objAdvancedVariables = updatedVariablesJson.get("updatedAdvanced").isObject();
        JSONBoolean variableJsonAdvanced = objAdvancedVariables.get(varName).isBoolean();
        if (variableJsonAdvanced != null) {
            variable.setAdvanced(variableJsonAdvanced.booleanValue());
        }
    }

    public FormItem[] getFields() {
        return fields;
    }
}
