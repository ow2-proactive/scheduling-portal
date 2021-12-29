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

import java.util.Arrays;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;

import org.ow2.proactive_grid_cloud_portal.common.client.Controller;
import org.ow2.proactive_grid_cloud_portal.common.client.Images;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LogModel;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LoginModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.JobsController;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.JobVariable;
import org.ow2.proactive_grid_cloud_portal.scheduler.server.SubmitEditServlet;

import com.google.gwt.core.client.GWT;
import com.google.gwt.core.client.JavaScriptException;
import com.google.gwt.event.dom.client.ChangeEvent;
import com.google.gwt.event.dom.client.ChangeHandler;
import com.google.gwt.event.shared.HandlerRegistration;
import com.google.gwt.http.client.Request;
import com.google.gwt.http.client.RequestBuilder;
import com.google.gwt.http.client.RequestCallback;
import com.google.gwt.http.client.RequestException;
import com.google.gwt.http.client.Response;
import com.google.gwt.i18n.client.DateTimeFormat;
import com.google.gwt.i18n.client.HasDirection;
import com.google.gwt.json.client.JSONArray;
import com.google.gwt.json.client.JSONBoolean;
import com.google.gwt.json.client.JSONException;
import com.google.gwt.json.client.JSONObject;
import com.google.gwt.json.client.JSONParser;
import com.google.gwt.json.client.JSONString;
import com.google.gwt.json.client.JSONValue;
import com.google.gwt.regexp.shared.MatchResult;
import com.google.gwt.regexp.shared.RegExp;
import com.google.gwt.user.client.ui.Anchor;
import com.google.gwt.user.client.ui.Button;
import com.google.gwt.user.client.ui.CheckBox;
import com.google.gwt.user.client.ui.FileUpload;
import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.FormPanel;
import com.google.gwt.user.client.ui.FormPanel.SubmitCompleteEvent;
import com.google.gwt.user.client.ui.FormPanel.SubmitCompleteHandler;
import com.google.gwt.user.client.ui.HTML;
import com.google.gwt.user.client.ui.HasHorizontalAlignment;
import com.google.gwt.user.client.ui.Hidden;
import com.google.gwt.user.client.ui.HorizontalPanel;
import com.google.gwt.user.client.ui.InlineLabel;
import com.google.gwt.user.client.ui.ListBox;
import com.google.gwt.user.client.ui.RadioButton;
import com.google.gwt.user.client.ui.VerticalPanel;
import com.google.gwt.user.client.ui.Widget;
import com.google.gwt.xml.client.Document;
import com.google.gwt.xml.client.NamedNodeMap;
import com.google.gwt.xml.client.Node;
import com.google.gwt.xml.client.NodeList;
import com.google.gwt.xml.client.XMLParser;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.util.DateUtil;
import com.smartgwt.client.widgets.DateChooser;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.BlurbItem;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.TimeItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;


/**
 * Popup Window for job submission
 *
 * @author mschnoor
 */
public class SubmitWindow {

    private static final String KEY_OF_MODEL = "model";

    private static final String KEY_OF_NAME = "name";

    private static final String KEY_OF_VALUE = "value";

    private static final String KEY_OF_GROUP = "group";

    private static final String KEY_OF_DESCRIPTION = "description";

    private static final String KEY_OF_HIDDEN = "hidden";

    private static final String KEY_OF_ADVANCED = "advanced";

    private static final String CATALOG_SELECT_BUCKET = "Select a Bucket";

    private static final String CATALOG_SELECT_WF = "Select a Workflow";

    private static final String ISO_8601_FORMAT = "yyyy-MM-dd'T'HH:mm:ssZZZ";

    private static final String URL_SUBMIT_XML = GWT.getModuleBaseURL() + "submitedit";

    private static final String URL_UPLOAD_FILE = GWT.getModuleBaseURL() + "uploader";

    private static final String METHOD_INSTRUCTION = "Select a method";

    private static final String METHOD_FROM_FILE = "import from file";

    private static final String METHOD_FROM_CATALOG = "import from Catalog";

    private static final String SESSION_ID_PARAMETER_NAME = "sessionId";

    private static final String JOB_ID_PARAMETER_NAME = "jobId";

    private static final String ERROR_MESSAGE = "errorMessage";

    private static final String JSON_ERROR = "JSON parse ERROR";

    private static final String ERROR_MESSAGE_REGEX = "\"errorMessage\":\"(.*)\",\"stackTrace\"";

    private static final int WINDOW_WIDTH = 800;

    private static final int WINDOW_HEIGHT = 700;

    private Window window;

    private HandlerRegistration todayClickHR = null;

    private HandlerRegistration changedHourHR = null;

    private HandlerRegistration changedMinuteHR = null;

    private HandlerRegistration gridClickHR = null;

    private VLayout rootPage; // ------------------------ the main layout of the
    // window

    private VLayout selectWfLayout; // ----------------- Select Workflow Panel

    private ListBox wfMethodsListBox; // ------------- Methods to select a
    // workflow

    private VLayout fromFilePanel; // ------------------- The panel to select wf
    // from disk

    private FileUpload fileUpload; // ------------------- FileUpload button

    private HorizontalPanel selectWorkflowButtonsPanel; // Panel that holds the
    // strategic items to
    // get a wf

    private Button sendFromFileButton; // --------------- Send the file to
    // servlet from disk

    private HorizontalPanel fromCatalogPanel; // -------- The panel to select wf
    // from catalog

    private ListBox bucketsListBox; // ------------------ Buckets dropdown list

    private ListBox workflowsListBox; // ---------------- Workflows dropdown
    // list

    private Button sendFromCatalogButton; // ------------ Send the file to
    // servlet from catalog

    // -------------------------------------------------- Variables Part
    private VLayout varsLayout; // --------------------- Variables Panel

    private VerticalPanel hiddenPane; // ---------------- Holds the parameters
    // to submit along with the job

    private Hidden validate = new Hidden("validate", "true");

    private FormItem[] fields; // ----------------------- (visual) Variables to
    // submit along with the job

    private Hidden[] _fields; // ------------------------ (hidden) Variables to
    // submit along with the job

    private Hidden startAtParameter; // ----------------- START_AT value to send
    // along with the job

    private Hidden planParameter; // ----------------- PLAN value to send
    // along with the job

    private FormPanel variablesActualForm; // ----------- Actual form to send

    // -------------------------------------------------- Start At Part
    private VLayout startAtLayout; // ------------------ Start At Panel

    private VerticalPanel startAtRadioGroupPanel; // ---- Now or Later

    private RadioButton startAccordingPlanningRadioButton; // ------------------ According to the defined EXECUTION_CALENDAR Generic Information of the Job

    private RadioButton startNowRadioButton; // ------------------ As soon as possible
    // radio button

    private RadioButton startAtRadioButton; // ------------------- at scheduled time
    // radio button

    private DateChooser dateChooser; // ----------------- DateChooser

    private HLayout submitCancelButtons; // ---------- -- Cancel and Submit
    // buttons

    private IButton submitButton;

    private IButton checkButton;

    private VLayout messagePanel; // -------------------- Loading Panel when
    // uploading

    private Label waitLabel;

    private Label errorLabel;

    private String CATALOG_URL = null;

    private Map<String, JobVariable> variables;

    private String job;

    /**
     *  JobId of the workflow to be re-submitted
     */
    private String jobId;

    private boolean isResubmit;

    private boolean isKillAndResubmit;

    private JobsController jobsController;

    private String documentation = null;

    private String icon = null;

    private String bucketNameGI = null;

    private Boolean existBucketName = false;

    private Boolean existDocumentation = false;

    private Boolean existIcon = false;

    private Boolean isExecCalendarValueNull = true; // capture if EXECUTION_CALENDAR value is null

    private boolean isAdvanced = false;

    private String currentJobDescriptor;

    private int noOfFields;

    /**
     * Default constructor
     *
     */
    public SubmitWindow() {
        this.isResubmit = false;
        this.isKillAndResubmit = false;
        this.build();
    }

    /**
     * Used for re-submitting a job. Has a slightly different layout
     *
     */
    public SubmitWindow(String jobId) {
        this.jobId = jobId;
        this.isResubmit = true;
        this.build();
    }

    /**
     * Used for kill & re-submit a job action. Has a slightly different layout
     *
     */
    public SubmitWindow(String jobId, JobsController controller) {
        this.jobId = jobId;
        this.isKillAndResubmit = true;
        this.jobsController = controller;
        this.build();
    }

    /**
     * Shows the window created by the constructor, calling this after destroy
     * will throw an NPE, you must create a new SubmitWindow for each job
     * submission
     */
    public void show() {
        this.window.show();
    }

    /**
     * Destroy the window, you may null the reference after this as it will not
     * be usable again
     */
    public void destroy() {
        this.window.destroy();
    }

    /**
     * Builds the catalog URL. If none is configured in the settings file, sets
     * the URL to the bundled Catalog
     *
     */
    private void buildCatalogUrl() {
        CATALOG_URL = new CatalogUrlSchedulerClientBuilder().getCatalogUrl();
    }

    private void initRootPage() {
        rootPage = new VLayout();
        rootPage.setMargin(5);
        rootPage.setWidth100();
        rootPage.setHeight100();
    }

    private void initSelectWfPart() {
        selectWfLayout = new VLayout();
        selectWfLayout.setGroupTitle("1. Select workflow");
        selectWfLayout.setIsGroup(true);
        selectWfLayout.setHeight("70px");

        // This panel changes depending on the selected method of getting a
        // workflow
        selectWorkflowButtonsPanel = new HorizontalPanel();
        selectWorkflowButtonsPanel.setHorizontalAlignment(HasHorizontalAlignment.ALIGN_CENTER);
        selectWorkflowButtonsPanel.setHeight("50px");
        selectWorkflowButtonsPanel.setSpacing(5);

        final VerticalPanel getWfMethodsPanel = new VerticalPanel();
        getWfMethodsPanel.setSpacing(10);
        getWfMethodsPanel.setHeight("30px");
        wfMethodsListBox = new ListBox();
        wfMethodsListBox.addItem(METHOD_INSTRUCTION);
        wfMethodsListBox.addItem(METHOD_FROM_FILE);
        wfMethodsListBox.addItem(METHOD_FROM_CATALOG);
        getWfMethodsPanel.add(wfMethodsListBox);

        wfMethodsListBox.addChangeHandler(new ChangeHandler() {
            @Override
            public void onChange(ChangeEvent event) {
                String selectedMethod = wfMethodsListBox.getSelectedValue();
                if (METHOD_FROM_FILE.compareTo(selectedMethod) == 0) {
                    clearPanel();
                    initSelectWorkflowFromFilePanel();
                    selectWorkflowButtonsPanel.add(fromFilePanel);
                    selectWorkflowButtonsPanel.add(sendFromFileButton);
                    initFooter();
                } else if (METHOD_FROM_CATALOG.compareTo(selectedMethod) == 0) {
                    clearPanel();
                    initSelectWorkflowFromCatalogPanel();
                    selectWorkflowButtonsPanel.add(fromCatalogPanel);
                    selectWorkflowButtonsPanel.add(sendFromCatalogButton);
                    initFooter();
                } else {
                    // default case: METHOD_INSTRUCTION
                    varsLayout.removeMembers(varsLayout.getMembers());
                    selectWorkflowButtonsPanel.clear();
                    initFooter();
                }
            }

            private void clearPanel() {
                varsLayout.removeMembers(varsLayout.getMembers());
                selectWorkflowButtonsPanel.clear();
                initSelectWorkflowFromFilePanel();
            }

            private void initFooter() {
                rootPage.removeMember(startAtLayout);
                rootPage.removeMember(messagePanel);
                rootPage.removeMember(submitCancelButtons);
                initSubmitAtPart();
                rootPage.addMember(messagePanel);
                rootPage.addMember(submitCancelButtons);
                startAccordingPlanningRadioButton.setValue(false);
                startNowRadioButton.setValue(false);
                startAtRadioButton.setValue(false);
                setEnabledStartAtPart(false);
            }
        });

        // init both panels (select from disk or from catalog)
        initSelectWorkflowFromFilePanel();
        initSelectWorkflowFromCatalogPanel();

        selectWfLayout.addMember(getWfMethodsPanel);
        selectWfLayout.addMember(selectWorkflowButtonsPanel);

        rootPage.addMember(selectWfLayout);
    }

    private void fillVarsPart(String jobDescriptor) {
        initVarsLayout();
        getGenericInformationAttributes(jobDescriptor);
        Widget workflowMetaDataWidget = prepareWorlflowInformationWidget();
        varsLayout.addMember(workflowMetaDataWidget);
        if (variables != null && !variables.isEmpty()) {
            long noOfAdvancedVariables = variables.values()
                                                  .stream()
                                                  .filter(jobVariable -> jobVariable.isAdvanced())
                                                  .count();
            if (noOfAdvancedVariables > 0) {
                Layout checkBoxLayout = createCheckBoxLayout();
                varsLayout.addMember(checkBoxLayout);
            }
            setVarsLayout();
        }
        Layout hiddenVarsLayout = initVariablesActualForm();
        varsLayout.addMember(hiddenVarsLayout);
        rootPage.addMember(varsLayout);
        rootPage.reflow();
    }

    private Layout createCheckBoxLayout() {
        Layout checkBoxLayout = new Layout();
        CheckBox advancedCheckBox = new CheckBox("Advanced", HasDirection.Direction.RTL);
        advancedCheckBox.setValue(isAdvanced);
        advancedCheckBox.addClickHandler(event -> {
            isAdvanced = advancedCheckBox.getValue();
            redrawVariables(currentJobDescriptor);
        });
        checkBoxLayout.addMember(advancedCheckBox);
        checkBoxLayout.setAlign(Alignment.RIGHT);
        return checkBoxLayout;
    }

    private void setVarsLayout() {
        Map<String, Map<String, JobVariable>> variablesByGroup = getVariablesByGroup();

        DynamicForm variablesVisualForm;
        variablesVisualForm = new DynamicForm();
        variablesVisualForm.setNumCols(3);
        variablesVisualForm.setColWidths("25%", "50%", "25%");
        fields = new FormItem[variablesByGroup.size() * 2 + 1];
        noOfFields = 0;
        noOfFields = 0;
        variablesByGroup.keySet().forEach(group -> initVariablesVisualForm(variablesByGroup.get(group), group));
        variablesVisualForm.setFields(fields);
        varsLayout.addMember(variablesVisualForm);
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

    private Widget prepareWorlflowInformationWidget() {

        String widgetIcon = "No icone <br>";
        String widgetDocumentation = "No documentation <br>";
        String widgetBucketName = "No bucket name";

        if (icon != null) {
            widgetIcon = "<img src=" + icon + " height=40px width=40px> <br>";
        }

        if (documentation != null) {
            widgetDocumentation = "<b>Documentation :</b> <a href=/doc/" + documentation + " target=_blank>" +
                                  documentation + " </a><br>";
        }

        if (bucketNameGI != null) {
            widgetBucketName = "<b> Bucket name :</b> " + bucketNameGI;
        }

        return new HTML("<center> " + widgetIcon + widgetDocumentation + widgetBucketName + " </center>");

    }

    private void getGenericInformationAttributes(String jobDescriptor) {
        Document dom = XMLParser.parse(jobDescriptor);
        NodeList genericInfo = dom.getElementsByTagName("genericInformation");
        // check if the job has genericInformation or not
        if (genericInfo != null && genericInfo.getLength() > 0) {
            // get the first item
            Node root = genericInfo.item(0);
            NodeList genericInfoMetaDataList = root.getChildNodes();
            for (int i = 0; i < genericInfoMetaDataList.getLength(); i++) {
                Node genericInfoMetadata = genericInfoMetaDataList.item(i);
                if (genericInfoMetadata.getNodeName().equals("info") && genericInfoMetadata.hasAttributes()) {
                    NamedNodeMap attributes = genericInfoMetadata.getAttributes();
                    getGenericInformationName(attributes);
                }
            }
        }
    }

    private void getGenericInformationName(NamedNodeMap attributes) {
        for (int j = 0; j < attributes.getLength(); j++) {
            Node attribute = attributes.item(j);
            if (attribute.getNodeType() == Node.ATTRIBUTE_NODE && attribute.getNodeName().equals(KEY_OF_NAME)) {
                if (attribute.getNodeValue().equalsIgnoreCase("bucketName"))
                    existBucketName = true;
                if (attribute.getNodeValue().equalsIgnoreCase("documentation"))
                    existDocumentation = true;
                if (attribute.getNodeValue().equalsIgnoreCase("workflow.icon"))
                    existIcon = true;
            }
            getGenericInformationValue(attribute);
        }
    }

    private void getGenericInformationValue(Node attribute) {
        if (attribute.getNodeType() == Node.ATTRIBUTE_NODE && attribute.getNodeName().equals(KEY_OF_VALUE)) {
            if (existBucketName) {
                bucketNameGI = attribute.getNodeValue();
                existBucketName = false;
            }
            if (existDocumentation) {
                documentation = attribute.getNodeValue();
                existDocumentation = false;
            }
            if (existIcon) {
                icon = attribute.getNodeValue();
                existIcon = false;
            }
        }
    }

    private void initVariablesVisualForm(Map<String, JobVariable> variablesByGroup, String group) {
        BlurbItem groupLabel = new BlurbItem();
        groupLabel.setDefaultValue("<u><b>" + group + "</b></u>");
        groupLabel.setStartRow(false);
        groupLabel.setEndRow(true);
        fields[noOfFields++] = groupLabel;

        for (Entry<String, JobVariable> var : variablesByGroup.entrySet()) {
            TextItem variableItem = createVariableItem(var);
            if (var.getValue().getDescription() != null && !var.getValue().getDescription().isEmpty()) {
                variableItem.setTooltip("<div class='tooltipStyle'>" + var.getValue().getDescription() + "</div>");
            }
            fields[noOfFields++] = variableItem;
            String model = var.getValue().getModel();
            BlurbItem modelItem = createModelItem(model);
            fields[noOfFields++] = modelItem;
        }
    }

    private Layout initVariablesActualForm() {
        // actual form used to POST
        variablesActualForm = new FormPanel();
        variablesActualForm.setMethod(FormPanel.METHOD_POST);
        variablesActualForm.setAction(URL_SUBMIT_XML);
        hiddenPane = new VerticalPanel();
        _fields = new Hidden[variables.size()];

        int i = 0;
        for (Entry<String, JobVariable> var : variables.entrySet()) {
            _fields[i] = new Hidden("var_" + var.getKey());
            hiddenPane.add(_fields[i]);
            i++;
        }

        hiddenPane.add(new Hidden("job", job));
        hiddenPane.add(new Hidden(SESSION_ID_PARAMETER_NAME, LoginModel.getInstance().getSessionId()));
        hiddenPane.add(validate);
        variablesActualForm.setWidget(hiddenPane);
        Layout fpanelWra = new Layout();
        fpanelWra.addMember(variablesActualForm);
        return fpanelWra;
    }

    private void initVarsLayout() {
        varsLayout = new VLayout();
        varsLayout.setIsGroup(true);
        varsLayout.setGroupTitle((isResubmit || isKillAndResubmit ? 1 : 2) + ". Fill workflow variables");
        varsLayout.setWidth100();
        varsLayout.setMinHeight(150);
        varsLayout.setPadding(5);
        varsLayout.setOverflow(Overflow.AUTO);
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

    private TextItem createVariableItem(Entry<String, JobVariable> var) {
        TextItem t = new TextItem(var.getKey(), var.getKey());
        t.setValue(var.getValue().getValue().replaceAll("ENC((.*))", "*******"));
        t.setWidth("100%");
        t.setStartRow(true);
        t.setEndRow(false);
        return t;
    }

    private void initVarsPart() {
        initVarsLayout();
        rootPage.addMember(varsLayout);
        rootPage.reflow();
    }

    private void setStartAccordingPlanningRadioButtonState(String job) {
        if (job != null && isExecutionCalendarGIDefined(job)) {
            startAccordingPlanningRadioButton.setVisible(true);
            startAccordingPlanningRadioButton.setValue(true);
        } else {
            startAccordingPlanningRadioButton.setVisible(false);
        }
    }

    private void initSubmitAtPart() {
        String startButton = "startAtRadioGroup";
        startAtLayout = new VLayout();
        startAtLayout.setIsGroup(true);
        startAtLayout.setGroupTitle((isResubmit || isKillAndResubmit ? 2 : 3) + ". Scheduled time");
        startAtLayout.setHeight("120px");

        startAtParameter = new Hidden("START_AT");
        planParameter = new Hidden("PLAN");

        startAccordingPlanningRadioButton = new RadioButton(startButton,
                                                            "Planned according to embedded Execution Calendar defintion");
        startNowRadioButton = new RadioButton(startButton, "As soon as possible");
        startAtRadioButton = new RadioButton(startButton, "At");

        startAccordingPlanningRadioButton.setVisible(true);

        startNowRadioButton.setValue(true);
        startAtRadioButton.setValue(false);

        startAccordingPlanningRadioButton.addClickHandler(event -> {
            if (isExecCalendarValueNull) {
                displayErrorMessage("EXECUTION_CALENDAR value is empty.");
            }

            startAtLayout.removeMember(dateChooser);
        });

        startNowRadioButton.addClickHandler(event -> {
            startAtRadioButton.setText("At");
            startAtLayout.removeMember(dateChooser);
        });

        startAtRadioButton.addClickHandler(event -> {
            startAtLayout.addMember(dateChooser);
            DateTimeFormat dateTimeFormat = DateTimeFormat.getFormat(ISO_8601_FORMAT);
            String iso8601DateStr = dateTimeFormat.format(dateChooser.getData());
            startAtParameter.setValue(iso8601DateStr);
            updateScheduledTimeForToday();
            resetAllHandlers();
        });

        dateChooser = new DateChooser();
        dateChooser.setShowTimeItem(true);
        dateChooser.setUse24HourTime(true);
        dateChooser.setShowTodayButton(true);
        dateChooser.setShowApplyButton(false);
        dateChooser.setWidth(WINDOW_WIDTH / 2);
        dateChooser.setLayoutAlign(Alignment.CENTER);
        dateChooser.setMargin(10);

        startAtRadioGroupPanel = new VerticalPanel();
        startAtRadioGroupPanel.setSpacing(10);

        startAtRadioGroupPanel.add(startAccordingPlanningRadioButton);

        startAtRadioGroupPanel.add(startNowRadioButton);
        startAtRadioGroupPanel.add(startAtRadioButton);
        startAtRadioGroupPanel.setHeight("30px");

        startAtLayout.addMember(startAtRadioGroupPanel);

        FlowPanel planJobPanel = new FlowPanel();
        InlineLabel firstPart = new InlineLabel("Or use the ");
        firstPart.getElement().getStyle().setProperty("margin-left", "15px");
        planJobPanel.add(firstPart);
        Anchor jobPlannerPortalLink = new Anchor("Job Planner Portal",
                                                 "/automation-dashboard/#/portal/job-planner-calendar-def-workflows",
                                                 "_blank");
        planJobPanel.add(jobPlannerPortalLink);
        planJobPanel.add(new InlineLabel(" to schedule the workflow execution periodically"));
        planJobPanel.setHeight("20px");
        startAtLayout.addMember(planJobPanel);

        rootPage.addMember(startAtLayout);
    }

    private void updateScheduledTimeAt() {
        TimeItem dateTimeItem = dateChooser.getTimeItem();
        int selectedHour = Integer.parseInt(dateTimeItem.getHourItem().getValueAsString());
        int selectedMinute = Integer.parseInt(dateTimeItem.getMinuteItem().getValueAsString());
        Date newDateTime = DateUtil.createLogicalTime(selectedHour, selectedMinute, 0, 0);
        Date updatedDate = DateUtil.combineLogicalDateAndTime(dateChooser.getData(), newDateTime);
        dateChooser.setData(updatedDate);
        startAtRadioButton.setText("At " + updatedDate.toString());
    }

    private void updateScheduledTimeForToday() {
        Date newDate = new Date();
        dateChooser.setData(newDate);
        dateChooser.getTimeItem().setHours(Integer.parseInt(DateTimeFormat.getFormat("HH").format(newDate)));
        dateChooser.getTimeItem().setMinutes(Integer.parseInt(DateTimeFormat.getFormat("mm").format(newDate)));
        startAtRadioButton.setText("At " + newDate.toString());
    }

    private void initMessagesPart() {
        messagePanel = new VLayout();
        messagePanel.setIsGroup(true);
        messagePanel.setGroupTitle("Messages");
        messagePanel.setHeight("90px");
        rootPage.addMember(messagePanel);
    }

    private void initButtonsPart() {
        submitCancelButtons = new HLayout();
        submitCancelButtons.setMargin(10);
        submitCancelButtons.setMembersMargin(5);
        submitCancelButtons.setHeight(30);
        submitCancelButtons.setWidth100();
        submitCancelButtons.setAlign(Alignment.RIGHT);

        if (isResubmit) {
            submitButton = new IButton("Re-Submit");
            submitButton.setIcon(SchedulerImages.instance.job_resubmit_22().getSafeUri().asString());
        } else if (isKillAndResubmit) {
            submitButton = new IButton("Kill & Re-Submit");
            submitButton.setWidth(120);
            submitButton.setIcon(SchedulerImages.instance.job_kill_resubmit_22().getSafeUri().asString());
        } else {
            submitButton = new IButton("Submit");
            submitButton.setIcon(Images.instance.ok_16().getSafeUri().asString());
        }
        submitButton.setShowDisabledIcon(false);
        submitButton.setTooltip("A workflow must be selected first");
        submitButton.addClickHandler(clickHandlerForSubmitButton());

        checkButton = new IButton("Check");
        checkButton.setIcon(Images.instance.ok_16().getSafeUri().asString());
        checkButton.setShowDisabledIcon(false);
        checkButton.setTooltip("Validate current workflow and variables");
        checkButton.addClickHandler(clickHandlerForCheckButton());

        final IButton cancelButton = new IButton("Cancel");
        cancelButton.setShowDisabledIcon(false);
        cancelButton.setIcon(Images.instance.cancel_16().getSafeUri().asString());
        cancelButton.addClickHandler(event -> {
            SubmitWindow.this.window.hide();
            SubmitWindow.this.destroy();
        });
        submitCancelButtons.setMembers(cancelButton, checkButton, submitButton);
        rootPage.addMember(submitCancelButtons);
    }

    private void clearMessagePanel() {
        if (messagePanel.contains(waitLabel)) {
            messagePanel.removeMember(waitLabel);
        }
        if (messagePanel.contains(errorLabel)) {
            messagePanel.removeMember(errorLabel);
        }
    }

    private void displayLoadingMessage() {

        clearMessagePanel();

        waitLabel = new Label("Please wait...");
        waitLabel.setHeight(30);
        waitLabel.setIcon("loading.gif");
        waitLabel.setWidth100();
        waitLabel.setMargin(10);
        waitLabel.setAlign(Alignment.CENTER);

        messagePanel.addMember(waitLabel);

        rootPage.reflow();
    }

    private void displayInfoMessage(String message) {
        clearMessagePanel();

        errorLabel = new Label(message);
        errorLabel.setHeight(30);
        errorLabel.setWidth100();
        errorLabel.setMargin(10);
        errorLabel.setAlign(Alignment.CENTER);
        errorLabel.setStyleName("infoMessage");

        messagePanel.addMember(errorLabel);

        rootPage.reflow();
    }

    private void displayErrorMessage(String message) {

        clearMessagePanel();

        errorLabel = new Label(message);
        errorLabel.setHeight(30);
        errorLabel.setWidth100();
        errorLabel.setMargin(10);
        errorLabel.setAlign(Alignment.CENTER);
        errorLabel.setStyleName(ERROR_MESSAGE);

        messagePanel.addMember(errorLabel);

        rootPage.reflow();
    }

    private void initSelectWorkflowFromFilePanel() {
        fromFilePanel = new VLayout();
        fromFilePanel.setHeight("30px");

        fileUpload = new FileUpload();
        fileUpload.setName("job");

        VerticalPanel formContent = new VerticalPanel();
        formContent.setHeight("30px");

        Hidden hiddenField = new Hidden();
        hiddenField.setName(SESSION_ID_PARAMETER_NAME);
        hiddenField.setValue(LoginModel.getInstance().getSessionId());
        formContent.add(hiddenField);
        formContent.add(fileUpload);

        FormPanel importFromFileformPanel = new FormPanel();
        importFromFileformPanel.setEncoding(FormPanel.ENCODING_MULTIPART);
        importFromFileformPanel.setMethod(FormPanel.METHOD_POST);
        importFromFileformPanel.setAction(URL_UPLOAD_FILE);
        importFromFileformPanel.add(formContent);
        importFromFileformPanel.addSubmitCompleteHandler(fileUploadCompleteHandler());
        importFromFileformPanel.setHeight("30px");
        fromFilePanel.addMember(importFromFileformPanel);

        sendFromFileButton = new Button("Upload file");
        sendFromFileButton.addClickHandler(clickHandlerForUploadFromFileButton(importFromFileformPanel));

    }

    private void initSelectWorkflowFromCatalogPanel() {
        fromCatalogPanel = new HorizontalPanel();
        fromCatalogPanel.setHeight("30px");
        fromCatalogPanel.setWidth("100%");
        fromCatalogPanel.setSpacing(2);
        fromCatalogPanel.setHorizontalAlignment(HasHorizontalAlignment.ALIGN_CENTER);

        bucketsListBox = new ListBox();
        workflowsListBox = new ListBox();

        bucketsListBox.setEnabled(false);
        bucketsListBox.addItem(CATALOG_SELECT_BUCKET);

        workflowsListBox.setEnabled(false);
        workflowsListBox.addItem(CATALOG_SELECT_WF);

        bucketsListBox.addChangeHandler(event -> {
            String selectedBucket = bucketsListBox.getSelectedValue();
            if (!CATALOG_SELECT_BUCKET.equals(selectedBucket)) {
                String workflowUrl = CATALOG_URL + "/buckets/" + selectedBucket + "/resources?kind=workflow";
                RequestBuilder req = new RequestBuilder(RequestBuilder.GET, workflowUrl);
                req.setHeader(SESSION_ID_PARAMETER_NAME, LoginModel.getInstance().getSessionId());
                req.setCallback(new RequestCallback() {
                    @Override
                    public void onResponseReceived(Request request, Response response) {
                        JSONArray workflows = JSONParser.parseStrict(response.getText()).isArray();
                        int workflowsSize = workflows.size();
                        workflowsListBox.setEnabled(false);
                        workflowsListBox.clear();
                        workflowsListBox.addItem(CATALOG_SELECT_WF);
                        for (int i = 0; i < workflowsSize; i++) {
                            JSONObject workflow = workflows.get(i).isObject();
                            String workflowName = workflow.get(KEY_OF_NAME).isString().stringValue();
                            workflowsListBox.addItem(workflowName);
                        }
                        workflowsListBox.setEnabled(true);
                    }

                    @Override
                    public void onError(Request request, Throwable exception) {
                        LogModel.getInstance().logImportantMessage("OOPS:" + request.toString());
                    }
                });
                try {
                    req.send();
                } catch (RequestException e) {
                    LogModel.getInstance().logImportantMessage("Error occured when fetching workflows from Catalog");
                    // For Dev debug mode only
                    e.printStackTrace();
                }
            }
        });

        RequestBuilder req = new RequestBuilder(RequestBuilder.GET, CATALOG_URL + "/buckets?kind=workflow");
        req.setHeader(SESSION_ID_PARAMETER_NAME, LoginModel.getInstance().getSessionId());
        req.setCallback(new RequestCallback() {
            @Override
            public void onResponseReceived(Request request, Response response) {
                JSONArray buckets = JSONParser.parseStrict(response.getText()).isArray();
                int bucketSize = buckets.size();
                for (int i = 0; i < bucketSize; i++) {
                    JSONObject bucket = buckets.get(i).isObject();
                    String bucketName = bucket.get(KEY_OF_NAME).isString().stringValue();
                    bucketsListBox.addItem(bucketName);
                }
                bucketsListBox.setEnabled(true);
            }

            @Override
            public void onError(Request request, Throwable exception) {
                LogModel.getInstance().logImportantMessage("Error occured when fetching buckets from Catalog");
            }
        });

        try {
            req.send();
        } catch (RequestException e) {
            LogModel.getInstance().logImportantMessage("Error occured when fetching buckets from Catalog");
            // For Dev debug mode only
            e.printStackTrace();
        }

        fromCatalogPanel.add(bucketsListBox);
        fromCatalogPanel.add(workflowsListBox);

        bucketsListBox.setWidth("130px");
        workflowsListBox.setWidth("230px");

        VerticalPanel formContent = new VerticalPanel();
        formContent.setHeight("30px");
        formContent.add(new Hidden(SESSION_ID_PARAMETER_NAME, LoginModel.getInstance().getSessionId()));

        FormPanel importFromCatalogformPanel = new FormPanel();
        importFromCatalogformPanel.setEncoding(FormPanel.ENCODING_MULTIPART);
        importFromCatalogformPanel.setMethod(FormPanel.METHOD_POST);
        importFromCatalogformPanel.setAction(URL_UPLOAD_FILE);
        importFromCatalogformPanel.add(formContent);
        importFromCatalogformPanel.addSubmitCompleteHandler(fileUploadCompleteHandler());
        importFromCatalogformPanel.setHeight("30px");
        fromCatalogPanel.add(importFromCatalogformPanel);

        sendFromCatalogButton = new Button("Import workflow");
        sendFromCatalogButton.addClickHandler(clickHandlerForUploadFromCatalogButton(formContent,
                                                                                     importFromCatalogformPanel));
    }

    @SuppressWarnings("squid:S3776")
    private ClickHandler clickHandlerForCheckButton() {
        return new ClickHandler() {
            @Override
            public void onClick(ClickEvent event) {
                setAllValuesAsString();
                handleStartAt();
                enableValidation(true);
                handleCompletedSubmission();
                variablesActualForm.submit();
                displayLoadingMessage();

            }

            private void handleCompletedSubmission() {
                variablesActualForm.addSubmitCompleteHandler(new SubmitCompleteHandler() {
                    @Override
                    public void onSubmitComplete(SubmitCompleteEvent event) {
                        String result = event.getResults();
                        if (isResultNull(result) || ifResultIsAnError(result)) {
                            return;
                        }

                        try {
                            JSONValue json = JSONParser.parseStrict(result);
                            JSONObject obj = json.isObject();
                            handleVariables(obj);
                        } catch (Exception e) {
                            handleParsingError(result);
                        }
                    }

                    private void handleParsingError(String result) {
                        // JSON parsing error workaround to force extract error message
                        MatchResult errorMessageMatcher = RegExp.compile(ERROR_MESSAGE_REGEX).exec(result);
                        if (errorMessageMatcher != null) {
                            LogModel.getInstance().logImportantMessage(errorMessageMatcher.getGroup(1));
                            displayErrorMessage(errorMessageMatcher.getGroup(1));
                        } else {
                            LogModel.getInstance().logImportantMessage(JSON_ERROR);
                            displayErrorMessage(JSON_ERROR);
                        }
                    }

                    private void handleVariables(JSONObject obj) {

                        if (obj != null && obj.containsKey("valid")) {
                            if (((JSONBoolean) obj.get("valid")).booleanValue()) {
                                if (obj.containsKey("updatedVariables")) {
                                    if (variables != null && !variables.isEmpty()) {
                                        updateVariables(obj);
                                    }
                                    redrawVariables(job);
                                }
                                LogModel.getInstance().logMessage("Job validated");
                                displayInfoMessage("Job is valid");
                            } else if (obj.containsKey(ERROR_MESSAGE)) {
                                String errorMessage = obj.get(ERROR_MESSAGE).toString();
                                if (errorMessage.contains("JobValidationException")) {
                                    errorMessage = errorMessage.substring(errorMessage.indexOf(':') + 2);
                                }
                                displayErrorMessage(errorMessage);
                            }
                        }

                    }

                    private boolean ifResultIsAnError(String result) {
                        if (result.startsWith(SubmitEditServlet.ERROR)) {
                            displayErrorMessage(result.substring(SubmitEditServlet.ERROR.length()));
                            return true;
                        }
                        return false;
                    }

                    private boolean isResultNull(String result) {
                        if (result == null) {
                            LogModel.getInstance().logImportantMessage("Unexpected empty result");
                            displayErrorMessage("Unexpected empty result");
                            return true;
                        }
                        return false;
                    }
                });
            }

            private void handleStartAt() {
                if (startAtRadioButton.getValue()) {
                    DateTimeFormat dateTimeFormat = DateTimeFormat.getFormat(ISO_8601_FORMAT);
                    String iso8601DateStr = dateTimeFormat.format(dateChooser.getData());
                    startAtParameter.setValue(iso8601DateStr);
                    hiddenPane.add(startAtParameter);
                }
            }

            /**
             * Sets the values of the variables on _fields when click on Check button
             * _fields is used to create the listGrid from Job Variables tab
             */
            private void setAllValuesAsString() {
                if (variables != null && !variables.isEmpty()) {
                    setVariablesOnFields();
                    setFormVariableOnFields();
                }
            }
        };
    }

    private void setFormVariableOnFields() {
        Arrays.stream(fields)
              .forEach(field -> Arrays.stream(_fields)
                                      .filter(_field -> ("var_" + field.getName()).equals(_field.getName()))
                                      .findAny()
                                      .ifPresent(_field -> _field.setValue(field.getValue().toString()))

        );
    }

    private void setVariablesOnFields() {
        for (int i = 0; i < _fields.length; i++) {
            int finalI = i;
            variables.keySet()
                     .stream()
                     .filter(key -> ("var_" + key).equals(_fields[finalI].getName()))
                     .findAny()
                     .ifPresent(name -> _fields[finalI].setValue(variables.get(name).getValue()));
        }
    }

    private void updateVariables(JSONObject updatedVariablesJson) {
        JSONValue updatedVariablesJsonValue = updatedVariablesJson.get("updatedVariables");
        JSONObject obj = updatedVariablesJsonValue.isObject();
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
        JSONValue updatedDescriptionsVariablesJsonValue = updatedVariablesJson.get("updatedDescriptions");
        JSONObject objDescriptionVariables = updatedDescriptionsVariablesJsonValue.isObject();
        JSONString variableDescriptionGroups = objDescriptionVariables.get(varName).isString();
        if (variableDescriptionGroups != null) {
            variable.setDescription(variableDescriptionGroups.stringValue());
        }
    }

    private void updateModel(JSONObject updatedVariablesJson, String varName, JobVariable variable) {
        JSONValue updatedModelVariablesJsonValue = updatedVariablesJson.get("updatedModels");
        JSONObject objModelVariables = updatedModelVariablesJsonValue.isObject();
        JSONString variableJsonModel = objModelVariables.get(varName).isString();
        if (variableJsonModel != null) {
            variable.setModel(variableJsonModel.stringValue());
        }
    }

    private void updateGroup(JSONObject updatedVariablesJson, String varName, JobVariable variable) {
        JSONValue updatedVGroupsVariablesJsonValue = updatedVariablesJson.get("updatedGroups");
        JSONObject objGroupsVariables = updatedVGroupsVariablesJsonValue.isObject();
        JSONString variableJsonGroups = objGroupsVariables.get(varName).isString();
        if (variableJsonGroups != null) {
            variable.setGroup(variableJsonGroups.stringValue());
        }
    }

    private void updateHidden(JSONObject updatedVariablesJson, String varName, JobVariable variable) {
        JSONValue updatedHiddenVariablesJsonValue = updatedVariablesJson.get("updatedHidden");
        JSONObject objHiddenVariables = updatedHiddenVariablesJsonValue.isObject();
        JSONBoolean variableJsonHidden = objHiddenVariables.get(varName).isBoolean();
        if (variableJsonHidden != null) {
            variable.setHidden(variableJsonHidden.booleanValue());
        }
    }

    private void updateAdvanced(JSONObject updatedVariablesJson, String varName, JobVariable variable) {
        JSONValue updatedAdvancedVariablesJsonValue = updatedVariablesJson.get("updatedAdvanced");
        JSONObject objAdvancedVariables = updatedAdvancedVariablesJsonValue.isObject();
        JSONBoolean variableJsonAdvanced = objAdvancedVariables.get(varName).isBoolean();
        if (variableJsonAdvanced != null) {
            variable.setAdvanced(variableJsonAdvanced.booleanValue());
        }
    }

    @SuppressWarnings("squid:S3776")
    private ClickHandler clickHandlerForSubmitButton() {
        return new ClickHandler() {
            @Override
            public void onClick(ClickEvent event) {
                setAllValuesAsString();
                enableValidation(false);
                handleRadioButton();
                submitWithVariables();
                displayLoadingMessage();
            }

            private void submitWithVariables() {
                variablesActualForm.addSubmitCompleteHandler(new SubmitCompleteHandler() {
                    @Override
                    public void onSubmitComplete(SubmitCompleteEvent event) {
                        handleJobSubmission(event);
                    }

                    private void handleJobSubmission(SubmitCompleteEvent event) {
                        checkEventResults(event);
                    }
                });
                variablesActualForm.submit();
            }

            private void checkEventResults(SubmitCompleteEvent event) {
                if (event.getResults() == null || !event.getResults().startsWith(SubmitEditServlet.ERROR)) {
                    LogModel.getInstance().logMessage("Job submitted to the scheduler");
                    // If Submit Window was created for Kill & Re-submit then kill job
                    if (SubmitWindow.this.isKillAndResubmit) {
                        jobsController.killJob(SubmitWindow.this.jobId);
                    }
                    SubmitWindow.this.window.removeMember(rootPage);
                    SubmitWindow.this.window.hide();
                    SubmitWindow.this.destroy();
                } else {
                    String jsonError = event.getResults().substring(SubmitEditServlet.ERROR.length());
                    JSONValue json = Controller.parseJSON(jsonError);
                    JSONObject obj = json.isObject();
                    if (obj != null && obj.containsKey(ERROR_MESSAGE)) {
                        String errorMessage = obj.get(ERROR_MESSAGE).toString();
                        if (errorMessage.contains("JobValidationException")) {
                            errorMessage = errorMessage.substring(errorMessage.indexOf(':') + 2);
                        }
                        displayErrorMessage(errorMessage);
                    } else {
                        displayErrorMessage(jsonError);
                    }
                }
            }

            private void handleRadioButton() {
                if (startAtRadioButton.getValue()) {
                    DateTimeFormat dateTimeFormat = DateTimeFormat.getFormat(ISO_8601_FORMAT);
                    String iso8601DateStr = dateTimeFormat.format(dateChooser.getData());
                    startAtParameter.setValue(iso8601DateStr);
                    hiddenPane.add(startAtParameter);
                } else if (startAccordingPlanningRadioButton.getValue() && !isExecCalendarValueNull) {
                    planParameter.setValue("true");
                    hiddenPane.add(planParameter);
                }
            }

            /**
             * Sets the values of the variables on _fields when click on Submit button
             * _fields is used to create the listGrid from Job Variables tab
             */
            private void setAllValuesAsString() {
                if (variables != null && !variables.isEmpty()) {
                    setVariablesOnFields();
                    setFormVariableOnFields();
                }
            }
        };
    }

    private com.google.gwt.event.dom.client.ClickHandler clickHandlerForUploadFromFileButton(FormPanel toSubmit) {
        return event -> {
            String fileName = fileUpload.getFilename();
            if ("".compareTo(fileName) != 0) {
                displayLoadingMessage();
                toSubmit.submit();
            } else {
                displayErrorMessage("Nothing to upload. Please select a file.");
            }

        };
    }

    private com.google.gwt.event.dom.client.ClickHandler
            clickHandlerForUploadFromCatalogButton(VerticalPanel formContent, FormPanel importFromCatalogformPanel) {
        return event -> {
            // filter only valid items
            if (bucketsListBox.getSelectedIndex() > 0 && workflowsListBox.getSelectedIndex() > 0) {
                String selectedWorkflowLabel = workflowsListBox.getSelectedValue();
                String selectedBucketName = bucketsListBox.getSelectedValue();
                formContent.add(new Hidden("bucketName", selectedBucketName));
                formContent.add(new Hidden("workflowName", selectedWorkflowLabel));
                displayLoadingMessage();
                importFromCatalogformPanel.submit();
            }
        };
    }

    private void redrawVariables(String job) {
        currentJobDescriptor = job;
        removeBottomMembers();
        setEnabledStartAtPart(true);
        fillVarsPart(job);
        rootPage.addMember(startAtLayout);
        clearMessagePanel();
        rootPage.addMember(messagePanel);
        rootPage.addMember(submitCancelButtons);
        rootPage.reflow();
    }

    private void removeBottomMembers() {
        rootPage.removeMember(varsLayout);
        rootPage.removeMember(startAtLayout);
        rootPage.removeMember(messagePanel);
        rootPage.removeMember(submitCancelButtons);
    }

    private void enableValidation(boolean enable) {
        validate.setValue(Boolean.toString(enable));
    }

    private void resetAllHandlers() {
        if (changedMinuteHR != null) {
            changedMinuteHR.removeHandler();
        }
        changedMinuteHR = dateChooser.getTimeItem().getMinuteItem().addChangedHandler(newCHForMinuteField());

        if (changedHourHR != null) {
            changedHourHR.removeHandler();
        }
        changedHourHR = dateChooser.getTimeItem().getHourItem().addChangedHandler(newCHForHourField());

        if (todayClickHR != null) {
            todayClickHR.removeHandler();
        }
        todayClickHR = dateChooser.getTodayButton().addClickHandler(newCHForTodayButton());

        if (gridClickHR != null) {
            gridClickHR.removeHandler();
        }
        gridClickHR = dateChooser.addClickHandler(newCHDateGrid());
    }

    private ClickHandler newCHForTodayButton() {
        return clickEvent -> {
            updateScheduledTimeForToday();
            resetAllHandlers();
        };
    }

    private ChangedHandler newCHForMinuteField() {
        return changedEvent -> {
            updateScheduledTimeAt();
            resetAllHandlers();
        };
    }

    private ChangedHandler newCHForHourField() {
        return changedEvent -> {
            updateScheduledTimeAt();
            resetAllHandlers();
        };
    }

    private ClickHandler newCHDateGrid() {
        return clickEvent -> {
            startAtRadioButton.setText("At " + dateChooser.getData().toString());
            resetAllHandlers();
        };
    }

    private void setEnabledStartAtPart(boolean state) {
        setStartAccordingPlanningRadioButtonState(job);
        startNowRadioButton.setEnabled(state);
        startAtRadioButton.setEnabled(state);
        submitButton.setDisabled(!state);
        checkButton.setDisabled(!state);
        if (!state) {
            submitButton.setTooltip("A workflow must be selected first");
        } else {
            submitButton.setTooltip("");
        }
    }

    private void uploadJobXMLForResubmit() {
        RequestBuilder req = new RequestBuilder(RequestBuilder.GET, URL_UPLOAD_FILE);
        req.setHeader(SESSION_ID_PARAMETER_NAME, LoginModel.getInstance().getSessionId());
        req.setHeader(JOB_ID_PARAMETER_NAME, jobId);
        req.setCallback(new RequestCallback() {
            @Override
            public void onResponseReceived(Request request, Response response) {
                handleUploadDoneResponse(response.getText());
            }

            @Override
            public void onError(Request request, Throwable exception) {
                // Exception is handled on request.send()
            }
        });
        try {
            req.send();
        } catch (RequestException e) {
            LogModel.getInstance().logImportantMessage("Error occurred when fetching workflow xml for job " + jobId);
            // For Dev debug mode only
            e.printStackTrace();
        }
    }

    private void handleUploadDoneResponse(String responseText) {
        String jobEditKey = "jobEdit";
        try {
            JSONValue js = JSONParser.parseStrict(responseText);
            JSONObject obj = js.isObject();

            if (obj.get(jobEditKey) != null && obj.get(jobEditKey).isString() != null) {
                String val = obj.get(jobEditKey).isString().stringValue();
                job = new String(org.ow2.proactive_grid_cloud_portal.common.shared.Base64Utils.fromBase64(val));
                // if the job has an EXECUTION_CALENDAR Generic Information defined, the startAccordingToPlanningRadioButton becomes visible, and invisible otherwise
                setStartAccordingPlanningRadioButtonState(job);
                variables = readVars(job);
                redrawVariables(job);
            } else {
                LogModel.getInstance().logImportantMessage(JSON_ERROR);
                displayErrorMessage(responseText);
                //Force disable check&submit buttons to prevent confusion if a valid job was uploaded first but not submitted
                setEnabledStartAtPart(false);
                startAccordingPlanningRadioButton.setVisible(false);
            }

        } catch (JSONException t) {
            LogModel.getInstance().logImportantMessage(JSON_ERROR);
            displayErrorMessage(responseText);
            //Force disable check&submit buttons to prevent confusion if a valid job was uploaded first but not submitted
            setEnabledStartAtPart(false);
            startAccordingPlanningRadioButton.setVisible(false);
        }

    }

    private SubmitCompleteHandler fileUploadCompleteHandler() {
        return event -> handleUploadDoneResponse(event.getResults());
    }

    private void build() {

        initRootPage(); // ------------ root page of the window
        if (!isResubmit && !isKillAndResubmit) {
            buildCatalogUrl();
            initSelectWfPart(); // -------- Select workflow Panel
        }
        initVarsPart(); // ------------ Fill workflow variables Panel
        initSubmitAtPart(); // -------- Submit at given time Panel
        initMessagesPart(); // --------- loading and error messages
        initButtonsPart(); // --------- Close and Submit buttons

        setEnabledStartAtPart(false);

        this.window = new Window();
        if (isResubmit) {
            this.window.setTitle("Re-submit job " + jobId);
        } else {
            this.window.setTitle(isKillAndResubmit ? "Kill & Re-Submit job " + jobId : "Submit a new job");
        }
        this.window.setShowMinimizeButton(false);
        this.window.setIsModal(true);
        this.window.setShowModalMask(true);
        this.window.addItem(rootPage);
        this.window.setWidth(WINDOW_WIDTH);
        this.window.setHeight(WINDOW_HEIGHT);
        this.window.centerInPage();
        this.window.setCanDragResize(true);
        if (isResubmit || isKillAndResubmit) {
            uploadJobXMLForResubmit();
        }
    }

    /**
     * @param jobDescriptor an XML job descriptor as a string
     * @return the name/value of all <variables><variable name value> elements
     */
    private Map<String, JobVariable> readVars(String jobDescriptor) {
        /*
         * this will fail if someday the XML schema gets another <variable> tag
         * elsewhere
         */
        NodeList jobVariables = XMLParser.parse(jobDescriptor).getElementsByTagName("variable");
        Map<String, JobVariable> ret = new LinkedHashMap<>();

        if (jobVariables.getLength() > 0) {
            for (int i = 0; i < jobVariables.getLength(); i++) {
                Node variableNode = jobVariables.item(i);

                if (variableNode != null && !isTaskVariableElement(variableNode)) {
                    NamedNodeMap attrs = variableNode.getAttributes();
                    try {
                        checkIfAttributes(ret, variableNode, attrs);
                    } catch (JavaScriptException t) {
                        // Node.hasAttributes() throws if there are no attributes... (GWT 2.1.0)
                    }
                }
            }
        }
        return ret;
    }

    private void checkIfAttributes(Map<String, JobVariable> ret, Node variableNode, NamedNodeMap attrs) {
        if (attrs != null && variableNode.hasAttributes()) {
            String name = extractNodeValue(attrs, KEY_OF_NAME);
            String value = extractNodeValue(attrs, KEY_OF_VALUE);
            String model = extractNodeValue(attrs, KEY_OF_MODEL);
            String group = extractNodeValue(attrs, KEY_OF_GROUP);
            String description = extractNodeValue(attrs, KEY_OF_DESCRIPTION);
            boolean hidden = Boolean.parseBoolean(extractNodeValue(attrs, KEY_OF_HIDDEN));
            boolean advanced = Boolean.parseBoolean(extractNodeValue(attrs, KEY_OF_ADVANCED));
            createJobVariable(ret, name, value, model, description, group, advanced, hidden);
        }
    }

    private void createJobVariable(Map<String, JobVariable> ret, String name, String value, String model,
            String description, String group, boolean advanced, boolean hidden) {
        if (name != null && value != null && name.matches("[A-Za-z0-9._]+")) {
            // this won't necessarily be a problem at job submission,
            // but it definitely will be here in the client; don't bother
            ret.put(name, new JobVariable(name, value, model, description, group, advanced, hidden));
        }
    }

    private String extractNodeValue(NamedNodeMap attrs, String keyOf) {

        for (int j = 0; j < attrs.getLength(); j++) {
            Node attr = attrs.item(j);
            if (attr.getNodeName().equals(keyOf)) {
                return attr.getNodeValue();
            }

        }
        return null;
    }

    private Boolean isExecutionCalendarGIDefined(String jobDescriptor) {
        Document dom = XMLParser.parse(jobDescriptor);

        NodeList genericInfo = dom.getElementsByTagName("genericInformation");
        // check if the job has genericInformation or not
        if (genericInfo != null && genericInfo.getLength() > 0) {
            // search calendar GI in the first item
            return searchCalendarGI(genericInfo.item(0));
        } else
            return false;
    }

    private Boolean searchCalendarGI(Node genericInfoNode) {
        Boolean exists = false;
        NodeList list = genericInfoNode.getChildNodes();
        for (int i = 0; i < list.getLength(); i++) {
            Node node = list.item(i);
            if (node.getNodeName().equals("info") && node.hasAttributes()) {
                NamedNodeMap attributes = node.getAttributes();
                for (int j = 0; j < attributes.getLength(); j++) {
                    Node attribute = attributes.item(j);
                    exists = checkExecutionCalendarName(exists, attribute);
                    if (checkExecutionCalendarValue(exists, attribute)) {
                        return true;
                    }

                }
            }
        }
        return false;
    }

    private boolean checkExecutionCalendarValue(Boolean exists, Node attribute) {
        if (isAttributeExecCalendarValueDefined(attribute, KEY_OF_VALUE) && exists) {
            if (!attribute.getNodeValue().isEmpty()) {
                isExecCalendarValueNull = false;
            }
            return true;
        }
        return false;
    }

    private Boolean checkExecutionCalendarName(Boolean exists, Node attribute) {
        if (attribute.getNodeType() == Node.ATTRIBUTE_NODE && attribute.getNodeName().equals(KEY_OF_NAME) &&
            attribute.getNodeValue().equalsIgnoreCase("execution_calendars")) {
            exists = true;
        }
        return exists;
    }

    private Boolean isAttributeExecCalendarValueDefined(Node attribute, String name) {
        return attribute.getNodeType() == Node.ATTRIBUTE_NODE && attribute.getNodeName().equals(name);
    }

    private boolean isTaskVariableElement(Node node) {
        if (node.getParentNode() != null && node.getParentNode().getParentNode() != null) {
            Node grandparentNode = node.getParentNode().getParentNode();
            if (grandparentNode.getNodeName().equals("task")) {
                return true;
            }
        }
        return false;
    }

}
