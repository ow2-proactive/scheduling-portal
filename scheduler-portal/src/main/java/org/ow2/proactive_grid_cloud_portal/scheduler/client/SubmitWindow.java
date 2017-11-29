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

import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;

import org.ow2.proactive_grid_cloud_portal.common.client.Images;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LoginModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.server.SubmitEditServlet;
import org.ow2.proactive_grid_cloud_portal.scheduler.shared.SchedulerConfig;

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
import com.google.gwt.json.client.JSONArray;
import com.google.gwt.json.client.JSONBoolean;
import com.google.gwt.json.client.JSONException;
import com.google.gwt.json.client.JSONObject;
import com.google.gwt.json.client.JSONParser;
import com.google.gwt.json.client.JSONString;
import com.google.gwt.json.client.JSONValue;
import com.google.gwt.user.client.ui.Button;
import com.google.gwt.user.client.ui.FileUpload;
import com.google.gwt.user.client.ui.FormPanel;
import com.google.gwt.user.client.ui.FormPanel.SubmitCompleteEvent;
import com.google.gwt.user.client.ui.FormPanel.SubmitCompleteHandler;
import com.google.gwt.user.client.ui.Hidden;
import com.google.gwt.user.client.ui.HorizontalPanel;
import com.google.gwt.user.client.ui.ListBox;
import com.google.gwt.user.client.ui.RadioButton;
import com.google.gwt.user.client.ui.VerticalPanel;
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
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
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

    private static final String CATALOG_SELECT_BUCKET = "Select a Bucket";

    private static final String CATALOG_SELECT_WF = "Select a Workflow";

    private static final String ISO_8601_FORMAT = "yyyy-MM-dd'T'HH:mm:ssZZZ";

    private static final String URL_SUBMIT_XML = GWT.getModuleBaseURL() + "submitedit";

    private static final String URL_UPLOAD_FILE = GWT.getModuleBaseURL() + "uploader";

    private static final String METHOD_INSTRUCTION = "Select a method";

    private static final String METHOD_FROM_FILE = "import from file";

    private static final String METHOD_FROM_CATALOG = "import from Catalog";

    private static final String SESSION_ID_PARAMETER_NAME = "sessionId";

    private static final int width = 600;

    private static final int height = 520;

    private Window window;

    private SchedulerController controller;

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

    private VerticalPanel selectWorkflowButtonsPanel; // Panel that holds the
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

    // ----- Catalog temp maps
    private HashMap<String, Integer> catalogBucketsMap;

    private String CATALOG_URL = null;

    private Map<String, JobVariable> variables;

    private String job;

    private Boolean isExecCalendarValueNull = true; // capture if EXECUTION_CALENDAR value is null

    /**
     * Default constructor
     *
     * @param controller
     */
    public SubmitWindow(SchedulerController controller) {
        this.controller = controller;
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
        String catalogUrlFromConfig = SchedulerConfig.get().getCatalogUrl();
        String defaultCatalogUrl = GWT.getHostPageBaseURL().replace("/scheduler/", "/") + "catalog";
        if (catalogUrlFromConfig == null || catalogUrlFromConfig.isEmpty()) {
            CATALOG_URL = defaultCatalogUrl;
        } else {
            CATALOG_URL = catalogUrlFromConfig;
        }
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
        selectWfLayout.setHeight("130px");

        // This panel changes depending on the selected method of getting a
        // workflow
        selectWorkflowButtonsPanel = new VerticalPanel();
        selectWorkflowButtonsPanel.setSpacing(5);
        selectWorkflowButtonsPanel.setHeight("50px");

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

    private void fillVarsPart() {

        DynamicForm variablesVisualForm = initVariablesVisualForm();

        Layout hiddenVarsLayout = initVariablesActualForm();

        initVarsLayout();
        varsLayout.addMember(variablesVisualForm);
        varsLayout.addMember(hiddenVarsLayout);
        rootPage.addMember(varsLayout);
        rootPage.reflow();
    }

    private DynamicForm initVariablesVisualForm() {
        // presentation form
        DynamicForm variablesVisualForm;
        variablesVisualForm = new DynamicForm();
        variablesVisualForm.setNumCols(3);
        variablesVisualForm.setColWidths("25%", "50%", "25%");
        fields = new FormItem[variables.size() * 2];
        int i = 0;

        for (Entry<String, JobVariable> var : variables.entrySet()) {
            TextItem variableItem = createVariableItem(var);
            fields[i++] = variableItem;
            String model = var.getValue().getModel();
            BlurbItem modelItem = createModelItem(model);
            fields[i++] = modelItem;
        }

        variablesVisualForm.setFields(fields);
        return variablesVisualForm;
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
        varsLayout.setGroupTitle("2. Fill workflow variables");
        varsLayout.setWidth100();
        varsLayout.setHeight("100px");
        varsLayout.setMaxHeight(100);
        varsLayout.setPadding(5);
        varsLayout.setOverflow(Overflow.AUTO);
    }

    private BlurbItem createModelItem(String model) {
        BlurbItem modelLabel = null;
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
        t.setValue(var.getValue().getValue());
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
        } else {
            startAccordingPlanningRadioButton.setVisible(false);
        }
    }

    private void initSubmitAtPart() {
        startAtLayout = new VLayout();
        startAtLayout.setIsGroup(true);
        startAtLayout.setGroupTitle("3. Scheduled time");

        startAtParameter = new Hidden("START_AT");

        startAccordingPlanningRadioButton = new RadioButton("startAtRadioGroup",
                                                            "Planned according to embedded Execution Calendar defintion");
        startNowRadioButton = new RadioButton("startAtRadioGroup", "As soon as possible");
        startAtRadioButton = new RadioButton("startAtRadioGroup", "At");

        startAccordingPlanningRadioButton.setVisible(true);

        startNowRadioButton.setValue(true);
        startAtRadioButton.setValue(false);

        startAccordingPlanningRadioButton.addClickHandler(new com.google.gwt.event.dom.client.ClickHandler() {
            @Override
            public void onClick(com.google.gwt.event.dom.client.ClickEvent event) {
                if (isExecCalendarValueNull) {
                    displayErrorMessage("EXECUTION_CALENDAR value is empty.");
                }

                startAtLayout.removeMember(dateChooser);
            }
        });

        startNowRadioButton.addClickHandler(new com.google.gwt.event.dom.client.ClickHandler() {
            @Override
            public void onClick(com.google.gwt.event.dom.client.ClickEvent event) {
                startAtRadioButton.setText("At");
                startAtLayout.removeMember(dateChooser);
            }
        });

        startAtRadioButton.addClickHandler(new com.google.gwt.event.dom.client.ClickHandler() {
            @Override
            public void onClick(com.google.gwt.event.dom.client.ClickEvent event) {
                startAtLayout.addMember(dateChooser);
                DateTimeFormat dateTimeFormat = DateTimeFormat.getFormat(ISO_8601_FORMAT);
                String iso8601DateStr = dateTimeFormat.format(dateChooser.getData());
                startAtParameter.setValue(iso8601DateStr);
                updateScheduledTimeForToday();
                resetAllHandlers();
            }
        });

        dateChooser = new DateChooser();
        dateChooser.setShowTimeItem(true);
        dateChooser.setUse24HourTime(true);
        dateChooser.setShowTodayButton(true);
        dateChooser.setShowApplyButton(false);
        dateChooser.setWidth(width / 2);
        dateChooser.setLayoutAlign(Alignment.CENTER);
        dateChooser.setMargin(10);

        startAtRadioGroupPanel = new VerticalPanel();
        startAtRadioGroupPanel.setSpacing(10);

        startAtRadioGroupPanel.add(startAccordingPlanningRadioButton);

        startAtRadioGroupPanel.add(startNowRadioButton);
        startAtRadioGroupPanel.add(startAtRadioButton);
        startAtRadioGroupPanel.setHeight("30px");

        startAtLayout.addMember(startAtRadioGroupPanel);
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
        rootPage.addMember(messagePanel);
    }

    private void initButtonsPart() {
        submitCancelButtons = new HLayout();
        submitCancelButtons.setMargin(10);
        submitCancelButtons.setMembersMargin(5);
        submitCancelButtons.setHeight(30);
        submitCancelButtons.setWidth100();
        submitCancelButtons.setAlign(Alignment.RIGHT);

        submitButton = new IButton("Submit");
        submitButton.setIcon(Images.instance.ok_16().getSafeUri().asString());
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
        cancelButton.addClickHandler(new ClickHandler() {
            @Override
            public void onClick(ClickEvent event) {
                SubmitWindow.this.window.hide();
                SubmitWindow.this.destroy();
            }
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
        errorLabel.setStyleName("errorMessage");

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

        final FormPanel importFromFileformPanel = new FormPanel();
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
        bucketsListBox = new ListBox();
        workflowsListBox = new ListBox();

        bucketsListBox.setEnabled(false);
        bucketsListBox.addItem(CATALOG_SELECT_BUCKET);

        workflowsListBox.setEnabled(false);
        workflowsListBox.addItem(CATALOG_SELECT_WF);

        bucketsListBox.addChangeHandler(new ChangeHandler() {
            @Override
            public void onChange(ChangeEvent event) {
                String selectedBucket = bucketsListBox.getSelectedValue();
                if (!CATALOG_SELECT_BUCKET.equals(selectedBucket)) {
                    String workflowUrl = CATALOG_URL + "/buckets/" + catalogBucketsMap.get(selectedBucket) +
                                         "/resources?kind=workflow";
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
                                String workflowName = workflow.get("name").isString().stringValue();
                                workflowsListBox.addItem(workflowName);
                            }
                            workflowsListBox.setEnabled(true);
                        }

                        @Override
                        public void onError(Request request, Throwable exception) {
                            GWT.log("OOPS:" + request.toString());
                        }
                    });
                    try {
                        req.send();
                    } catch (RequestException e) {
                        GWT.log("Error occured when fetching workflows from Catalog");
                        e.printStackTrace();

                    }
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
                catalogBucketsMap = new HashMap<>(bucketSize);
                for (int i = 0; i < bucketSize; i++) {
                    JSONObject bucket = buckets.get(i).isObject();
                    String bucketName = bucket.get("name").isString().stringValue();
                    String bucketId = bucket.get("id").isNumber().toString();
                    String dropdownListItemLabel = bucketName + " (" + bucketId + ")";
                    bucketsListBox.addItem(bucketName + " (" + bucketId + ")");
                    catalogBucketsMap.put(dropdownListItemLabel, Integer.parseInt(bucketId));
                }
                bucketsListBox.setEnabled(true);
            }

            @Override
            public void onError(Request request, Throwable exception) {
                GWT.log("Error occured when fetching buckets from Catalog");
            }
        });

        try {
            req.send();
        } catch (RequestException e) {
            GWT.log("Error occured when fetching buckets from Catalog");
            e.printStackTrace();
        }

        fromCatalogPanel.add(bucketsListBox);
        fromCatalogPanel.add(workflowsListBox);

        bucketsListBox.setWidth("130px");
        workflowsListBox.setWidth("230px");

        final VerticalPanel formContent = new VerticalPanel();
        formContent.setHeight("30px");
        formContent.add(new Hidden(SESSION_ID_PARAMETER_NAME, LoginModel.getInstance().getSessionId()));

        final FormPanel importFromCatalogformPanel = new FormPanel();
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

    private ClickHandler clickHandlerForCheckButton() {
        return new ClickHandler() {
            @Override
            public void onClick(ClickEvent event) {
                for (int i = 0; i < _fields.length; i++) {
                    String val = "";
                    if (fields[2 * i].getValue() != null) {
                        val = fields[2 * i].getValue().toString();
                    }
                    _fields[i].setValue(val);
                }
                if (startAtRadioButton.getValue()) {
                    DateTimeFormat dateTimeFormat = DateTimeFormat.getFormat(ISO_8601_FORMAT);
                    String iso8601DateStr = dateTimeFormat.format(dateChooser.getData());
                    startAtParameter.setValue(iso8601DateStr);
                    hiddenPane.add(startAtParameter);
                }
                enableValidation(true);
                variablesActualForm.addSubmitCompleteHandler(new SubmitCompleteHandler() {
                    @Override
                    public void onSubmitComplete(SubmitCompleteEvent event) {
                        String result = event.getResults();
                        if (result == null) {
                            GWT.log("Unexpected empty result");
                            displayErrorMessage("Unexpected empty result");
                            return;
                        }

                        if (result.startsWith(SubmitEditServlet.ERROR)) {
                            displayErrorMessage(result.substring(SubmitEditServlet.ERROR.length()));
                            return;
                        }

                        JSONValue json = controller.parseJSON(result);
                        JSONObject obj = json.isObject();
                        if (obj != null) {
                            if (obj.containsKey("valid")) {
                                if (((JSONBoolean) obj.get("valid")).booleanValue()) {
                                    if (obj.containsKey("updatedVariables")) {
                                        updateVariables(obj.get("updatedVariables"));
                                        redrawVariables();
                                    }
                                    GWT.log("Job validated");
                                    displayInfoMessage("Job is valid");
                                } else if (obj.containsKey("errorMessage")) {
                                    String errorMessage = obj.get("errorMessage").toString();
                                    if (errorMessage.contains("JobValidationException")) {
                                        errorMessage = errorMessage.substring(errorMessage.indexOf(":") + 2);
                                    }
                                    displayErrorMessage(errorMessage);
                                }
                            }
                        }
                    }
                });
                variablesActualForm.submit();
                displayLoadingMessage();

            }
        };
    }

    private void updateVariables(JSONValue updatedVariablesJsonValue) {
        JSONObject obj = updatedVariablesJsonValue.isObject();
        if (obj != null) {
            for (String varName : obj.keySet()) {
                JobVariable variable = variables.get(varName);
                if (variable != null) {
                    JSONValue variableJsonValue = obj.get(varName);
                    JSONString variableJsonString = variableJsonValue.isString();
                    if (variableJsonString != null) {
                        variable.setValue(variableJsonString.stringValue());
                    }

                }
            }
        }
    }

    private ClickHandler clickHandlerForSubmitButton() {
        return new ClickHandler() {
            @Override
            public void onClick(ClickEvent event) {
                for (int i = 0; i < _fields.length; i++) {
                    String val = "";
                    if (fields[2 * i].getValue() != null) {
                        val = fields[2 * i].getValue().toString();
                    }
                    _fields[i].setValue(val);
                }
                enableValidation(false);
                if (startAtRadioButton.getValue()) {
                    DateTimeFormat dateTimeFormat = DateTimeFormat.getFormat(ISO_8601_FORMAT);
                    String iso8601DateStr = dateTimeFormat.format(dateChooser.getData());
                    startAtParameter.setValue(iso8601DateStr);
                    hiddenPane.add(startAtParameter);
                }
                variablesActualForm.addSubmitCompleteHandler(new SubmitCompleteHandler() {
                    @Override
                    public void onSubmitComplete(SubmitCompleteEvent event) {
                        if (event.getResults() == null || !event.getResults().startsWith(SubmitEditServlet.ERROR)) {

                            GWT.log("Job submitted to the scheduler");
                            SubmitWindow.this.window.removeMember(rootPage);
                            SubmitWindow.this.window.hide();
                            SubmitWindow.this.destroy();
                        } else {
                            String jsonError = event.getResults().substring(SubmitEditServlet.ERROR.length());
                            JSONValue json = controller.parseJSON(jsonError);
                            JSONObject obj = json.isObject();
                            if (obj != null && obj.containsKey("errorMessage")) {
                                String errorMessage = obj.get("errorMessage").toString();
                                if (errorMessage.contains("JobValidationException")) {
                                    errorMessage = errorMessage.substring(errorMessage.indexOf(":") + 2);
                                }
                                displayErrorMessage(errorMessage);

                            } else {
                                displayErrorMessage(jsonError);
                            }

                        }
                    }
                });
                variablesActualForm.submit();
                displayLoadingMessage();

            }
        };
    }

    private com.google.gwt.event.dom.client.ClickHandler clickHandlerForUploadFromFileButton(final FormPanel toSubmit) {
        return new com.google.gwt.event.dom.client.ClickHandler() {
            @Override
            public void onClick(com.google.gwt.event.dom.client.ClickEvent event) {
                String fileName = fileUpload.getFilename();
                if ("".compareTo(fileName) != 0) {
                    displayLoadingMessage();
                    toSubmit.submit();
                } else {
                    displayErrorMessage("Nothing to upload. Please select a file.");
                }

            }
        };
    }

    private com.google.gwt.event.dom.client.ClickHandler clickHandlerForUploadFromCatalogButton(
            final VerticalPanel formContent, final FormPanel importFromCatalogformPanel) {
        return new com.google.gwt.event.dom.client.ClickHandler() {
            @Override
            public void onClick(com.google.gwt.event.dom.client.ClickEvent event) {
                // filter only valid items
                if (bucketsListBox.getSelectedIndex() > 0 && workflowsListBox.getSelectedIndex() > 0) {
                    String selectedBucketLabel = bucketsListBox.getSelectedValue();
                    String selectedWorkflowLabel = workflowsListBox.getSelectedValue();
                    String selectedBucketId = String.valueOf(catalogBucketsMap.get(selectedBucketLabel));
                    formContent.add(new Hidden("bucketId", selectedBucketId));
                    formContent.add(new Hidden("workflowName", selectedWorkflowLabel));
                    displayLoadingMessage();
                    importFromCatalogformPanel.submit();
                }
            }
        };
    }

    private SubmitCompleteHandler fileUploadCompleteHandler() {
        return new SubmitCompleteHandler() {
            @Override
            public void onSubmitComplete(SubmitCompleteEvent event) {

                String fn = fileUpload.getFilename();
                // chrome workaround
                final String fileName = fn.replace("C:\\fakepath\\", "");
                String res = event.getResults();

                try {
                    JSONValue js = JSONParser.parseStrict(res);
                    JSONObject obj = js.isObject();

                    if (obj.get("jobEdit") != null && obj.get("jobEdit").isString() != null) {
                        String val = obj.get("jobEdit").isString().stringValue();
                        job = new String(org.ow2.proactive_grid_cloud_portal.common.shared.Base64Utils.fromBase64(val));
                        // if the job has an EXECUTION_CALENDAR Generic Information defined, the startAccordingToPlanningRadioButton becomes visible, and invisible otherwise
                        setStartAccordingPlanningRadioButtonState(job);
                        variables = readVars(job);
                    } else {
                        GWT.log("JSON parse ERROR");
                        return;
                    }

                } catch (JSONException t) {
                    GWT.log("JSON parse ERROR");
                    return;
                }
                redrawVariables();
            }
        };
    }

    private void redrawVariables() {
        removeBottomMembers();
        setEnabledStartAtPart(true);
        fillVarsPart();
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
        return new ClickHandler() {
            @Override
            public void onClick(ClickEvent clickEvent) {
                updateScheduledTimeForToday();
                resetAllHandlers();
            }
        };
    }

    private ChangedHandler newCHForMinuteField() {
        return new ChangedHandler() {
            @Override
            public void onChanged(ChangedEvent changedEvent) {
                updateScheduledTimeAt();
                resetAllHandlers();
            }
        };
    }

    private ChangedHandler newCHForHourField() {
        return new ChangedHandler() {
            @Override
            public void onChanged(ChangedEvent changedEvent) {
                updateScheduledTimeAt();
                resetAllHandlers();
            }
        };
    }

    private ClickHandler newCHDateGrid() {
        return new ClickHandler() {
            @Override
            public void onClick(ClickEvent clickEvent) {
                startAtRadioButton.setText("At " + dateChooser.getData().toString());
                resetAllHandlers();
            }
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

    private void build() {

        buildCatalogUrl();

        initRootPage(); // ------------ root page of the window
        initSelectWfPart(); // -------- Select workflow Panel
        initVarsPart(); // ------------ Fill workflow variables Panel
        initSubmitAtPart(); // -------- Submit at given time Panel
        initMessagesPart(); // --------- loading and error messages
        initButtonsPart(); // --------- Close and Submit buttons

        setEnabledStartAtPart(false);

        this.window = new Window();
        this.window.setTitle("Submit a new job");
        this.window.setShowMinimizeButton(false);
        this.window.setIsModal(true);
        this.window.setShowModalMask(true);
        this.window.addItem(rootPage);
        this.window.setWidth(this.width);
        this.window.setHeight(this.height);
        this.window.centerInPage();
        this.window.setCanDragResize(true);
    }

    /**
     * @param jobDescriptor
     *            an XML job descriptor as a string
     * @return the name/value of all <variables><variable name value> elements
     */
    private Map<String, JobVariable> readVars(String jobDescriptor) {
        /*
         * this will fail if someday the XML schema gets another <variable> tag
         * elsewhere
         */
        Document dom = XMLParser.parse(jobDescriptor);
        NodeList variables = dom.getElementsByTagName("variable");
        Map<String, JobVariable> ret = new LinkedHashMap<>();

        if (variables.getLength() > 0) {
            for (int i = 0; i < variables.getLength(); i++) {
                Node variableNode = variables.item(i);

                if (variableNode != null && !isTaskVariableElement(variableNode)) {
                    NamedNodeMap attrs = variableNode.getAttributes();
                    try {
                        if (attrs != null && variableNode.hasAttributes()) {
                            String name = null;
                            String value = null;
                            String model = null;
                            for (int j = 0; j < attrs.getLength(); j++) {
                                Node attr = attrs.item(j);
                                if (attr.getNodeName().equals("name")) {
                                    name = attr.getNodeValue();
                                }
                                if (attr.getNodeName().equals("value")) {
                                    value = attr.getNodeValue();
                                }
                                if (attr.getNodeName().equals("model")) {
                                    model = attr.getNodeValue();
                                }
                            }
                            if (name != null && value != null) {
                                if (!name.matches("[A-Za-z0-9._]+")) {
                                    // this won't necessarily be a problem at
                                    // job submission,
                                    // but it definitely will be here in the
                                    // client; don't bother
                                    continue;
                                }

                                ret.put(name, new JobVariable(name, value, model));
                            }
                        }
                    } catch (JavaScriptException t) {
                        // Node.hasAttributes() throws if there are no
                        // attributes... (GWT 2.1.0)
                    }
                }
            }
        }
        return ret;
    }

    private Boolean isExecutionCalendarGIDefined(String jobDescriptor) {
        Document dom = XMLParser.parse(jobDescriptor);
        Boolean exists = false;
        Boolean executionCalendarDefined = false;
        NodeList genericInfo = dom.getElementsByTagName("genericInformation");
        // check if the job has genericInformation or not
        if (genericInfo != null && genericInfo.getLength() > 0) {
            // get the first item
            Node root = genericInfo.item(0);
            NodeList list = root.getChildNodes();
            for (int i = 0; i < list.getLength(); i++) {
                Node node = list.item(i);
                if (node.getNodeName().equals("info") && node.hasAttributes()) {
                    NamedNodeMap attributes = node.getAttributes();
                    for (int j = 0; j < attributes.getLength(); j++) {
                        Node attribute = attributes.item(j);
                        if (attribute.getNodeType() == Node.ATTRIBUTE_NODE && attribute.getNodeName().equals("name") &&
                            attribute.getNodeValue().equalsIgnoreCase("execution_calendars")) {
                            exists = true;
                        }
                        if (isAttributeExecCalendarValueDefined(attribute, "value") && exists) {
                            executionCalendarDefined = true;
                            if (!attribute.getNodeValue().isEmpty())
                                isExecCalendarValueNull = false;
                        }

                    }
                }
            }
        }
        return executionCalendarDefined;

    }

    private Boolean isAttributeExecCalendarValueDefined(Node attribute, String name) {
        if (attribute.getNodeType() == Node.ATTRIBUTE_NODE && attribute.getNodeName().equals(name)) {
            return true;
        } else
            return false;
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

    class JobVariable {
        private String name;

        private String value;

        private String model;

        public JobVariable(String name, String value, String model) {
            this.name = name;
            this.value = value;
            this.model = model;
        }

        public String getName() {
            return name;
        }

        public void setValue(String value) {
            this.value = value;
        }

        public String getValue() {
            return value;
        }

        public String getModel() {
            return model;
        }

    }

}
