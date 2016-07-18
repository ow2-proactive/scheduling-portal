/*
 * ################################################################
 *
 * ProActive Parallel Suite(TM): The Java(TM) library for
 *    Parallel, Distributed, Multi-Core Computing for
 *    Enterprise Grids & Clouds
 *
 * Copyright (C) 1997-2015 INRIA/University of
 *                 Nice-Sophia Antipolis/ActiveEon
 * Contact: proactive@ow2.org or contact@activeeon.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Affero General Public License
 * as published by the Free Software Foundation; version 3 of
 * the License.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
 * USA
 *
 * If needed, contact us to obtain a release under GPL Version 2 or 3
 * or a different license than the AGPL.
 *
 *  Initial developer(s):               The ProActive Team
 *                        http://proactive.inria.fr/team_members.htm
 *  Contributor(s):
 *
 * ################################################################
 * $$PROACTIVE_INITIAL_DEV$$
 */
package org.ow2.proactive_grid_cloud_portal.scheduler.client;

import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import com.google.gwt.event.dom.client.ChangeEvent;
import com.google.gwt.event.dom.client.ChangeHandler;
import com.google.gwt.event.shared.HandlerRegistration;
import com.google.gwt.http.client.*;
import com.google.gwt.i18n.client.DateTimeFormat;
import com.google.gwt.json.client.*;
import com.google.gwt.user.client.ui.*;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.util.DateUtil;
import com.smartgwt.client.widgets.DateChooser;
import com.smartgwt.client.widgets.form.fields.*;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import org.ow2.proactive_grid_cloud_portal.common.client.Images;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LoginModel;

import com.google.gwt.core.client.GWT;
import com.google.gwt.core.client.JavaScriptException;
import com.google.gwt.user.client.ui.FormPanel.SubmitCompleteEvent;
import com.google.gwt.user.client.ui.FormPanel.SubmitCompleteHandler;
import com.google.gwt.xml.client.Document;
import com.google.gwt.xml.client.NamedNodeMap;
import com.google.gwt.xml.client.Node;
import com.google.gwt.xml.client.NodeList;
import com.google.gwt.xml.client.XMLParser;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
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
    private static final String URL_CATALOG = "http://localhost:8080/workflow-catalog";
    private static final String URL_CATALOG_BUCKETS = URL_CATALOG + "/buckets";
    private static final String ISO_8601_FORMAT = "yyyy-MM-dd'T'HH:mm:ssZZZ";
    private static final String URL_SUBMIT_XML = GWT.getModuleBaseURL() + "submitedit";
    private static final String URL_UPLOAD_FILE = GWT.getModuleBaseURL() + "uploader";

    private Window window;

    private SchedulerController controller;

    private HandlerRegistration todayClickHR = null;
    private HandlerRegistration changedHourHR = null;
    private HandlerRegistration changedMinuteHR = null;
    private HandlerRegistration gridClickHR = null;

    private VLayout rootPage; // ------------------------ the main layout of the window

    private VLayout selectWfLayout; // ----------------- Select Workflow Panel
    private VLayout fromFilePanel; // ------------------- The panel to select wf from disk
    private FileUpload fileUpload; // ------------------- FileUpload button
    private VerticalPanel selectWorkflowButtonsPanel; //  Panel that holds the strategic items to get a wf
    private Button sendFromFileButton; // --------------- Send the file to servlet from disk
    private HorizontalPanel fromCatalogPanel; // -------- The panel to select wf from catalog
    private ListBox bucketsListBox; // ------------------ Buckets dropdown list
    private ListBox workflowsListBox; // ---------------- Workflows dropdown list
    private Button sendFromCatalogButton; // ------------ Send the file to servlet from catalog

    // -------------------------------------------------- Variables Part
    private VLayout varsLayout; // --------------------- Variables Panel
    private VerticalPanel hiddenPane; // ---------------- Holds the parameters to submit along with the job
    private FormItem[] fields; // ----------------------- (visual) Variables to submit along with the job
    private Hidden[] _fields; // ------------------------ (hidden) Variables to submit along with the job
    private Hidden startAtParameter; // ----------------- START_AT value to send along with the job
    private FormPanel variablesActualForm; // ----------- Actual form to send

    // -------------------------------------------------- Start At Part
    private VLayout startAtLayout; // ------------------ Start At Panel
    private VerticalPanel startAtRadioGroupPanel; // ---- Now or Later
    private RadioButton startNowRB; // ------------------ As soon as possible radio button
    private RadioButton startAtRB; // ------------------- at scheduled time radio button
    private DateChooser dateChooser; // ----------------- DateChooser

    private HLayout submitCancelButtons; // ---------- -- Cancel and Submit buttons
    private IButton submitButton;

    private VLayout loadingPanel; // -------------------- Loading Panel when uploading

    // ----- Catalog temp maps
    private HashMap<String, Integer> catalogBucketsMap;
    private HashMap<String, Integer> catalogWorkflowsMap;


    private static final int width = 420;
    private static final int height = 500;

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
     * Shows the window created by the constructor,
     * calling this after destroy will throw an NPE,
     * you must create a new SubmitWindow for each job submission
     */
    public void show() {
        this.window.show();
    }

    /**
     * Destroy the window, you may null the reference after this as it will not be usable again
     */
    public void destroy() {
        this.window.destroy();
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

        // put the buttons into a single panel for easy spacing later on
        selectWorkflowButtonsPanel = new VerticalPanel();
        selectWorkflowButtonsPanel.setSpacing(5);
        selectWorkflowButtonsPanel.setHeight("50px");

        final RadioButton importFromFileRadioButton = new RadioButton("selectWfRadioGroup", "Import xml file");
        final RadioButton importFromCatalogRadioButton = new RadioButton("selectWfRadioGroup", "From catalog");
        importFromFileRadioButton.setValue(true);
        importFromCatalogRadioButton.setValue(false);

        importFromFileRadioButton.addClickHandler(new com.google.gwt.event.dom.client.ClickHandler() {
            @Override
            public void onClick(com.google.gwt.event.dom.client.ClickEvent event) {
                varsLayout.removeMembers(varsLayout.getMembers());
                selectWorkflowButtonsPanel.clear();
                initSelectWorkflowFromFilePanel();
                selectWorkflowButtonsPanel.add(fromFilePanel);
                selectWorkflowButtonsPanel.add(sendFromFileButton);
                rootPage.removeMember(startAtLayout);
                rootPage.removeMember(submitCancelButtons);
                initSubmitAtPart();
                rootPage.addMember(submitCancelButtons);
                startNowRB.setValue(true);
                startAtRB.setValue(false);
                setEnabledStartAtPart(false);
            }
        });

        importFromCatalogRadioButton.addClickHandler(new com.google.gwt.event.dom.client.ClickHandler() {
            @Override
            public void onClick(com.google.gwt.event.dom.client.ClickEvent event) {
                varsLayout.removeMembers(varsLayout.getMembers());
                selectWorkflowButtonsPanel.clear();
                initSelectWorkflowFromCatalogPanel();
                selectWorkflowButtonsPanel.add(fromCatalogPanel);
                selectWorkflowButtonsPanel.add(sendFromCatalogButton);
                rootPage.removeMember(startAtLayout);
                rootPage.removeMember(submitCancelButtons);
                initSubmitAtPart();
                rootPage.addMember(submitCancelButtons);
                startNowRB.setValue(true);
                startAtRB.setValue(false);
                setEnabledStartAtPart(false);
            }
        });

        final VerticalPanel selectWfRadioGroupPanel = new VerticalPanel();
        selectWfRadioGroupPanel.add(importFromFileRadioButton);
        selectWfRadioGroupPanel.add(importFromCatalogRadioButton);
        selectWfRadioGroupPanel.setSpacing(5);
        selectWfRadioGroupPanel.setHeight("50px");

        // init both panels (select from disk or from catalog)
        initSelectWorkflowFromFilePanel();
        initSelectWorkflowFromCatalogPanel();

        // by default we select from disk
        selectWorkflowButtonsPanel.add(fromFilePanel);
        selectWorkflowButtonsPanel.add(sendFromFileButton);

        selectWfLayout.addMember(selectWfRadioGroupPanel);
        selectWfLayout.addMember(selectWorkflowButtonsPanel);

        rootPage.addMember(selectWfLayout);
    }

    private void initVarsPart(Widget varsContent, Widget hiddenContent) {
        varsLayout = new VLayout();
        varsLayout.setIsGroup(true);
        varsLayout.setGroupTitle("2. Fill workflow variables");
        varsLayout.setWidth100();
        varsLayout.setHeight("100px");
        varsLayout.setMaxHeight(100);
        varsLayout.setPadding(5);
        varsLayout.setOverflow(Overflow.AUTO);
        varsLayout.addMember(varsContent);
        varsLayout.addMember(hiddenContent);
        rootPage.addMember(varsLayout);
        rootPage.reflow();
    }

    private void initVarsPart() {
        varsLayout = new VLayout();
        varsLayout.setIsGroup(true);
        varsLayout.setGroupTitle("2. Fill workflow variables");
        varsLayout.setWidth100();
        varsLayout.setHeight("100px");
        varsLayout.setMaxHeight(100);
        varsLayout.setPadding(5);
        varsLayout.setOverflow(Overflow.AUTO);
        rootPage.addMember(varsLayout);
        rootPage.reflow();
    }

    private void initSubmitAtPart() {
        startAtLayout = new VLayout();
        startAtLayout.setIsGroup(true);
        startAtLayout.setGroupTitle("3. Scheduled time");

        startAtParameter = new Hidden("START_AT");

        startNowRB = new RadioButton("startAtRadioGroup", "As soon as possible");
        startAtRB = new RadioButton("startAtRadioGroup", "At");
        startNowRB.setValue(true);
        startAtRB.setValue(false);

        startNowRB.addClickHandler(new com.google.gwt.event.dom.client.ClickHandler() {
            @Override
            public void onClick(com.google.gwt.event.dom.client.ClickEvent event) {
                startAtRB.setText("At");
                startAtLayout.removeMember(dateChooser);
            }
        });

        startAtRB.addClickHandler(new com.google.gwt.event.dom.client.ClickHandler() {
            @Override
            public void onClick(com.google.gwt.event.dom.client.ClickEvent event) {
                startAtLayout.addMember(dateChooser);
                DateTimeFormat dateTimeFormat =
                        DateTimeFormat.getFormat(ISO_8601_FORMAT);
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
        startAtRadioGroupPanel.add(startNowRB);
        startAtRadioGroupPanel.add(startAtRB);
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
        startAtRB.setText("At " + updatedDate.toString());
    }

    private void updateScheduledTimeForToday() {
        Date newDate = new Date();
        dateChooser.setData(newDate);
        dateChooser.getTimeItem().setHours(Integer.parseInt(DateTimeFormat.getFormat("HH").format(newDate)));
        dateChooser.getTimeItem().setMinutes(Integer.parseInt(DateTimeFormat.getFormat("mm").format(newDate)));
        startAtRB.setText("At " + newDate.toString());
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
        submitCancelButtons.setMembers(cancelButton, submitButton);
        rootPage.addMember(submitCancelButtons);
    }

    private void displayLoadingMessage() {

        loadingPanel = new VLayout();

        final Label waitLabel = new Label("Please wait...");
        waitLabel.setHeight(30);
        waitLabel.setIcon("loading.gif");
        waitLabel.setWidth100();
        waitLabel.setAlign(Alignment.CENTER);

        loadingPanel.addMember(waitLabel);
        rootPage.removeMember(varsLayout);
        rootPage.removeMember(startAtLayout);
        rootPage.removeMember(submitCancelButtons);
        rootPage.addMember(loadingPanel);
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
        hiddenField.setName("sessionId");
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
                if (CATALOG_SELECT_BUCKET.compareTo(selectedBucket) != 0) {
                    String workflowUrl = URL_CATALOG_BUCKETS + "/" + catalogBucketsMap.get(selectedBucket) + "/workflows";
                    RequestBuilder req = new RequestBuilder(RequestBuilder.GET, workflowUrl);
                    req.setCallback(new RequestCallback() {
                        @Override
                        public void onResponseReceived(Request request, Response response) {
                            JSONObject jsonObjectResponse = JSONParser.parseStrict(response.getText()).isObject();
                            JSONArray workflows = jsonObjectResponse.get("_embedded")
                                    .isObject().get("workflowMetadataList").isArray();
                            int workflowsSize = workflows.size();
                            catalogWorkflowsMap = new HashMap<>(workflowsSize);
                            workflowsListBox.setEnabled(false);
                            workflowsListBox.clear();
                            workflowsListBox.addItem(CATALOG_SELECT_WF);
                            for (int i = 0; i < workflowsSize; i++) {
                                JSONObject workflow = workflows.get(i).isObject();
                                String workflowName = workflow.get("name").isString().stringValue();
                                String workflowId = workflow.get("id").isNumber().toString();
                                String dropdownListItemLabel = workflowName + " (" + workflowId + ")";
                                workflowsListBox.addItem(dropdownListItemLabel);
                                catalogWorkflowsMap.put(dropdownListItemLabel, Integer.parseInt(workflowId));
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


        RequestBuilder req = new RequestBuilder(RequestBuilder.GET, URL_CATALOG + "/buckets");
        req.setCallback(new RequestCallback() {
            @Override
            public void onResponseReceived(Request request, Response response) {
                JSONObject jsonObjectResponse = JSONParser.parseStrict(response.getText()).isObject();
                JSONArray buckets = jsonObjectResponse.get("_embedded").isObject().get("bucketMetadataList").isArray();
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
        formContent.add(new Hidden("sessionId", LoginModel.getInstance().getSessionId()));

        final FormPanel importFromCatalogformPanel = new FormPanel();
        importFromCatalogformPanel.setEncoding(FormPanel.ENCODING_MULTIPART);
        importFromCatalogformPanel.setMethod(FormPanel.METHOD_POST);
        importFromCatalogformPanel.setAction(URL_UPLOAD_FILE);
        importFromCatalogformPanel.add(formContent);
        importFromCatalogformPanel.addSubmitCompleteHandler(fileUploadCompleteHandler());
        importFromCatalogformPanel.setHeight("30px");
        fromCatalogPanel.add(importFromCatalogformPanel);

        sendFromCatalogButton = new Button("Upload file");
        sendFromCatalogButton.addClickHandler(
                clickHandlerForUploadFromCatalogButton(formContent, importFromCatalogformPanel));
    }

    private ClickHandler clickHandlerForSubmitButton() {
        return new ClickHandler() {
            @Override
            public void onClick(ClickEvent event) {
                for (int i = 0; i < fields.length; i++) {
                    String val = "";
                    if (fields[i].getValue() != null) {
                        val = fields[i].getValue().toString();
                    }
                    _fields[i].setValue(val);
                }
                if (startAtRB.getValue()) {
                    DateTimeFormat dateTimeFormat =
                            DateTimeFormat.getFormat(ISO_8601_FORMAT);
                    String iso8601DateStr = dateTimeFormat.format(dateChooser.getData());
                    startAtParameter.setValue(iso8601DateStr);
                    hiddenPane.add(startAtParameter);
                }
                variablesActualForm.addSubmitCompleteHandler(new SubmitCompleteHandler() {
                    @Override
                    public void onSubmitComplete(SubmitCompleteEvent event) {
                        GWT.log("Job submitted to the scheduler");
                    }
                });
                variablesActualForm.submit();
                displayLoadingMessage();
                SubmitWindow.this.window.removeMember(rootPage);
                SubmitWindow.this.window.hide();
                SubmitWindow.this.destroy();
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
                    String selectedWorkflowId = String.valueOf(catalogWorkflowsMap.get(selectedWorkflowLabel));
                    formContent.add(new Hidden("bucketId", selectedBucketId));
                    formContent.add(new Hidden("workflowId", selectedWorkflowId));
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
                DynamicForm variablesVisualForm = null;
                Layout fpanelWra = null;
                variablesActualForm = null;

                try {
                    JSONValue js = JSONParser.parseStrict(res);
                    JSONObject obj = js.isObject();

                    if (obj.get("jobEdit") != null && obj.get("jobEdit").isString() != null) {
                        String val = obj.get("jobEdit").isString().stringValue();
                        String job = new String(org.ow2.proactive_grid_cloud_portal.common.shared.Base64Utils
                                .fromBase64(val));
                        final Map<String, String> variables = readVars(job);

                        // presentation form
                        variablesVisualForm = new DynamicForm();
                        fields = new FormItem[variables.size()];
                        _fields = new Hidden[variables.size()];
                        int i = 0;
                        hiddenPane = new VerticalPanel();
                        for (Entry<String, String> var : variables.entrySet()) {
                            TextItem t = new TextItem(var.getKey(), var.getKey());
                            t.setValue(var.getValue());
                            t.setWidth(240);

                            _fields[i] = new Hidden("var_" + var.getKey());
                            hiddenPane.add(_fields[i]);
                            fields[i] = t;
                            i++;
                        }
                        variablesVisualForm.setFields(fields);

                        // actual form used to POST
                        variablesActualForm = new FormPanel();
                        variablesActualForm.setMethod(FormPanel.METHOD_POST);
                        variablesActualForm.setAction(URL_SUBMIT_XML);
                        hiddenPane.add(new Hidden("job", job));
                        hiddenPane.add(new Hidden("sessionId", LoginModel.getInstance().getSessionId()));
                        variablesActualForm.setWidget(hiddenPane);
                        fpanelWra = new Layout();
                        fpanelWra.addMember(variablesActualForm);
                    }
                } catch (JSONException t) {
                    GWT.log("JSON parse ERROR");
                }
                setEnabledStartAtPart(true);
                rootPage.removeMember(loadingPanel);
                initVarsPart(variablesVisualForm, fpanelWra);
                rootPage.addMember(startAtLayout);
                rootPage.addMember(submitCancelButtons);
                rootPage.reflow();
            }
        };
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
                startAtRB.setText("At " + dateChooser.getData().toString());
                resetAllHandlers();
            }
        };
    }

    private void setEnabledStartAtPart(boolean state) {
        startNowRB.setEnabled(state);
        startAtRB.setEnabled(state);
        submitButton.setDisabled(!state);
        if (!state) {
            submitButton.setTooltip("A workflow must be selected first");
        }
        else {
            submitButton.setTooltip("");
        }
    }

    private void build() {
        initRootPage(); // ------------ root page of the window
        initSelectWfPart(); // -------- Select workflow Panel
        initVarsPart(); // ------------ Fill workflow variables Panel
        initSubmitAtPart(); // -------- Submit at given time Panel
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
     * @param jobDescriptor an XML job descriptor as a string
     * @return the name/value of all <variables><variable name value> elements
     */
    private Map<String, String> readVars(String jobDescriptor) {
        /* this will fail if someday the XML schema gets another <variable> tag elsewhere */

        Document dom = XMLParser.parse(jobDescriptor);
        NodeList variables = dom.getElementsByTagName("variable");
        Map<String, String> ret = new HashMap<String, String>();

        if (variables.getLength() > 0) {
            for (int i = 0; i < variables.getLength(); i++) {
                Node n = variables.item(i);

                if (n != null) {
                    NamedNodeMap attrs = n.getAttributes();
                    try {
                        if (attrs != null && n.hasAttributes()) {
                            String name = null;
                            String value = null;
                            for (int j = 0; j < attrs.getLength(); j++) {
                                Node attr = attrs.item(j);
                                if (attr.getNodeName().equals("name")) {
                                    name = attr.getNodeValue();
                                }
                                if (attr.getNodeName().equals("value")) {
                                    value = attr.getNodeValue();
                                }
                            }
                            if (name != null && value != null) {
                                if (!name.matches("[A-Za-z0-9._]+")) {
                                    // this won't necessarily be a problem at job submission,
                                    // but it definitely will be here in the client; don't bother
                                    continue;
                                }

                                ret.put(name, value);
                            }
                        }
                    } catch (JavaScriptException t) {
                        // Node.hasAttributes() throws if there are no attributes... (GWT 2.1.0)
                    }
                }
            }
        }
        return ret;
    }
}
