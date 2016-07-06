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

import com.google.gwt.event.logical.shared.ValueChangeEvent;
import com.google.gwt.event.logical.shared.ValueChangeHandler;
import com.google.gwt.event.shared.HandlerRegistration;
import com.google.gwt.i18n.client.DateTimeFormat;
import com.google.gwt.user.client.ui.*;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.util.DateUtil;
import com.smartgwt.client.widgets.DateChooser;
import com.smartgwt.client.widgets.form.fields.*;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.grid.DateGrid;
import org.ow2.proactive_grid_cloud_portal.common.client.Images;
import org.ow2.proactive_grid_cloud_portal.common.client.json.JSONUtils;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LogModel;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LoginModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.server.SubmitEditServlet;
import org.ow2.proactive_grid_cloud_portal.scheduler.server.UploadServlet;

import com.google.gwt.core.client.GWT;
import com.google.gwt.core.client.JavaScriptException;
import com.google.gwt.json.client.JSONException;
import com.google.gwt.json.client.JSONObject;
import com.google.gwt.json.client.JSONParser;
import com.google.gwt.json.client.JSONValue;
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

    private static final String ISO_8601_FORMAT = "yyyy-MM-dd'T'HH:mm:ssZZZ";

    private Window window;

    private SchedulerController controller;

    private HandlerRegistration todayClickHR = null;
    private HandlerRegistration changedHourHR = null;
    private HandlerRegistration changedMinuteHR = null;
    private HandlerRegistration gridClickHR = null;

    private VLayout rootPage; // ------------------------ the main layout of the window

    private VLayout selectWfLayout; // ----------------- Select Workflow Panel
    private FileUpload fileUpload; // ------------------- FileUpload button

    // -------------------------------------------------- Variables Part
    private VLayout varsLayout; // ---------------------- Variables Panel
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

    private HLayout submitCancelButtons; // -------------------------- Cancel and Submit buttons

    private VLayout loadingPanel; // --------------------- Loading Panel when uploading


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

        final RadioButton importFromFileRadioButton = new RadioButton("selectWfRadioGroup", "Import xml file");
        final RadioButton importFromCatalogRadioButton = new RadioButton("selectWfRadioGroup", "From catalog");
        importFromFileRadioButton.setValue(true);
        importFromCatalogRadioButton.setValue(false);
        importFromCatalogRadioButton.setEnabled(false);
        final VerticalPanel selectWfRadioGroupPanel = new VerticalPanel();
        selectWfRadioGroupPanel.add(importFromFileRadioButton);
        selectWfRadioGroupPanel.add(importFromCatalogRadioButton);
        selectWfRadioGroupPanel.setSpacing(5);
        selectWfRadioGroupPanel.setHeight("50px");


        // TODO implement contextual zone for fetching from the catalog
        // contextual zone
        // - open file button
        // or
        // - dropdown list from bucket which workflow
        final VLayout fromFilePanel = new VLayout();
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
        importFromFileformPanel.setAction(GWT.getModuleBaseURL() + "uploader");
        importFromFileformPanel.add(formContent);
        importFromFileformPanel.addSubmitCompleteHandler(fileUploadCompleteHandler());
        importFromFileformPanel.setHeight("30px");
        fromFilePanel.addMember(importFromFileformPanel);

        final Button uploadButton = new Button("Upload file");
        uploadButton.addClickHandler(clickHandlerForUploadButton(importFromFileformPanel));

        // put the buttons into a single panel for easy spacing
        final VerticalPanel fromFileButtonsPanel = new VerticalPanel();
        fromFileButtonsPanel.setSpacing(5);
        fromFileButtonsPanel.setHeight("50px");
        fromFileButtonsPanel.add(fromFilePanel);
        fromFileButtonsPanel.add(uploadButton);

        selectWfLayout.addMember(selectWfRadioGroupPanel);
        selectWfLayout.addMember(fromFileButtonsPanel);

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

        final IButton submitButton = new IButton("Submit");
        submitButton.setIcon(Images.instance.ok_16().getSafeUri().asString());
        submitButton.addClickHandler(new ClickHandler() {
            @Override
            public void onClick(ClickEvent event) {
                for (int i = 0; i < fields.length; i++) {
                    String val = "";
                    if (fields[i].getValue() != null) {
                        val = fields[i].getValue().toString();
                    }
                    _fields[i].setValue(val);
                }
                DateTimeFormat dateTimeFormat =
                        DateTimeFormat.getFormat(ISO_8601_FORMAT);
                String iso8601DateStr = dateTimeFormat.format(dateChooser.getData());
                startAtParameter.setValue(iso8601DateStr);
                hiddenPane.add(startAtParameter);
                variablesActualForm.addSubmitCompleteHandler(new SubmitCompleteHandler() {
                    @Override
                    public void onSubmitComplete(SubmitCompleteEvent event) {
                        GWT.log("Job submitted to the scheduler");
                        GWT.log(event.getResults());
                    }
                });
                variablesActualForm.submit();
                displayLoadingMessage();
                SubmitWindow.this.window.removeMember(rootPage);
                SubmitWindow.this.window.hide();
                SubmitWindow.this.destroy();
            }
        });

        final IButton cancelButton = new IButton("Cancel");
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

    private com.google.gwt.event.dom.client.ClickHandler clickHandlerForUploadButton(final FormPanel toSubmit) {
        return new com.google.gwt.event.dom.client.ClickHandler() {
            @Override
            public void onClick(com.google.gwt.event.dom.client.ClickEvent event) {
                displayLoadingMessage();
                toSubmit.submit();
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
                        variablesActualForm.setAction(GWT.getModuleBaseURL() + "submitedit");
                        hiddenPane.add(new Hidden("job", job));
                        hiddenPane.add(new Hidden("sessionId", LoginModel.getInstance().getSessionId()));
                        variablesActualForm.setWidget(hiddenPane);
                        fpanelWra = new Layout();
                        fpanelWra.addMember(variablesActualForm);
                    }
                } catch (JSONException t) {
                    GWT.log("JSON parse ERROR");
                }

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
        changedMinuteHR = dateChooser.getTimeItem().getMinuteItem()
                .addChangedHandler(newCHForMinuteField());

        if (changedHourHR != null) {
            changedHourHR.removeHandler();
        }
        changedHourHR = dateChooser.getTimeItem().getHourItem()
                .addChangedHandler(newCHForHourField());

        if (todayClickHR != null) {
            todayClickHR.removeHandler();
        }
        todayClickHR = dateChooser.getTodayButton()
                .addClickHandler(newCHForTodayButton());

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

    private void build() {
        initRootPage(); // ------------ root page of the window
        initSelectWfPart(); // -------- Select workflow Panel
        initVarsPart(); // ------------ Fill workflow variables Panel
        initSubmitAtPart(); // -------- Submit at given time Panel
        initButtonsPart(); // --------- Close and Submit buttons

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
