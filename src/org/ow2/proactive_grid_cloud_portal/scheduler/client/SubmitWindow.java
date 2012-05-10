/*
 * ################################################################
 *
 * ProActive Parallel Suite(TM): The Java(TM) library for
 *    Parallel, Distributed, Multi-Core Computing for
 *    Enterprise Grids & Clouds
 *
 * Copyright (C) 1997-2011 INRIA/University of
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

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import org.ow2.proactive_grid_cloud_portal.common.client.Controller;
import org.ow2.proactive_grid_cloud_portal.common.client.Images;
import org.ow2.proactive_grid_cloud_portal.scheduler.server.SubmitEditServlet;
import org.ow2.proactive_grid_cloud_portal.scheduler.server.UploadServlet;

import com.google.gwt.core.client.GWT;
import com.google.gwt.core.client.JavaScriptException;
import com.google.gwt.json.client.JSONException;
import com.google.gwt.json.client.JSONObject;
import com.google.gwt.json.client.JSONParser;
import com.google.gwt.json.client.JSONValue;
import com.google.gwt.user.client.ui.FileUpload;
import com.google.gwt.user.client.ui.FormPanel;
import com.google.gwt.user.client.ui.FormPanel.SubmitCompleteEvent;
import com.google.gwt.user.client.ui.FormPanel.SubmitCompleteHandler;
import com.google.gwt.user.client.ui.Hidden;
import com.google.gwt.user.client.ui.VerticalPanel;
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
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;


/**
 * Popup Window for job submission
 * 
 * 
 * @author mschnoor
 *
 */
public class SubmitWindow {

    private Window window;

    private SchedulerController controller;

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

    /**
     * internal layout creation
     * 
     * <pre>
     * +- Window ------------------+
     * |+- VLayout ---------------+|
     * ||+- Label --------+       || <-- error messages
     * ||+----------------+       ||     when applicable
     * ||+- Layout --------------+||
     * |||+- FormPanel ---------+|||
     * ||||+- VerticalPanel ---+||||
     * ||||| form fields       ||||| <-- GWT form wrapped
     * ||||+-------------------+||||     in SmartGWT layout
     * |||+---------------------+|||
     * ||+-----------------------+||
     * ||+- DynamicForm ---------+||     SmartGWT form, check
     * ||| form fields           ||| <-- to enable variable edition 
     * ||+-----------------------+||
     * ||           +- IButton --+|| <-- submit button
     * ||           +------------+||
     * |+-------------------------+|
     * +---------------------------+
     * </pre>
     * 
     * If the <code>Edit variables</code> checkbox is checked,
     * the {@link UploadServlet} called by the GWT form will return the content
     * of the job descriptor, and we will create a new form to edit the
     * variables so that we may submit the job to a second servlet, {@link SubmitEditServlet}.
     * If the {@link SubmitEditServlet} submission fails, we get back in the same state
     * as before the first click to Submit
     * 
     * 
     */
    private void build() {

        /* mixing GWT's native FormPanel with SmartGWT containers,
         * because SmartGWT's form somehow sucks when not using the datasource stuff
         * as a result the layout is a bit messy */

        // root page of the window
        final VLayout layout = new VLayout();
        layout.setMargin(10);
        layout.setWidth100();
        layout.setHeight100();

        // buttons 
        final HLayout buttons = new HLayout();
        buttons.setMembersMargin(5);
        buttons.setHeight(20);
        buttons.setWidth100();
        buttons.setAlign(Alignment.RIGHT);

        final IButton uploadButton = new IButton("Submit");
        uploadButton.setIcon(Images.instance.ok_16().getSafeUri().asString());

        final IButton cancelButton = new IButton("Cancel");
        cancelButton.setIcon(Images.instance.cancel_16().getSafeUri().asString());
        cancelButton.addClickHandler(new ClickHandler() {
            @Override
            public void onClick(ClickEvent event) {
                SubmitWindow.this.window.hide();
                SubmitWindow.this.destroy();
            }
        });

        buttons.setMembers(uploadButton, cancelButton);

        // holds the form fields
        VerticalPanel formContent = new VerticalPanel();
        Hidden hiddenField = new Hidden();
        hiddenField.setName("sessionId");
        hiddenField.setValue(this.controller.getModel().getSessionId());
        formContent.add(hiddenField);

        final FileUpload fileUpload = new FileUpload();
        fileUpload.setName("job");
        final Hidden editField = new Hidden("edit");
        editField.setValue("0");

        formContent.add(fileUpload);
        formContent.add(editField);

        // actual form		
        final FormPanel formPanel = new FormPanel();
        formPanel.setEncoding(FormPanel.ENCODING_MULTIPART);
        formPanel.setMethod(FormPanel.METHOD_POST);
        formPanel.setAction(GWT.getModuleBaseURL() + "uploader");
        formPanel.add(formContent);
        formPanel.setWidth("350px");
        formPanel.setHeight("30px");

        // wraps the GWT component so that we may show/hide it
        final HLayout formWrapper = new HLayout();
        formWrapper.setAlign(Alignment.CENTER);
        formWrapper.setHeight(30);
        formWrapper.addChild(formPanel);

        // error messages when applicable
        final Label label = new Label("Submit an XML Job Descriptor or a Job Archive:");
        label.setHeight(30);
        label.setWidth100();

        // shown during submission
        final Label waitLabel = new Label("Please wait...");
        waitLabel.setHeight(30);
        waitLabel.setIcon("loading.gif");
        waitLabel.setWidth100();
        waitLabel.setAlign(Alignment.CENTER);

        final CheckboxItem edit = new CheckboxItem("edit", "Edit variables definitions");
        final DynamicForm editForm = new DynamicForm();
        editForm.setHeight100();
        editForm.setColWidths(20, "*");
        editForm.setFields(edit);

        layout.addMember(label);
        layout.addMember(formWrapper);
        layout.addMember(editForm);
        layout.addMember(buttons);

        this.window = new Window();
        this.window.setTitle("Submit Job");
        this.window.setShowMinimizeButton(false);
        this.window.setIsModal(true);
        this.window.setShowModalMask(true);
        this.window.addItem(layout);
        this.window.setWidth(420);
        this.window.setHeight(180);
        this.window.centerInPage();
        this.window.setCanDragResize(true);

        // click the upload button :
        // hide the form, show a 'please wait' label,
        // wait for the form's callback
        uploadButton.addClickHandler(new com.smartgwt.client.widgets.events.ClickHandler() {
            public void onClick(ClickEvent e) {
                editField.setValue(edit.getValueAsBoolean() ? "1" : "0");

                formPanel.submit();

                layout.removeMember(label);
                layout.removeMember(formWrapper);
                layout.removeMember(editForm);
                layout.removeMember(buttons);

                layout.addMember(waitLabel);
            }
        });

        // form callback : silently close the window if no error,
        // else display message and allow new submission
        formPanel.addSubmitCompleteHandler(new SubmitCompleteHandler() {

            public void onSubmitComplete(SubmitCompleteEvent event) {
                String fn = fileUpload.getFilename();

                // chrome workaround
                final String fileName = fn.replace("C:\\fakepath\\", "");
                String res = event.getResults();
                boolean isError = false;

                try {
                    JSONValue js = JSONParser.parseStrict(res);
                    JSONObject obj = js.isObject();
                    /* 
                     * submission with no edition successful, result is the job id 
                     */
                    if (obj.get("id") != null && obj.get("id").isNumber() != null) {
                        int id = (int) obj.get("id").isNumber().doubleValue();
                        SubmitWindow.this.destroy();
                        controller.getModel()
                                .logMessage("Successfully submitted job " + fileName + ": " + id);
                        controller.addSubmittingJob(id, fileName);
                    }
                    /*
                     *  submission with edition:
                     */
                    else if (obj.get("jobEdit") != null && obj.get("jobEdit").isString() != null) {
                        String val = obj.get("jobEdit").isString().stringValue();
                        String job = new String(org.ow2.proactive_grid_cloud_portal.common.shared.Base64Utils
                                .fromBase64(val));
                        final Map<String, String> variables = readVars(job);

                        // presentation form
                        final DynamicForm varForm = new DynamicForm();
                        final FormItem[] fields = new FormItem[variables.size()];
                        final Hidden[] _fields = new Hidden[variables.size()];
                        int i = 0;
                        final VerticalPanel hiddenPane = new VerticalPanel();
                        for (Entry<String, String> var : variables.entrySet()) {
                            TextItem t = new TextItem(var.getKey(), var.getKey());
                            t.setValue(var.getValue());
                            t.setWidth(240);

                            _fields[i] = new Hidden("var_" + var.getKey());
                            hiddenPane.add(_fields[i]);
                            fields[i] = t;
                            i++;
                        }
                        varForm.setFields(fields);
                        varForm.setWidth100();
                        varForm.setHeight100();

                        // actual form used to POST
                        final FormPanel fpanel = new FormPanel();
                        fpanel.setMethod(FormPanel.METHOD_POST);
                        fpanel.setAction(GWT.getModuleBaseURL() + "submitedit");
                        hiddenPane.add(new Hidden("job", job));
                        hiddenPane.add(new Hidden("sessionId", controller.getModel().getSessionId()));
                        fpanel.setWidget(hiddenPane);
                        final Layout fpanelWrapper = new Layout();
                        fpanelWrapper.addMember(fpanel);

                        label.setContents("Edit the variable definitions for job <strong>" + fileName +
                            "</strong>");

                        final HLayout buttons2 = new HLayout();
                        buttons2.setWidth100();
                        buttons2.setHeight(20);
                        buttons2.setAlign(Alignment.RIGHT);
                        buttons2.setMembersMargin(5);
                        final IButton reset = new IButton("Reset");
                        reset.setIcon(Images.instance.clear_16().getSafeUri().asString());
                        reset.addClickHandler(new ClickHandler() {
                            public void onClick(ClickEvent event) {
                                for (FormItem it : fields) {
                                    String key = it.getName();
                                    String val = variables.get(key);
                                    it.setValue(val);
                                }
                            }
                        });
                        final IButton submit2 = new IButton("Submit");
                        submit2.setIcon(Images.instance.ok_16().getSafeUri().asString());
                        submit2.addClickHandler(new ClickHandler() {
                            public void onClick(ClickEvent event) {
                                for (int i = 0; i < fields.length; i++) {
                                    String val = "";
                                    if (fields[i].getValue() != null) {
                                        val = fields[i].getValue().toString();
                                    }
                                    _fields[i].setValue(val);
                                }

                                fpanel.submit();

                                layout.removeMember(label);
                                layout.removeMember(varForm);
                                layout.removeMember(buttons2);
                                layout.removeMember(fpanelWrapper);

                                layout.addMember(waitLabel);
                                layout.reflow();
                            }
                        });
                        final IButton cancel2 = new IButton("Cancel");
                        cancel2.setIcon(Images.instance.cancel_16().getSafeUri().asString());
                        cancel2.addClickHandler(new ClickHandler() {
                            @Override
                            public void onClick(ClickEvent event) {
                                SubmitWindow.this.window.hide();
                                SubmitWindow.this.destroy();
                            }
                        });

                        buttons2.setMembers(reset, submit2, cancel2);

                        fpanel.addSubmitCompleteHandler(new SubmitCompleteHandler() {
                            public void onSubmitComplete(SubmitCompleteEvent event) {
                                String res = event.getResults();
                                boolean failure = false;
                                try {
                                    JSONValue val = JSONParser.parseStrict(res);
                                    if (val.isObject() != null && val.isObject().containsKey("id")) {
                                        int id = (int) val.isObject().get("id").isNumber().doubleValue();
                                        SubmitWindow.this.destroy();
                                        controller.getModel().logMessage(
                                                "Successfully submitted job " + fileName + ": " + id);
                                        controller.addSubmittingJob(id, fileName);
                                    } else {
                                        failure = true;
                                    }
                                } catch (JSONException e) {
                                    failure = true;
                                }

                                if (failure) {
                                    String msg = Controller.getJsonErrorMessage(res);
                                    layout.removeMember(waitLabel);

                                    label
                                            .setContents("<span style='color:red; font-weight:bold'>Job submission failed:</span><br>" +
                                                "<span style=''>" + msg + "</span>");

                                    layout.addMember(label);
                                    layout.addMember(formWrapper);
                                    layout.addMember(editForm);
                                    layout.addMember(buttons);
                                    layout.reflow();
                                    controller.getModel().logImportantMessage("Failed to submit job: " + msg);
                                }
                            }
                        });

                        layout.removeMember(waitLabel);
                        layout.addMember(label);
                        layout.addMember(varForm);
                        layout.addMember(fpanelWrapper);
                        layout.addMember(buttons2);
                        layout.setMargin(10);
                        layout.reflow();

                    } else {
                        isError = true;
                    }
                } catch (JSONException t) {
                    isError = true;
                }

                /* 
                 * submission failure 
                 */
                if (isError) {
                    String msg = Controller.getJsonErrorMessage(res);
                    layout.removeMember(waitLabel);

                    label
                            .setContents("<span style='color:red; font-weight:bold'>Job submission failed:</span><br>" +
                                "<span style=''>" + msg + "</span>");

                    layout.addMember(label);
                    layout.addMember(formWrapper);
                    layout.addMember(editForm);
                    layout.addMember(buttons);
                    layout.reflow();
                    controller.getModel().logImportantMessage("Failed to submit job: " + msg);
                }
            }
        });
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
