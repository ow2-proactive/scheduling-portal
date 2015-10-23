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

import org.ow2.proactive_grid_cloud_portal.common.client.Images;
import org.ow2.proactive_grid_cloud_portal.common.client.JSUtil;
import org.ow2.proactive_grid_cloud_portal.common.client.json.JSONUtils;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LogModel;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LoginModel;

import com.google.gwt.core.client.GWT;
import com.google.gwt.core.client.JavaScriptObject;
import com.google.gwt.json.client.JSONObject;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.Encoding;
import com.smartgwt.client.types.FormMethod;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.HiddenItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.UploadItem;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;


/**
 * Flat job submission window
 * <p>
 * takes required parameters as input in a form,
 * uploads it on the corresponding servlet
 * 
 * 
 * @author mschnoor
 *
 */
public class FlatSubmitWindow {

    private Window window;

    private SchedulerController controller;

    public FlatSubmitWindow(SchedulerController controller) {
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

    private void build() {
        // root page of the window
        final VLayout layout = new VLayout();
        layout.setMargin(10);
        layout.setWidth100();
        layout.setHeight100();

        final Label label = new Label(
            "Submit file containing commands, each line will represent a single native task:");
        label.setHeight(30);
        label.setWidth100();

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
                window.hide();
                destroy();
            }
        });

        buttons.setMembers(uploadButton, cancelButton);

        final DynamicForm form = new DynamicForm();
        form.setHeight100();
        form.setEncoding(Encoding.MULTIPART);
        form.setMethod(FormMethod.POST);
        form.setAction(GWT.getModuleBaseURL() + "flatsubmit");
        form.setTarget("__hiddenFrame");

        HiddenItem sessionItem = new HiddenItem("sessionId");
        HiddenItem callback = new HiddenItem("flatCallback");
        TextItem nameItem = new TextItem("jobName", "Job name");
        //nameItem.setRequired(true);
        UploadItem commandsItem = new UploadItem("commandFile", "Command file");
        //commandsItem.setRequired(true);
        UploadItem selectionItem = new UploadItem("selectionScript", "Selection script");

        form.setItems(sessionItem, nameItem, commandsItem, selectionItem, callback);

        final Label waitLabel = new Label("Please wait...");
        waitLabel.setHeight100();
        waitLabel.setIcon("loading.gif");
        waitLabel.setWidth100();
        waitLabel.setAlign(Alignment.CENTER);

        layout.addMember(label);
        layout.addMember(form);
        layout.addMember(waitLabel);
        layout.addMember(buttons);

        layout.hideMember(waitLabel);

        uploadButton.addClickHandler(new ClickHandler() {
            @Override
            public void onClick(ClickEvent event) {
                form.setValue("sessionId", LoginModel.getInstance().getSessionId());
                form.setCanSubmit(true);

                form.setValue("flatCallback", JSUtil.register(new JSUtil.JSCallback() {
                    @Override
                    public void execute(JavaScriptObject obj) {
                        JSONObject js = new JSONObject(obj);

                        if (js.containsKey("id") && js.get("id").isNumber() != null) {
                            int id = (int) js.get("id").isNumber().doubleValue();
                            String name = form.getValueAsString("jobName");
                            FlatSubmitWindow.this.destroy();
                            LogModel.getInstance().logMessage(
                                    "Successfully submitted flat job " + name + ": " + id);
                            controller.getJobsController().addSubmittingJob(id, name);
                        } else {
                            String msg = JSONUtils.getJsonErrorMessage(js.toString());
                            label
                                    .setContents("<span style='color:red; font-weight:bold'>Job submission failed:</span><br>" +
                                        "<span style=''>" + msg + "</span>");
                            LogModel.getInstance().logImportantMessage("Failed to submit flat job: " + msg);

                            layout.hideMember(waitLabel);
                            layout.showMember(label);
                            layout.showMember(form);
                            buttons.showMember(uploadButton);
                        }
                    }
                }));

                layout.hideMember(label);
                layout.hideMember(form);
                layout.showMember(waitLabel);
                buttons.hideMember(uploadButton);

                form.submitForm();
            }
        });

        this.window = new Window();
        this.window.setTitle("Submit Command File Job");
        this.window.setShowMinimizeButton(false);
        this.window.setIsModal(true);
        this.window.setShowModalMask(true);
        this.window.addItem(layout);
        this.window.setWidth(420);
        this.window.setHeight(220);
        this.window.centerInPage();
        this.window.setCanDragResize(true);
    }
}
