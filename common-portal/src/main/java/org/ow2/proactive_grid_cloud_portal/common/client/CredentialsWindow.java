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
package org.ow2.proactive_grid_cloud_portal.common.client;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.ui.FileUpload;
import com.google.gwt.user.client.ui.FormPanel;
import com.google.gwt.user.client.ui.FormPanel.SubmitCompleteEvent;
import com.google.gwt.user.client.ui.FormPanel.SubmitCompleteHandler;
import com.google.gwt.user.client.ui.Hidden;
import com.google.gwt.user.client.ui.VerticalPanel;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.events.DrawEvent;
import com.smartgwt.client.widgets.events.DrawHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.PasswordItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.form.validator.CustomValidator;
import com.smartgwt.client.widgets.form.validator.Validator;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;


/**
 * Helper to create and download a Credential file for future authentication
 * 
 * 
 * @author mschnoor
 *
 */
public class CredentialsWindow {

    private Window window;

    private boolean disableFormWrapper = true;

    public CredentialsWindow() {
        this.build();
    }

    public void show() {
        this.window.show();
    }

    public void destroy() {
        this.window.destroy();
    }

    private void build() {
        /*
         * smartGWT forms don't allow simple multipart file upload,
         * so we use a smartGWT form for login/password/checkbox,
         * a pure GWT form for file upload, and upon submission,
         * put the fields from the first form as hidden fields of the
         * pure GWT form. It's a bit convoluted but like this we get
         * the pretty widgets and the nice features
         */

        TextItem loginField = new TextItem("login", "Login");
        loginField.setRequired(true);

        PasswordItem passwordField = new PasswordItem("password", "Password");
        passwordField.setRequired(true);

        final CheckboxItem moreField = new CheckboxItem("useSSH", "Use SSH private key");
        moreField.setValue(false);

        // smartGWT form: only used to input the data before filling the hidden fields
        // in the other form with it
        final DynamicForm form = new DynamicForm();
        form.setFields(loginField, passwordField, moreField);

        // pure GWT form for uploading, will be used to contact the servlet
        // even if no ssh key is used
        final FileUpload fileUpload = new FileUpload();
        fileUpload.setName("sshkey");
        final Hidden hiddenUser = new Hidden("username");
        final Hidden hiddenPass = new Hidden("password");
        final FormPanel formPanel = new FormPanel();
        formPanel.setEncoding(FormPanel.ENCODING_MULTIPART);
        formPanel.setMethod(FormPanel.METHOD_POST);
        formPanel.setAction(GWT.getModuleBaseURL() + "createcredential");
        final VerticalPanel vpan = new VerticalPanel();
        vpan.add(hiddenUser);
        vpan.add(hiddenPass);
        vpan.add(fileUpload);
        formPanel.setWidget(vpan);
        formPanel.setWidth("100%");
        formPanel.setHeight("30px");
        final HLayout formWrapper = new HLayout();
        formWrapper.setAlign(Alignment.CENTER);
        formWrapper.addChild(formPanel);
        formWrapper.setWidth100();
        formWrapper.addDrawHandler(new DrawHandler() {
            public void onDraw(DrawEvent event) {
                // took me half a day to find this hack:
                // if the form is added to the page in a hidden element,
                // it is never created and submission fails without callback.
                // it needs to be visible so that it is created once, then
                // we can safely hide it and still use it
                if (disableFormWrapper) {
                    disableFormWrapper = false;
                    formWrapper.setVisible(false);
                }
            }
        });

        // hide/show the ssh key upload input
        moreField.addChangedHandler(new ChangedHandler() {
            public void onChanged(ChangedEvent event) {
                if (moreField.getValueAsBoolean()) {
                    formWrapper.setVisible(true);
                } else {
                    formWrapper.setVisible(false);
                    formPanel.reset();
                }
            }
        });
        // prevent form validation if no ssh key is selected
        Validator moreVal = new CustomValidator() {
            @Override
            protected boolean condition(Object value) {
                if (moreField.getValueAsBoolean()) {
                    String file = fileUpload.getFilename();
                    return (file != null && file.length() > 0);
                } else {
                    return true;
                }
            }
        };
        moreVal.setErrorMessage("No file selected");
        moreField.setValidators(moreVal);

        final IButton clearButton = new IButton("Clear");
        clearButton.setIcon(Images.instance.clear_16().getSafeUri().asString());
        clearButton.addClickHandler(new ClickHandler() {
            public void onClick(ClickEvent event) {
                form.clearValues();
                formPanel.reset();
                formWrapper.setVisible(false);
            }
        });

        final IButton closeButton = new IButton("Close");

        final Label label = new Label("A Credential is a file containing all information used" +
                                      " for authentication, in an encrypted form. It allows easier authentication and" +
                                      " automation.");
        label.setHeight(50);

        final HLayout buttonBar = new HLayout();

        final IButton okButton = new IButton();
        okButton.setShowDisabled(false);
        okButton.setIcon(Images.instance.ok_16().getSafeUri().asString());
        okButton.setTitle("Create");
        okButton.addClickHandler(new ClickHandler() {
            public void onClick(ClickEvent event) {
                if (!form.validate())
                    return;

                String login = form.getValueAsString("login");
                String pw = form.getValueAsString("password");
                hiddenUser.setValue(login);
                hiddenPass.setValue(pw);

                formPanel.submit();
            }
        });

        closeButton.setIcon(Images.instance.cancel_16().getSafeUri().asString());
        closeButton.addClickHandler(new ClickHandler() {
            @Override
            public void onClick(ClickEvent event) {
                CredentialsWindow.this.window.hide();
                CredentialsWindow.this.destroy();
            }
        });

        formPanel.addSubmitCompleteHandler(new SubmitCompleteHandler() {
            public void onSubmitComplete(SubmitCompleteEvent event) {
                /*
                 * this happens only on error, if the call succeeds,
                 * the response is relocated so that a 'save file' dialog appears
                 */
                String str = event.getResults();
                label.setContents("<span style='color:red;'>" + str + "</span>");

            }
        });

        Layout formLayout = new VLayout();
        formLayout.setHeight100();
        formLayout.setWidth100();
        formLayout.setMembersMargin(10);
        formLayout.addMember(form);
        formLayout.addMember(formWrapper);

        buttonBar.setWidth100();
        buttonBar.setAlign(Alignment.RIGHT);
        buttonBar.setMembersMargin(5);
        buttonBar.setMembers(clearButton, okButton, closeButton);
        formLayout.addMember(buttonBar);

        VLayout layout = new VLayout();
        layout.setMembersMargin(10);
        layout.setMargin(5);
        layout.setMembers(label, formLayout, buttonBar);

        this.window = new Window();
        this.window.setTitle("Create Credentials");
        this.window.setShowMinimizeButton(false);
        this.window.setIsModal(true);
        this.window.setShowModalMask(true);
        this.window.addItem(layout);
        this.window.setWidth(370);
        this.window.setHeight(260);
        this.window.centerInPage();
    }
}
