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
package org.ow2.proactive_grid_cloud_portal.common.client;

import org.ow2.proactive_grid_cloud_portal.common.shared.Config;

import com.google.gwt.core.client.GWT;
import com.google.gwt.http.client.Request;
import com.google.gwt.http.client.RequestBuilder;
import com.google.gwt.http.client.RequestCallback;
import com.google.gwt.http.client.RequestException;
import com.google.gwt.http.client.Response;
import com.google.gwt.http.client.URL;
import com.google.gwt.json.client.JSONObject;
import com.google.gwt.json.client.JSONParser;
import com.google.gwt.json.client.JSONValue;
import com.google.gwt.user.client.ui.FileUpload;
import com.google.gwt.user.client.ui.FormPanel;
import com.google.gwt.user.client.ui.FormPanel.SubmitCompleteEvent;
import com.google.gwt.user.client.ui.FormPanel.SubmitCompleteHandler;
import com.google.gwt.user.client.ui.Hidden;
import com.google.gwt.user.client.ui.VerticalPanel;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.AnimationEffect;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Img;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.events.DrawEvent;
import com.smartgwt.client.widgets.events.DrawHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.events.ItemKeyPressEvent;
import com.smartgwt.client.widgets.form.events.ItemKeyPressHandler;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.PasswordItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.form.validator.CustomValidator;
import com.smartgwt.client.widgets.form.validator.Validator;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;


/**
 * Page shown when the user is not logged in
 * <p>
 * Allows plain and credentials authentication through a specific servlet
 * 
 * 
 * @author mschnoor
 *
 */
public class LoginPage {

	/** root layout: parent to all widgets of this view */
	private Layout layout = null;

	private Controller controller;

	/** credential/plain auth */
	private DynamicForm authTypeSelectForm = null;

	// some ui fields kept global for convenience
	private HLayout authSelLayout = null;
	private Label optsLabel = null;
	private Runnable switchPlainCredForm = null;
	private Label motdLabel = null;
	private Canvas motdMargin = null;
	private HLayout motdCanvas = null;

	/** true before the page is rendered for the first time,
	 * used to hack the apparition of the sshkey upload form */
	private boolean disableFormWrapper = true;

	/** displays error messages when login fails */
	private Label errorLabel = null;

	/**
	 * Default constructor
	 *
	 * @param controller Controller that created this page
	 * @param initialErrorMessage error message to display at the creation of the page, or null
	 */
	public LoginPage(Controller controller, String initialErrorMessage) {
		this.controller = controller;
		buildAndShow();

		if (initialErrorMessage != null && initialErrorMessage.length() > 0) {
			errorLabel.setContents("<span style='color:red;'>" + initialErrorMessage + "</span>");
			errorLabel.animateShow(AnimationEffect.FLY);
		}
	}

	/**
	 * Creates the layout and add it to the page
	 */
	private void buildAndShow() {

		Img logo = new Img(controller.getLogo350Url(), 350, 88);

		// contains the forms for authentication
		final VLayout auth = new VLayout();
		auth.setBackgroundColor("#f0f0f0");
		auth.setBorder("1px solid #ccc");
		auth.setWidth(350);
		auth.setHeight(140);
		auth.setAlign(VerticalAlignment.CENTER);
		auth.setPadding(10);

		authSelLayout = new HLayout();
		authSelLayout.setAlign(Alignment.CENTER);
		//authSel.setMargin(5);
		authSelLayout.setWidth100();
		authSelLayout.setHeight(20);

		final SelectItem sl = new SelectItem("Mode");
		sl.setValueMap("Basic", "Credentials");
		sl.setValue("Basic");
		authTypeSelectForm = new DynamicForm();
		authTypeSelectForm.setNumCols(1);
		authTypeSelectForm.setFields(sl);
		authSelLayout.addMember(authTypeSelectForm);
		authSelLayout.setVisible(false);

		optsLabel = new Label("<nobr style='color:#003168;font-size: 1.2em;cursor:pointer'>"
			+ "more options</nobr>");
		optsLabel.setHeight(30);
		optsLabel.setIcon(Images.instance.expand_16().getSafeUri().asString());
		optsLabel.setAlign(Alignment.RIGHT);
		optsLabel.setValign(VerticalAlignment.TOP);
		optsLabel.setWidth100();

		final Layout plainLayout = getPlainAuth();
		final Layout credLayout = getCredAuth();
		credLayout.hide();

		Label label = new Label("<nobr style='color:#003168;font-size:1.2em;'><strong>" +
			Config.get().getApplicationName() + "</strong> login</nobr>");
		label.setHeight(30);
		label.setValign(VerticalAlignment.TOP);

		Canvas filler = new Canvas();
		filler.setWidth100();

		HLayout labelLayout = new HLayout();
		labelLayout.setWidth100();
		labelLayout.setHeight(30);
		labelLayout.setMembers(label, optsLabel);

		motdCanvas = new HLayout();
		motdCanvas.setPadding(5);
		motdCanvas.setMembersMargin(5);
		//motdCanvas.setBackgroundColor("#ddd");

		Img img = new Img(Images.instance.info_32().getSafeUri().asString(), 32, 32);
		img.setLayoutAlign(Alignment.CENTER);

		motdLabel = new Label("wait");
		motdLabel.setBackgroundColor("#e8e8e8");
		motdLabel.setWidth100();

		motdCanvas.addMember(img);
		//motdCanvas.addMember(separator);
		motdCanvas.addMember(motdLabel);

		motdMargin = new Canvas();
		motdMargin.setHeight(10);

		motdCanvas.setVisible(false);
		motdMargin.setVisible(false);

		auth.addMember(motdCanvas);
		auth.addMember(motdMargin);

		String motdUrl = URL.encode(GWT.getModuleBaseURL() + "motd");
		RequestBuilder rqb = new RequestBuilder(RequestBuilder.GET, motdUrl);
		try {
			rqb.sendRequest(null, new RequestCallback() {
				@Override
				public void onResponseReceived(Request request, Response response) {
					String content = "";
					if (response.getStatusCode() == 200) {
						content = response.getText();
						if (content.trim().length() == 0) {
							return;
						}
					} else if (response.getStatusCode() == 0) {
						return;
					} else {
						content = "Failed to get MOTD (" + response.getStatusCode() + " " +
							response.getStatusText() + ")";
					}
					motdLabel
							.setContents("<div style='color:#003168;font-size: 1.2em;border-left:5px solid #ccc;padding-left:5px'>" +
								content + "</div>");
					motdCanvas.setVisible(true);
					motdMargin.setVisible(true);

				}

				@Override
				public void onError(Request request, Throwable exception) {
					System.out.println(exception);
				}
			});
		} catch (RequestException e) {
			// not much we can do here...
			// not really important enough to display a trace on screen
		}

		auth.addMember(labelLayout);
		auth.addMember(authSelLayout);
		auth.addMember(plainLayout);
		auth.addMember(credLayout);

		switchPlainCredForm = new Runnable() {
			@Override
			public void run() {
				if (sl.getValueAsString().equals("Basic")) {
					plainLayout.animateShow(AnimationEffect.FLY);
					credLayout.animateHide(AnimationEffect.FLY);
				} else {
					credLayout.animateShow(AnimationEffect.FLY);
					plainLayout.animateHide(AnimationEffect.FLY);
				}
			}
		};

		sl.addChangedHandler(new ChangedHandler() {
			public void onChanged(ChangedEvent event) {
				switchPlainCredForm.run();
			}
		});

		this.errorLabel = new Label();
		errorLabel.setBorder("1px solid red");
		errorLabel.setBackgroundColor("#fff0f0");
		//errorLabel.setMargin(5);
		errorLabel.setPadding(5);
		errorLabel.setHeight(30);
		errorLabel.setAlign(Alignment.CENTER);

		// contains the logo and the forms
		VLayout vlayout = new VLayout();
		vlayout.setMembersMargin(15);
		// 350(auth) + 2*10(padding) = 370
		vlayout.setWidth(370);
		// 140(auth) + 15(membersMargin) + 88(logo) + 2*10(padding) = 263
		vlayout.setHeight(263);
		vlayout.setPadding(10);
		vlayout.setBackgroundColor("#fafafa");
		vlayout.setBorder("1px solid #ccc");
		vlayout.setShowShadow(true);
		vlayout.setShadowSoftness(8);
		vlayout.setShadowOffset(3);

		vlayout.addMember(logo);
		vlayout.addMember(errorLabel);
		vlayout.addMember(auth);

		vlayout.hideMember(errorLabel);

		// dummy box for horizontal centering
		HLayout hlayout = new HLayout();
		hlayout.setWidth100();
		hlayout.setHeight(263);
		hlayout.setAlign(Alignment.CENTER);
		hlayout.addMember(vlayout);

		// dummy box for vertical centering
		this.layout = new VLayout();
		this.layout.setHeight100();
		this.layout.setWidth100();
		this.layout.setAlign(VerticalAlignment.CENTER);
		this.layout.addMember(hlayout);
		this.layout.draw();
	}

	/**
	 * @return the forms and widgets for plain login/password authentication
	 */
	private Layout getPlainAuth() {

		/* smartGWT forms don't allow simple multipart file upload,
		 * so we use a smartGWT form for login/password/checkbox,
		 * a pure GWT form for file upload, and upon submission,
		 * put the fields from the first form as hidden fields of the
		 * pure GWT form. It's a bit convoluted but like this we get
		 * the pretty widgets and the nice features		 */

		TextItem loginField = new TextItem("login", "User");
		loginField.setRequired(true);

		PasswordItem passwordField = new PasswordItem("password", "Password");
		passwordField.setRequired(true);

		final CheckboxItem moreField = new CheckboxItem("useSSH", "Use SSH private key");
		moreField.setValue(false);

		// smartGWT form: only used to input the data before filling the hidden fields
		// in the other form with it
		final DynamicForm form = new DynamicForm();
		form.setFields(loginField, passwordField, moreField);
		form.hideItem("useSSH");

		// pure GWT form for uploading, will be used to contact the servlet
		// even if no ssh key is used
		final FileUpload fileUpload = new FileUpload();
		fileUpload.setName("sshkey");
		final Hidden hiddenUser = new Hidden("username");
		final Hidden hiddenPass = new Hidden("password");
		final FormPanel formPanel = new FormPanel();
		formPanel.setEncoding(FormPanel.ENCODING_MULTIPART);
		formPanel.setMethod(FormPanel.METHOD_POST);
		formPanel.setAction(GWT.getModuleBaseURL() + "login");
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
					//formWrapper.setVisible(true);
					formWrapper.animateShow(AnimationEffect.FLY);
				} else {
					//formWrapper.setVisible(false);
					formWrapper.animateHide(AnimationEffect.FLY);
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

		final Runnable advancedVisibilityChanged = new Runnable() {
			@Override
			public void run() {
				if (!moreField.getVisible()) {
					authSelLayout.setVisible(true);
					form.showItem("useSSH");
					optsLabel.setIcon(Images.instance.close_16().getSafeUri().asString());
					optsLabel.setContents("<nobr style='color:#003168;font-size: 1.2em;"
						+ "cursor:pointer'>less options</nobr>");
				} else {
					authTypeSelectForm.setValue("Mode", "Basic");
					switchPlainCredForm.run();
					authSelLayout.setVisible(false);
					form.hideItem("useSSH");
					formWrapper.animateHide(AnimationEffect.FLY);
					moreField.setValue(false);
					formPanel.reset();
					optsLabel.setIcon(Images.instance.expand_16().getSafeUri().asString());
					optsLabel.setContents("<nobr style='color:#003168;font-size: 1.2em;"
						+ "cursor:pointer'>more options</nobr>");
				}
			}
		};
		optsLabel.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				advancedVisibilityChanged.run();
			}
		});

		String cacheLogin = Settings.get().getSetting(controller.getLoginSettingKey());
		if (cacheLogin != null) {
			form.setValue("login", cacheLogin);
		}

		final IButton okButton = new IButton();
		okButton.setShowDisabled(false);
		okButton.setIcon(Images.instance.connect_16().getSafeUri().asString());
		okButton.setTitle("Connect");
		okButton.setWidth(120);
		okButton.addClickHandler(new ClickHandler() {
			public void onClick(ClickEvent event) {
				errorLabel.animateHide(AnimationEffect.FLY);

				if (!form.validate())
					return;

				String login = form.getValueAsString("login");
				String pw = form.getValueAsString("password");
				hiddenUser.setValue(login);
				hiddenPass.setValue(pw);

				okButton.setIcon("loading.gif");
				okButton.setTitle("Connecting...");
				form.disable();
				formWrapper.disable();

				authTypeSelectForm.disable();
				okButton.disable();

				formPanel.submit();
			}
		});

		formPanel.addSubmitCompleteHandler(new SubmitCompleteHandler() {
			public void onSubmitComplete(SubmitCompleteEvent event) {
				String res = event.getResults();
				boolean fail = false;
				try {
					JSONValue val = JSONParser.parseStrict(res);
					JSONObject obj = val.isObject();
					if (obj != null && obj.containsKey("sessionId")) {
						String sess = obj.isObject().get("sessionId").isString().stringValue();
						controller.login(sess, form.getValueAsString("login"));
					} else {
						fail = true;
					}
				} catch (Throwable t) {
					fail = true;
				}

				if (fail) {
					String err = Controller.getJsonErrorMessage(res);
					int sta = Controller.getJsonErrorCode(res);
					if (sta != -1)
						err += " (" + sta + ")";
					errorLabel.setContents("<span style='color:red;'>Could not login: " + err + "</span>");
					errorLabel.animateShow(AnimationEffect.FLY);

					okButton.setIcon(Images.instance.connect_16().getSafeUri().asString());
					okButton.setTitle("Connect");
					formWrapper.enable();
					form.enable();
					authTypeSelectForm.enable();
					okButton.enable();
				}
			}
		});

		form.addItemKeyPressHandler(new ItemKeyPressHandler() {
			public void onItemKeyPress(ItemKeyPressEvent event) {
				if ("Enter".equals(event.getKeyName())) {
					okButton.fireEvent(new ClickEvent(null));
				}
			}
		});

		Layout formLayout = new VLayout();
		formLayout.setWidth100();
		formLayout.setMembersMargin(10);
		formLayout.addMember(form);
		formLayout.addMember(formWrapper);

		HLayout buttonBar = new HLayout();
		buttonBar.setWidth100();
		buttonBar.setAlign(Alignment.CENTER);
		buttonBar.addMember(okButton);
		formLayout.addMember(buttonBar);

		return formLayout;
	}

	/**
	 * @return the forms and widgets for credentials authentication
	 */
	private Layout getCredAuth() {
		final FileUpload fileUpload = new FileUpload();
		fileUpload.setName("credential");

		// actual form		
		final FormPanel formPanel = new FormPanel();
		formPanel.setEncoding(FormPanel.ENCODING_MULTIPART);
		formPanel.setMethod(FormPanel.METHOD_POST);
		formPanel.setAction(GWT.getModuleBaseURL() + "login");
		formPanel.add(fileUpload);
		formPanel.setWidth("100%");
		formPanel.setHeight("30px");

		// wraps the GWT component so that we may show/hide it
		final VLayout formWrapper = new VLayout();
		formWrapper.setAlign(Alignment.CENTER);
		formWrapper.addMember(formPanel);
		formWrapper.setWidth100();
		formWrapper.setMargin(10);

		final IButton okButton = new IButton();
		okButton.setShowDisabled(false);
		okButton.setIcon(Images.instance.connect_16().getSafeUri().asString());
		okButton.setTitle("Connect");
		okButton.setWidth(120);
		okButton.addClickHandler(new ClickHandler() {
			public void onClick(ClickEvent event) {
				errorLabel.animateHide(AnimationEffect.FLY);

				okButton.setIcon("loading.gif");
				okButton.setTitle("Connecting...");
				formWrapper.disable();
				authTypeSelectForm.disable();
				okButton.disable();

				// submits the form to LoginServlet
				formPanel.submit();
			}
		});

		formPanel.addSubmitCompleteHandler(new SubmitCompleteHandler() {
			public void onSubmitComplete(SubmitCompleteEvent event) {
				String res = event.getResults();
				boolean fail = false;
				try {
					JSONValue val = JSONParser.parseStrict(res);
					JSONObject obj = val.isObject();
					if (obj != null && obj.containsKey("sessionId")) {
						String sess = obj.isObject().get("sessionId").isString().stringValue();
						controller.login(sess, null);
					} else {
						fail = true;
					}
				} catch (Throwable t) {
					fail = true;
				}

				if (fail) {
					String err = Controller.getJsonErrorMessage(res);
					errorLabel.setContents("<span style='color:red;'>Could not login: " + err + "</span>");
					errorLabel.animateShow(AnimationEffect.FLY);

					okButton.setIcon(Images.instance.connect_16().getSafeUri().asString());
					okButton.setTitle("Connect");
					formWrapper.enable();
					authTypeSelectForm.enable();
					okButton.enable();
				}
			}
		});
		Label createCred = new Label("<nobr style='color:#003168;font-size: 1.2em;cursor:pointer'>"
			+ "Create credentials</nobr>");
		createCred.setHeight(20);
		createCred.setAlign(Alignment.CENTER);
		createCred.setIcon(Images.instance.key_16().getSafeUri().asString());
		createCred.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				CredentialsWindow win = new CredentialsWindow();
				win.show();
			}
		});

		formWrapper.addMember(createCred);

		Layout formLayout = new VLayout();
		formLayout.setWidth100();
		formLayout.setMembersMargin(10);
		formLayout.addMember(formWrapper);

		HLayout buttonBar = new HLayout();
		buttonBar.setWidth100();
		buttonBar.setAlign(Alignment.CENTER);
		buttonBar.addMember(okButton);
		formLayout.addMember(buttonBar);

		return formLayout;
	}

	/**
	 * Removes the layout and widgets from the page
	 * Call this when the view should be definitely removed and GC's, else just hide() it
	 */
	public void destroy() {
		this.layout.destroy();
		this.layout = null;
		this.controller = null;
	}
}
