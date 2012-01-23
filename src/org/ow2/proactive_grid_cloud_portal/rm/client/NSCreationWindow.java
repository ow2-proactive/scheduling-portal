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
package org.ow2.proactive_grid_cloud_portal.rm.client;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;

import org.ow2.proactive_grid_cloud_portal.common.client.CredentialsWindow;
import org.ow2.proactive_grid_cloud_portal.common.client.Images;
import org.ow2.proactive_grid_cloud_portal.common.client.JSUtil;
import org.ow2.proactive_grid_cloud_portal.rm.client.PluginDescriptor.Field;

import com.google.gwt.core.client.GWT;
import com.google.gwt.core.client.JavaScriptObject;
import com.google.gwt.json.client.JSONObject;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.Encoding;
import com.smartgwt.client.types.FormMethod;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.HiddenItem;
import com.smartgwt.client.widgets.form.fields.PasswordItem;
import com.smartgwt.client.widgets.form.fields.PickerIcon;
import com.smartgwt.client.widgets.form.fields.PickerIcon.Picker;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SpacerItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.UploadItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.form.fields.events.FormItemClickHandler;
import com.smartgwt.client.widgets.form.fields.events.FormItemIconClickEvent;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.layout.VStack;


/**
 * NodeSource creation dialog
 * <p>
 * dynamically downloads infrastructure and policy
 * info when created
 * 
 * 
 * @author mschnoor
 *
 */
public class NSCreationWindow {

	private RMController controller;
	private Window window;

	private SelectItem infraSelect, policySelect;

	private String oldInfra = null, oldPolicy = null;

	private Runnable policySelectChanged, infraSelectChanged;

	NSCreationWindow(RMController controller) {
		this.controller = controller;
		this.build();
	}

	public void show() {
		this.window.show();
	}

	public void destroy() {
		this.window.destroy();
	}

	private void build() {
		final VLayout layout = new VLayout();
		layout.setMargin(5);

		final VStack infraLayout = new VStack();
		infraLayout.setHeight(26);
		final Label infraLabel = new Label("Updating available Infrastructures and Policies");
		infraLabel.setIcon("loading.gif");
		infraLabel.setHeight(26);
		infraLabel.setAlign(Alignment.CENTER);
		infraLayout.addMember(infraLabel);

		final DynamicForm infraForm = new DynamicForm();
		infraForm.setEncoding(Encoding.MULTIPART);
		infraForm.setMethod(FormMethod.POST);
		infraForm.setAction(GWT.getModuleBaseURL() + "createnodesource");
		infraForm.setTarget("__hiddenFrame");

		infraLayout.addMember(infraForm);

		final Label label = new Label(
			"A Node Source is a combination of an Infrastructure, which defines how resources"
				+ " will be acquired, and a Policy, that dictates when resources can be acquired.");
		label.setHeight(40);

		final HashMap<String, List<FormItem>> allForms = new HashMap<String, List<FormItem>>();

		controller.fetchSupportedInfrastructuresAndPolicies(new Runnable() {
			public void run() {

				infraSelect = new SelectItem("infra", "Infrastructure");
				infraSelect.setRequired(true);
				policySelect = new SelectItem("policy", "Policy");
				policySelect.setRequired(true);

				infraSelect.setWidth(300);
				policySelect.setWidth(300);

				HiddenItem name = new HiddenItem("nsName");
				HiddenItem callback = new HiddenItem("nsCallback");
				HiddenItem session = new HiddenItem("sessionId");

				ArrayList<FormItem> tmpAll = new ArrayList<FormItem>();
				tmpAll.add(name);
				tmpAll.add(callback);
				tmpAll.add(session);
				tmpAll.add(infraSelect);

				LinkedHashMap<String, String> values = new LinkedHashMap<String, String>();
				for (PluginDescriptor inf : controller.getModel().getSupportedInfrastructures().values()) {
					String shortName = inf.getPluginName()
							.substring(inf.getPluginName().lastIndexOf('.') + 1);
					values.put(inf.getPluginName(), shortName);

					ArrayList<FormItem> forms = new ArrayList<FormItem>();
					for (Field f : inf.getConfigurableFields()) {
						FormItem infra = null;
						if (f.isPassword()) {
							infra = new PasswordItem(inf.getPluginName() + f.getName(), f.getName());
						} else if (f.isFile() || f.isCredential()) {
							infra = new UploadItem(inf.getPluginName() + f.getName(), f.getName());
							if (f.isCredential()) {
								PickerIcon cred = new PickerIcon(new Picker(Images.instance.key_16()
										.getSafeUri().asString()), new FormItemClickHandler() {
									@Override
									public void onFormItemClick(FormItemIconClickEvent event) {
										CredentialsWindow win = new CredentialsWindow();
										win.show();
									}
								});
								cred.setPrompt("Create a Credential file");
								cred.setWidth(16);
								cred.setHeight(16);
								cred.setAttribute("hspace", 6);
								infra.setIcons(cred);
							}
						} else {
							infra = new TextItem(inf.getPluginName() + f.getName(), f.getName());
						}
						infra.setValue(f.getValue());
						infra.setWidth(250);
						infra.setHint(f.getDescription());
						forms.add(infra);
						tmpAll.add(infra);
					}
					allForms.put(inf.getPluginName(), forms);
				}
				infraSelect.setValueMap(values);

				tmpAll.add(new SpacerItem());
				values.clear();
				tmpAll.add(policySelect);
				for (PluginDescriptor inf : controller.getModel().getSupportedPolicies().values()) {
					String shortName = inf.getPluginName()
							.substring(inf.getPluginName().lastIndexOf('.') + 1);
					values.put(inf.getPluginName(), shortName);

					ArrayList<FormItem> forms = new ArrayList<FormItem>();
					for (Field f : inf.getConfigurableFields()) {
						FormItem pol = null;
						if (f.isPassword()) {
							pol = new PasswordItem(inf.getPluginName() + f.getName(), f.getName());
						} else if (f.isFile() || f.isCredential()) {
							pol = new UploadItem(inf.getPluginName() + f.getName(), f.getName());
							if (f.isCredential()) {
								PickerIcon cred = new PickerIcon(new Picker(Images.instance.key_16()
										.getSafeUri().asString()), new FormItemClickHandler() {
									@Override
									public void onFormItemClick(FormItemIconClickEvent event) {
										CredentialsWindow win = new CredentialsWindow();
										win.show();
									}
								});
								cred.setPrompt("Create a Credential file");
								cred.setWidth(16);
								cred.setHeight(16);
								cred.setAttribute("hspace", 6);
								pol.setIcons(cred);
							}
						} else {
							pol = new TextItem(inf.getPluginName() + f.getName(), f.getName());
						}
						pol.setValue(f.getValue());
						pol.setWidth(250);
						pol.setHint(f.getDescription());
						forms.add(pol);
						tmpAll.add(pol);
					}
					allForms.put(inf.getPluginName(), forms);
				}
				policySelect.setValueMap(values);

				infraSelectChanged = new Runnable() {
					@Override
					public void run() {
						if (policySelect.getValueAsString() == null) {
							return;
						}

						String nsName = infraSelect.getValueAsString();
						if (oldInfra != null) {
							for (FormItem f : allForms.get(oldInfra)) {
								f.hide();
							}
						}
						for (FormItem f : allForms.get(nsName)) {
							f.show();
						}

						if (oldInfra == null) {
							oldInfra = nsName;
							policySelectChanged.run();
						} else {
							oldInfra = nsName;
						}
					}
				};

				policySelectChanged = new Runnable() {
					@Override
					public void run() {
						if (infraSelect.getValueAsString() == null) {
							return;
						}

						String policy = policySelect.getValueAsString();
						if (oldPolicy != null) {
							for (FormItem f : allForms.get(oldPolicy)) {
								f.hide();
							}
						}
						for (FormItem f : allForms.get(policy)) {
							f.show();
						}

						if (oldPolicy == null) {
							oldPolicy = policy;
							infraSelectChanged.run();
						} else {
							oldPolicy = policy;
						}

					}
				};

				infraSelect.addChangedHandler(new ChangedHandler() {
					public void onChanged(ChangedEvent event) {
						infraSelectChanged.run();
					}
				});

				policySelect.addChangedHandler(new ChangedHandler() {
					public void onChanged(ChangedEvent event) {
						policySelectChanged.run();
					}
				});

				infraForm.setFields(tmpAll.toArray(new FormItem[tmpAll.size()]));
				infraLabel.hide();
				infraForm.show();

				for (List<FormItem> li : allForms.values()) {
					for (FormItem it : li) {
						it.hide();
					}
				}
			}
		}, new Runnable() {
			@Override
			public void run() {
				window.hide();
			}
		});

		final TextItem nameItem = new TextItem("nsName", "Name");
		DynamicForm nameForm = new DynamicForm();
		nameForm.setFields(nameItem);

		HLayout buttons = new HLayout();

		buttons.setWidth100();
		buttons.setHeight(22);
		buttons.setMargin(5);
		buttons.setAlign(Alignment.RIGHT);
		buttons.setMembersMargin(5);

		final IButton okButton = new IButton("Ok");
		okButton.setIcon(Images.instance.ok_16().getSafeUri().asString());
		okButton.setShowDisabledIcon(false);
		final IButton cancelButton = new IButton("Cancel");
		cancelButton.setIcon(Images.instance.cancel_16().getSafeUri().asString());
		cancelButton.setShowDisabledIcon(false);

		okButton.addClickHandler(new ClickHandler() {
			public void onClick(ClickEvent event) {
				infraForm.setValue("nsName", nameItem.getValueAsString());
				infraForm.setValue("sessionId", controller.getModel().getSessionId());
				infraForm.setCanSubmit(true);

				/* this smartGWT form looks nice but cannot do callbacks ;
				 * we register a native JS function to the document, send it to
				 * the servlet so that it writes it back when returning
				 * when the browser reads the return value and interprets it as JS,
				 * the callback is called */
				infraForm.setValue("nsCallback", JSUtil.register(new JSUtil.JSCallback() {
					public void execute(JavaScriptObject obj) {
						JSONObject js = new JSONObject(obj);
						if (js.containsKey("result") && js.get("result").isBoolean().booleanValue()) {
							window.hide();
							controller.getModel().logMessage(
									"Successfully created nodesource: " + nameItem.getValueAsString());
						} else {
							String msg;
							if (js.get("errorMessage").isString() != null) {
								msg = js.get("errorMessage").isString().stringValue();
							} else {
								msg = js.toString();
							}
							label.setContents("<span style='color:red'>Failed to create Node Source :<br>" +
								msg + "</span>");
							controller.getModel()
									.logImportantMessage(
											"Failed to create nodesource " + nameItem.getValueAsString() +
												": " + msg);
							layout.scrollToTop();
						}
						infraLabel.hide();
						infraForm.show();
						okButton.setDisabled(false);
						cancelButton.setDisabled(false);
					}
				}));
				infraForm.submitForm();

				cancelButton.setDisabled(true);
				okButton.setDisabled(true);

				infraLabel.setContents("Node Source creation requested...");
				infraLabel.show();
				infraForm.hide();
			}
		});
		cancelButton.addClickHandler(new ClickHandler() {
			public void onClick(ClickEvent event) {
				window.hide();
			}
		});
		buttons.setMembers(okButton, cancelButton);

		VLayout scroll = new VLayout();
		scroll.setHeight100();
		scroll.setWidth100();
		scroll.setMembers(infraLayout);
		scroll.setOverflow(Overflow.AUTO);
		scroll.setBorder("1px solid #ddd");
		scroll.setBackgroundColor("#fafafa");

		layout.addMember(label);
		layout.addMember(nameForm);
		layout.addMember(scroll);
		layout.addMember(buttons);

		int winWidth = com.google.gwt.user.client.Window.getClientWidth() * 80 / 100;
		int winHeight = com.google.gwt.user.client.Window.getClientHeight() * 80 / 100;
		winWidth = Math.min(1000, winWidth);
		winHeight = Math.min(1000, winHeight);

		this.window = new Window();
		this.window.setTitle("Create Node Source");
		this.window.setShowMinimizeButton(false);
		this.window.setIsModal(true);
		this.window.setShowModalMask(true);
		this.window.addItem(layout);
		this.window.setWidth(winWidth);
		this.window.setHeight(winHeight);
		this.window.setCanDragResize(true);
		this.window.setCanDragReposition(true);
		this.window.centerInPage();
	}

}
