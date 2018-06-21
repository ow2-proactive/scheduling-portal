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
package org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.serialization;

import org.ow2.proactive_grid_cloud_portal.common.client.Images;
import org.ow2.proactive_grid_cloud_portal.common.client.json.JSONUtils;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LoginModel;
import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSourceConfiguration;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMController;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMModelImpl;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMServiceAsync;
import org.ow2.proactive_grid_cloud_portal.rm.shared.CatalogRequestParams;
import org.ow2.proactive_grid_cloud_portal.rm.shared.ServletMappings;

import com.google.gwt.core.client.GWT;
import com.google.gwt.http.client.*;
import com.google.gwt.json.client.JSONArray;
import com.google.gwt.json.client.JSONObject;
import com.google.gwt.json.client.JSONParser;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.google.gwt.user.client.ui.*;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;


public class ExportToCatalogConfirmWindow extends Window {

    private String nodeSourceName;

    private RMController rmController;

    private NodeSourceConfigurationParser parser;

    private ExportToCatalogHiddenPanel hiddenFormItemsPanel;

    private NodeSourceSerializationFormPanel exportNodeSourceToCatalogForm;

    public ExportToCatalogConfirmWindow(String nodeSourceName, RMController rmController) {
        this.nodeSourceName = nodeSourceName;
        this.rmController = rmController;
        this.parser = new NodeSourceConfigurationParser(rmController);
        this.exportNodeSourceToCatalogForm = new NodeSourceSerializationFormPanel(ServletMappings.EXPORT_NODE_SOURCE_TO_CATALOG);
        configureWindow();
        buildForm();
    }

    private void configureWindow() {
        setTitle("Export Node Source to Catalog");
        setShowMinimizeButton(false);
        setIsModal(true);
        setShowModalMask(true);
        setWidth(380);
        setHeight(160);
        setCanDragResize(false);
        setCanDragReposition(false);
        centerInPage();
    }

    private void buildForm() {
        this.hiddenFormItemsPanel = new ExportToCatalogHiddenPanel();
        this.exportNodeSourceToCatalogForm.setWidget(this.hiddenFormItemsPanel);
        //addChild(this.exportNodeSourceToCatalogForm);

        Label label = new Label("You are about to publish the Node Source " + nodeSourceName +
                                " to the node-sources bucket.");
        label.setHeight(40);

        ListBox bucketList = new ListBox();

        bucketList.setEnabled(false);

        RequestBuilder request = new RequestBuilder(RequestBuilder.GET,
                                                    new CatalogUrlBuilder().getCatalogUrl() + "/buckets");
        request.setHeader("sessionId", LoginModel.getInstance().getSessionId());
        request.setCallback(fillBucketListWithRequestCallback(bucketList, label));
        try {
            request.send();
        } catch (RequestException e) {
            String errorMessage = "Request sent to catalog failed";
            GWT.log(errorMessage, e);
            label.setContents(errorMessage);
        }

        HLayout buttons = new HLayout();
        buttons.setMembersMargin(5);
        buttons.setAlign(Alignment.RIGHT);
        buttons.setHeight(25);

        IButton ok = new IButton("OK", event -> {

            Window window = new Window();
            window.addChild(exportNodeSourceToCatalogForm);
            window.show();
            this.rmController.getRMService().getNodeSourceConfiguration(LoginModel.getInstance().getSessionId(),
                                                                        this.nodeSourceName,
                                                                        new AsyncCallback<String>() {
                                                                            public void onSuccess(String result) {
                                                                                try {
                                                                                    NodeSourceConfiguration nodeSourceConfiguration = ExportToCatalogConfirmWindow.this.parser.parseNodeSourceConfiguration(result);
                                                                                    hiddenFormItemsPanel.sessionIdFormField.setValue(LoginModel.getInstance()
                                                                                                                                               .getSessionId());
                                                                                    hiddenFormItemsPanel.bucketNameFormField.setValue(bucketList.getSelectedValue());
                                                                                    hiddenFormItemsPanel.nodeSourceNameFormField.setValue(nodeSourceConfiguration.getNodeSourceName());
                                                                                    hiddenFormItemsPanel.nodeSourceJsonFormField.setValue(result);
                                                                                    hiddenFormItemsPanel.catalogObjectKindFormField.setValue("NodeSource");
                                                                                    hiddenFormItemsPanel.catalogObjectCommitMessageFormField.setValue("commitmessage");
                                                                                    hiddenFormItemsPanel.catalogObjectContentTypeFormField.setValue("application/json");
                                                                                    exportNodeSourceToCatalogForm.submit();
                                                                                    window.hide();
                                                                                    window.destroy();
                                                                                } catch (ImportException e) {
                                                                                    String msg = JSONUtils.getJsonErrorMessage(e);
                                                                                    SC.warn("Failed to export node source to catalog:<br>" +
                                                                                            msg);
                                                                                }
                                                                            }

                                                                            public void onFailure(Throwable caught) {
                                                                                String msg = JSONUtils.getJsonErrorMessage(caught);
                                                                                SC.warn("Failed to fetch configuration of node source " +
                                                                                        ExportToCatalogConfirmWindow.this.nodeSourceName +
                                                                                        ":<br>" + msg);
                                                                            }
                                                                        });

            hide();
            destroy();
        });
        ok.setIcon(Images.instance.ok_16().getSafeUri().asString());
        IButton cancel = new IButton("Cancel", event -> {
            hide();
            destroy();
        });
        cancel.setIcon(Images.instance.cancel_16().getSafeUri().asString());
        buttons.setMembers(ok, cancel);

        VLayout layout = new VLayout();
        layout.setMembersMargin(5);
        layout.setMargin(5);
        layout.addMember(label);
        layout.addMember(bucketList);
        layout.addMember(buttons);

        addItem(layout);
    }

    private RequestCallback fillBucketListWithRequestCallback(ListBox bucketList, Label label) {
        return new RequestCallback() {
            @Override
            public void onResponseReceived(Request request, Response response) {
                JSONArray bucketsArray = JSONParser.parseStrict(response.getText()).isArray();
                for (int i = 0; i < bucketsArray.size(); i++) {
                    JSONObject bucketObject = bucketsArray.get(i).isObject();
                    bucketList.addItem(bucketObject.get("name").isString().stringValue());
                }
                bucketList.setEnabled(true);
            }

            @Override
            public void onError(Request request, Throwable t) {
                String errorMessage = "List buckets from catalog failed";
                GWT.log(errorMessage, t);
                label.setContents(errorMessage);
            }
        };
    }

    private void fetchNodeSourceConfigurationOnConfirmation() {
        this.rmController.getRMService().getNodeSourceConfiguration(LoginModel.getInstance().getSessionId(),
                                                                    this.nodeSourceName,
                                                                    new AsyncCallback<String>() {
                                                                        public void onSuccess(String result) {
                                                                            try {
                                                                                NodeSourceConfiguration nodeSourceConfiguration = ExportToCatalogConfirmWindow.this.parser.parseNodeSourceConfiguration(result);
                                                                                hiddenFormItemsPanel.sessionIdFormField.setValue(LoginModel.getInstance()
                                                                                                                                           .getSessionId());
                                                                                hiddenFormItemsPanel.bucketNameFormField.setValue("node-sources");
                                                                                hiddenFormItemsPanel.nodeSourceNameFormField.setValue(nodeSourceConfiguration.getNodeSourceName());
                                                                                hiddenFormItemsPanel.nodeSourceJsonFormField.setValue("toto");//result);
                                                                                hiddenFormItemsPanel.catalogObjectKindFormField.setValue("NodeSource");
                                                                                hiddenFormItemsPanel.catalogObjectCommitMessageFormField.setValue("commitmessage");
                                                                                hiddenFormItemsPanel.catalogObjectContentTypeFormField.setValue("application/json");
                                                                                exportNodeSourceToCatalogForm.submit();
                                                                            } catch (ImportException e) {
                                                                                String msg = JSONUtils.getJsonErrorMessage(e);
                                                                                SC.warn("Failed to export node source to catalog:<br>" +
                                                                                        msg);
                                                                            }
                                                                        }

                                                                        public void onFailure(Throwable caught) {
                                                                            String msg = JSONUtils.getJsonErrorMessage(caught);
                                                                            SC.warn("Failed to fetch configuration of node source " +
                                                                                    ExportToCatalogConfirmWindow.this.nodeSourceName +
                                                                                    ":<br>" + msg);
                                                                        }
                                                                    });
    }

    private class ExportToCatalogHiddenPanel extends VerticalPanel {

        private Hidden sessionIdFormField;

        private Hidden bucketNameFormField;

        private Hidden nodeSourceNameFormField;

        private Hidden nodeSourceJsonFormField;

        private Hidden catalogObjectKindFormField;

        private Hidden catalogObjectCommitMessageFormField;

        private Hidden catalogObjectContentTypeFormField;

        private ExportToCatalogHiddenPanel() {

            this.sessionIdFormField = new Hidden("sessionId");
            this.bucketNameFormField = new Hidden(CatalogRequestParams.BUCKET_NAME_PARAM);
            this.nodeSourceNameFormField = new Hidden(CatalogRequestParams.NAME_PARAM);
            this.nodeSourceJsonFormField = new Hidden(CatalogRequestParams.FILE_CONTENT_PARAM);
            this.catalogObjectKindFormField = new Hidden(CatalogRequestParams.KIND_PARAM);
            this.catalogObjectCommitMessageFormField = new Hidden(CatalogRequestParams.COMMIT_MESSAGE_PARAM);
            this.catalogObjectContentTypeFormField = new Hidden(CatalogRequestParams.OBJECT_CONTENT_TYPE_PARAM);

            add(this.sessionIdFormField);
            add(this.bucketNameFormField);
            add(this.nodeSourceNameFormField);
            add(this.nodeSourceJsonFormField);
            add(this.catalogObjectKindFormField);
            add(this.catalogObjectCommitMessageFormField);
            add(this.catalogObjectContentTypeFormField);
        }

        private Widget[] getItems() {
            return new Widget[] { this.sessionIdFormField, this.bucketNameFormField, this.nodeSourceNameFormField,
                                  this.nodeSourceJsonFormField, this.catalogObjectKindFormField,
                                  this.catalogObjectCommitMessageFormField, this.catalogObjectContentTypeFormField };
        }

    }

}
