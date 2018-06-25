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
import org.ow2.proactive_grid_cloud_portal.rm.shared.CatalogRequestParams;
import org.ow2.proactive_grid_cloud_portal.rm.shared.ServletMappings;

import com.google.gwt.core.client.GWT;
import com.google.gwt.http.client.Request;
import com.google.gwt.http.client.RequestBuilder;
import com.google.gwt.http.client.RequestCallback;
import com.google.gwt.http.client.RequestException;
import com.google.gwt.http.client.Response;
import com.google.gwt.json.client.JSONArray;
import com.google.gwt.json.client.JSONObject;
import com.google.gwt.json.client.JSONParser;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.google.gwt.user.client.ui.Hidden;
import com.google.gwt.user.client.ui.HorizontalPanel;
import com.google.gwt.user.client.ui.ListBox;
import com.google.gwt.user.client.ui.VerticalPanel;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;


public class ExportToCatalogConfirmWindow extends Window {

    public static final String WINDOW_TITLE = "Export Node Source to Catalog";

    public static final int WINDOW_WIDTH = 380;

    public static final int WINDOW_HEIGHT = 180;

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
        addContent();
    }

    private void configureWindow() {
        setTitle(WINDOW_TITLE);
        setShowMinimizeButton(false);
        setIsModal(true);
        setShowModalMask(true);
        setWidth(WINDOW_WIDTH);
        setHeight(WINDOW_HEIGHT);
        setCanDragResize(false);
        setCanDragReposition(false);
        centerInPage();
    }

    private void addContent() {
        this.hiddenFormItemsPanel = new ExportToCatalogHiddenPanel();
        this.exportNodeSourceToCatalogForm.setWidget(this.hiddenFormItemsPanel);

        Label label = new Label("You are about to publish the Node Source " + nodeSourceName +
                                " to the catalog. Please choose the catalog bucket in which you want to export the Node Source.");
        label.setHeight(40);

        HorizontalPanel bucketListPanel = new HorizontalPanel();
        bucketListPanel.setHeight("40px");
        ListBox bucketList = new ListBox();
        fillBucketList(label, bucketList);
        bucketListPanel.add(bucketList);

        HLayout buttons = new HLayout();
        buttons.setHeight(40);
        buttons.setMembersMargin(5);
        buttons.setAlign(Alignment.RIGHT);

        IButton ok = new IButton("OK", event -> submitNodeSourceConfiguration(bucketList));
        ok.setIcon(Images.instance.ok_16().getSafeUri().asString());
        IButton cancel = new IButton("Cancel", event -> hideAndDestroy(this));
        cancel.setIcon(Images.instance.cancel_16().getSafeUri().asString());
        buttons.setMembers(ok, cancel);

        VLayout layout = new VLayout();
        layout.setAlign(VerticalAlignment.TOP);
        layout.setMargin(10);
        layout.addMember(label);
        layout.addMember(bucketListPanel);
        layout.addMember(buttons);

        addItem(layout);
    }

    private void fillBucketList(Label label, ListBox bucketList) {
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
    }

    private void submitNodeSourceConfiguration(ListBox bucketList) {
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
                                                                            } catch (ImportException e) {
                                                                                String msg = JSONUtils.getJsonErrorMessage(e);
                                                                                SC.warn("Failed to export node source to catalog:<br>" +
                                                                                        msg);
                                                                            } finally {
                                                                                hideAndDestroy(window);
                                                                            }
                                                                        }

                                                                        public void onFailure(Throwable caught) {
                                                                            String msg = JSONUtils.getJsonErrorMessage(caught);
                                                                            SC.warn("Failed to fetch configuration of node source " +
                                                                                    ExportToCatalogConfirmWindow.this.nodeSourceName +
                                                                                    ":<br>" + msg);
                                                                            hideAndDestroy(window);
                                                                        }
                                                                    });

        hideAndDestroy(this);
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

    private void hideAndDestroy(Window window) {
        window.hide();
        window.destroy();
    }

    private class ExportToCatalogHiddenPanel extends VerticalPanel {

        private Hidden sessionIdFormField = new Hidden("sessionId");

        private Hidden bucketNameFormField = new Hidden(CatalogRequestParams.BUCKET_NAME_PARAM);

        private Hidden nodeSourceNameFormField = new Hidden(CatalogRequestParams.NAME_PARAM);

        private Hidden nodeSourceJsonFormField = new Hidden(CatalogRequestParams.FILE_CONTENT_PARAM);

        private Hidden catalogObjectKindFormField = new Hidden(CatalogRequestParams.KIND_PARAM);

        private Hidden catalogObjectCommitMessageFormField = new Hidden(CatalogRequestParams.COMMIT_MESSAGE_PARAM);

        private Hidden catalogObjectContentTypeFormField = new Hidden(CatalogRequestParams.OBJECT_CONTENT_TYPE_PARAM);

        private ExportToCatalogHiddenPanel() {
            add(this.sessionIdFormField);
            add(this.bucketNameFormField);
            add(this.nodeSourceNameFormField);
            add(this.nodeSourceJsonFormField);
            add(this.catalogObjectKindFormField);
            add(this.catalogObjectCommitMessageFormField);
            add(this.catalogObjectContentTypeFormField);
        }

    }

}
