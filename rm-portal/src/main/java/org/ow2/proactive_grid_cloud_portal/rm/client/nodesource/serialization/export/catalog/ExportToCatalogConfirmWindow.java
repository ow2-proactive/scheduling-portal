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
package org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.serialization.export.catalog;

import static org.ow2.proactive_grid_cloud_portal.rm.shared.CatalogConstants.EXPORT_FAILED_MESSAGE;
import static org.ow2.proactive_grid_cloud_portal.rm.shared.CatalogConstants.INITIAL_COMMIT_MESSAGE;
import static org.ow2.proactive_grid_cloud_portal.rm.shared.CatalogConstants.NODE_SOURCE_CONTENT_TYPE;
import static org.ow2.proactive_grid_cloud_portal.rm.shared.CatalogConstants.NODE_SOURCE_KIND;

import org.ow2.proactive_grid_cloud_portal.common.client.Images;
import org.ow2.proactive_grid_cloud_portal.common.client.json.JSONUtils;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LoginModel;
import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSourceConfiguration;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMController;
import org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.serialization.CatalogRequestBuilder;
import org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.serialization.CatalogUrlBuilder;
import org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.serialization.NodeSourceConfigurationParser;
import org.ow2.proactive_grid_cloud_portal.rm.shared.CatalogConstants;
import org.ow2.proactive_grid_cloud_portal.rm.shared.SerializationType;

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
import com.google.gwt.user.client.ui.FormPanel;
import com.google.gwt.user.client.ui.HasVerticalAlignment;
import com.google.gwt.user.client.ui.Hidden;
import com.google.gwt.user.client.ui.HorizontalPanel;
import com.google.gwt.user.client.ui.ListBox;
import com.google.gwt.user.client.ui.ScrollPanel;
import com.google.gwt.user.client.ui.TextBox;
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

    private static final String WINDOW_TITLE = "Export Node Source to Catalog";

    private static final int WINDOW_WIDTH = 680;

    private static final int WINDOW_HEIGHT = 210;

    private static final String SELECT_A_BUCKET_OPTION = "Select a bucket";

    private String nodeSourceName;

    private RMController rmController;

    private NodeSourceConfigurationParser parser;

    private ExportToCatalogHiddenPanel hiddenFormItemsPanel;

    private FormPanel exportNodeSourceToCatalogForm;

    private Label windowLabel;

    private ListBox bucketList;

    private TextBox commitMessage;

    private Label revisionLabel;

    private boolean nodeSourceRevised;

    public ExportToCatalogConfirmWindow(String nodeSourceName, RMController rmController) {
        this.nodeSourceRevised = false;
        this.nodeSourceName = nodeSourceName;
        this.rmController = rmController;
        this.parser = new NodeSourceConfigurationParser();
        this.exportNodeSourceToCatalogForm = new FormPanel();
        this.exportNodeSourceToCatalogForm.setEncoding(FormPanel.ENCODING_MULTIPART);
        this.exportNodeSourceToCatalogForm.setMethod(FormPanel.METHOD_POST);
        this.exportNodeSourceToCatalogForm.setAction(GWT.getModuleBaseURL() +
                                                     SerializationType.EXPORT_NODE_SOURCE_TO_CATALOG.getFormTarget());
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

        this.windowLabel = new Label("Choose the catalog bucket in which to publish the Node Source " + nodeSourceName);
        this.windowLabel.setHeight(30);

        HorizontalPanel exportInfoPanel = new HorizontalPanel();
        exportInfoPanel.setHeight("80px");
        exportInfoPanel.setWidth("640px");
        exportInfoPanel.setVerticalAlignment(HasVerticalAlignment.ALIGN_TOP);
        this.bucketList = new ListBox();
        fillBucketList();
        exportInfoPanel.add(this.bucketList);
        Label commitLabel = new Label("Commit message:");
        commitLabel.setHeight("80px");
        commitLabel.setValign(VerticalAlignment.TOP);
        commitLabel.setAlign(Alignment.RIGHT);
        exportInfoPanel.add(commitLabel);
        this.commitMessage = new TextBox();
        exportInfoPanel.add(this.commitMessage);
        this.revisionLabel = new Label(INITIAL_COMMIT_MESSAGE);
        this.revisionLabel.setHeight("80px");
        this.revisionLabel.setValign(VerticalAlignment.TOP);
        this.revisionLabel.setAlign(Alignment.LEFT);
        ScrollPanel scrollPanel = new ScrollPanel();
        scrollPanel.setHeight("80px");
        scrollPanel.add(this.revisionLabel);
        exportInfoPanel.add(scrollPanel);

        HLayout buttons = new HLayout();
        buttons.setHeight(40);
        buttons.setMembersMargin(5);
        buttons.setAlign(Alignment.RIGHT);
        buttons.setAlign(VerticalAlignment.BOTTOM);

        IButton ok = new IButton("OK", event -> submitNodeSourceConfiguration());
        ok.setIcon(Images.instance.ok_16().getSafeUri().asString());
        IButton cancel = new IButton("Cancel", event -> hideAndDestroy(this));
        cancel.setIcon(Images.instance.cancel_16().getSafeUri().asString());
        buttons.setMembers(ok, cancel);

        VLayout layout = new VLayout();
        layout.setAlign(VerticalAlignment.TOP);
        layout.setMargin(10);
        layout.setMembersMargin(10);
        layout.addMember(this.windowLabel);
        layout.addMember(exportInfoPanel);
        layout.addMember(buttons);

        addItem(layout);
    }

    private void fillBucketList() {
        this.bucketList.setEnabled(false);
        this.bucketList.addItem(SELECT_A_BUCKET_OPTION);
        this.bucketList.addChangeHandler(event -> requestNodeSourceInBucket(this.bucketList.getSelectedValue()));
        RequestBuilder request = new RequestBuilder(RequestBuilder.GET,
                                                    new CatalogUrlBuilder().getCatalogUrl() + "/buckets");
        request.setHeader("sessionId", LoginModel.getInstance().getSessionId());
        request.setCallback(fillBucketListWithRequestCallback());
        try {
            request.send();
        } catch (RequestException e) {
            String errorMessage = "Request sent to catalog failed";
            GWT.log(errorMessage, e);
            this.windowLabel.setContents(errorMessage);
        }
    }

    private void requestNodeSourceInBucket(String bucketName) {
        if (!bucketName.equals(SELECT_A_BUCKET_OPTION)) {
            CatalogRequestBuilder catalogRequestBuilder = new CatalogRequestBuilder(null);
            catalogRequestBuilder.requestNodeSourcesForBucket(bucketName, new RequestCallback() {
                @Override
                public void onResponseReceived(Request request, Response response) {
                    JSONArray nodeSources = JSONParser.parseStrict(response.getText()).isArray();
                    nodeSourceRevised = false;
                    revisionLabel.setContents("(Initial commit)");
                    for (int i = 0; i < nodeSources.size(); i++) {
                        JSONObject nodeSource = nodeSources.get(i).isObject();
                        String nodeSourceNameInBucket = nodeSource.get("name").isString().stringValue();
                        if (nodeSourceNameInBucket.equals(ExportToCatalogConfirmWindow.this.nodeSourceName)) {
                            nodeSourceRevised = true;
                            catalogRequestBuilder.sendRequestToCatalog("buckets/" + bucketName + "/resources/" +
                                                                       nodeSourceNameInBucket + "/revisions",
                                                                       new RequestCallback() {
                                                                           @Override
                                                                           public void onResponseReceived(
                                                                                   Request request, Response response) {
                                                                               JSONArray nodeSourceRevisions = JSONParser.parseStrict(response.getText())
                                                                                                                         .isArray();
                                                                               JSONObject lastNodeSourceRevision = nodeSourceRevisions.get(0)
                                                                                                                                      .isObject();
                                                                               revisionLabel.setContents("(Last Revision: " +
                                                                                                         lastNodeSourceRevision.get("commit_time")
                                                                                                                               .isString()
                                                                                                                               .stringValue() +
                                                                                                         " \"" +
                                                                                                         lastNodeSourceRevision.get("commit_message")
                                                                                                                               .isString()
                                                                                                                               .stringValue() +
                                                                                                         "\")");
                                                                           }

                                                                           @Override
                                                                           public void onError(Request request,
                                                                                   Throwable exception) {
                                                                               revisionLabel.setContents("<span style='color:red'>Last revision of this node source could not be retrieved</span>");
                                                                           }
                                                                       });
                            break;
                        }
                    }
                    revisionLabel.redraw();
                }

                @Override
                public void onError(Request request, Throwable exception) {
                    revisionLabel.setContents("<span style='color:red'>Could not check whether a revision exists for this node source</span>");
                }
            });
        } else {
            this.nodeSourceRevised = false;
            this.revisionLabel.setContents(INITIAL_COMMIT_MESSAGE);
            this.revisionLabel.redraw();
        }
    }

    private void submitNodeSourceConfiguration() {
        if (!this.bucketList.getSelectedValue().equals(SELECT_A_BUCKET_OPTION)) {
            Window window = new Window();
            window.addChild(this.exportNodeSourceToCatalogForm);
            this.exportNodeSourceToCatalogForm.addSubmitCompleteHandler(this::displayErrorToUserIfExportFailed);
            window.show();
            this.rmController.getRMService().getNodeSourceConfiguration(LoginModel.getInstance().getSessionId(),
                                                                        this.nodeSourceName,
                                                                        submitExportParametersCallback(window));
        } else {
            this.windowLabel.setContents("<span style='color:red'>Please select a bucket</span>");
        }
    }

    private void displayErrorToUserIfExportFailed(FormPanel.SubmitCompleteEvent event) {
        if (event.getResults().contains(EXPORT_FAILED_MESSAGE)) {
            windowLabel.setContents("<span style='color:red'>" + event.getResults() + "</span>");
        } else {
            hideAndDestroy(this);
        }
    }

    private AsyncCallback<String> submitExportParametersCallback(Window window) {
        return new AsyncCallback<String>() {
            public void onSuccess(String result) {
                try {
                    NodeSourceConfiguration nodeSourceConfiguration = parser.parseNodeSourceConfiguration(result);
                    hiddenFormItemsPanel.sessionIdFormField.setValue(LoginModel.getInstance().getSessionId());
                    hiddenFormItemsPanel.bucketNameFormField.setValue(bucketList.getSelectedValue());
                    hiddenFormItemsPanel.nodeSourceNameFormField.setValue(nodeSourceConfiguration.getNodeSourceName());
                    hiddenFormItemsPanel.nodeSourceJsonFormField.setValue(result);
                    hiddenFormItemsPanel.catalogObjectKindFormField.setValue(NODE_SOURCE_KIND);
                    hiddenFormItemsPanel.catalogObjectCommitMessageFormField.setValue(commitMessage.getText());
                    hiddenFormItemsPanel.catalogObjectContentTypeFormField.setValue(NODE_SOURCE_CONTENT_TYPE);
                    hiddenFormItemsPanel.revised.setValue(Boolean.toString(nodeSourceRevised));
                    exportNodeSourceToCatalogForm.submit();
                } catch (RuntimeException e) {
                    String msg = JSONUtils.getJsonErrorMessage(e);
                    SC.warn("Failed to export node source to catalog:<br>" + msg);
                } finally {
                    hideAndDestroy(window);
                }
            }

            public void onFailure(Throwable caught) {
                String msg = JSONUtils.getJsonErrorMessage(caught);
                SC.warn("Failed to fetch configuration of node source " + nodeSourceName + ":<br>" + msg);
                hideAndDestroy(window);
            }
        };
    }

    private RequestCallback fillBucketListWithRequestCallback() {
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
                windowLabel.setContents(errorMessage);
            }
        };
    }

    private void hideAndDestroy(Window window) {
        window.hide();
        window.destroy();
    }

    private class ExportToCatalogHiddenPanel extends VerticalPanel {

        private Hidden sessionIdFormField = new Hidden(CatalogConstants.SESSION_ID_PARAM);

        private Hidden bucketNameFormField = new Hidden(CatalogConstants.BUCKET_NAME_PARAM);

        private Hidden nodeSourceNameFormField = new Hidden(CatalogConstants.NAME_PARAM);

        private Hidden nodeSourceJsonFormField = new Hidden(CatalogConstants.FILE_CONTENT_PARAM);

        private Hidden catalogObjectKindFormField = new Hidden(CatalogConstants.KIND_PARAM);

        private Hidden catalogObjectCommitMessageFormField = new Hidden(CatalogConstants.COMMIT_MESSAGE_PARAM);

        private Hidden catalogObjectContentTypeFormField = new Hidden(CatalogConstants.OBJECT_CONTENT_TYPE_PARAM);

        private Hidden revised = new Hidden(CatalogConstants.REVISED_PARAM);

        private ExportToCatalogHiddenPanel() {
            add(this.sessionIdFormField);
            add(this.bucketNameFormField);
            add(this.nodeSourceNameFormField);
            add(this.nodeSourceJsonFormField);
            add(this.catalogObjectKindFormField);
            add(this.catalogObjectCommitMessageFormField);
            add(this.catalogObjectContentTypeFormField);
            add(this.revised);
        }

    }

}
