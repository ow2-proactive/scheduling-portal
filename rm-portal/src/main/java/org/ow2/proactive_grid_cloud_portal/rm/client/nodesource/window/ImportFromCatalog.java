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
package org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.window;

import org.ow2.proactive_grid_cloud_portal.common.client.model.LoginModel;
import org.ow2.proactive_grid_cloud_portal.rm.shared.RMConfig;

import com.google.gwt.core.client.GWT;
import com.google.gwt.http.client.Request;
import com.google.gwt.http.client.RequestBuilder;
import com.google.gwt.http.client.RequestCallback;
import com.google.gwt.http.client.RequestException;
import com.google.gwt.http.client.Response;
import com.google.gwt.json.client.JSONArray;
import com.google.gwt.json.client.JSONObject;
import com.google.gwt.json.client.JSONParser;
import com.google.gwt.user.client.ui.Button;
import com.google.gwt.user.client.ui.FormPanel;
import com.google.gwt.user.client.ui.HasHorizontalAlignment;
import com.google.gwt.user.client.ui.Hidden;
import com.google.gwt.user.client.ui.HorizontalPanel;
import com.google.gwt.user.client.ui.ListBox;
import com.google.gwt.user.client.ui.Panel;
import com.google.gwt.user.client.ui.VerticalPanel;


public class ImportFromCatalog {

    private static final String NAME_KEY = "name";

    private static final String CATALOG_SELECT_BUCKET = "Select a Bucket";

    private static final String CATALOG_SELECT_WF = "Select a Workflow";

    private static final String SESSION_ID_PARAMETER_NAME = "sessionId";

    private static final String URL_UPLOAD_FILE = GWT.getModuleBaseURL() + "uploader";

    private String CATALOG_URL = null;

    /**
     * Builds the catalog URL. If none is configured in the settings file, sets
     * the URL to the bundled Catalog
     *
     */
    private void buildCatalogUrl() {
        String catalogUrlFromConfig = RMConfig.get().getCatalogUrl();
        String defaultCatalogUrl = GWT.getHostPageBaseURL().replace("/rm/", "/") + "catalog";
        if (catalogUrlFromConfig == null || catalogUrlFromConfig.isEmpty()) {
            CATALOG_URL = defaultCatalogUrl;
        } else {
            CATALOG_URL = catalogUrlFromConfig;
        }
    }

    public Panel initSelectWorkflowFromCatalogPanel() {
        buildCatalogUrl();
        HorizontalPanel fromCatalogPanel = new HorizontalPanel();
        fromCatalogPanel.setHeight("30px");
        fromCatalogPanel.setWidth("100%");
        fromCatalogPanel.setSpacing(2);
        fromCatalogPanel.setHorizontalAlignment(HasHorizontalAlignment.ALIGN_CENTER);

        ListBox bucketsListBox = new ListBox();
        ListBox workflowsListBox = new ListBox();

        bucketsListBox.setEnabled(false);
        bucketsListBox.addItem(CATALOG_SELECT_BUCKET);

        workflowsListBox.setEnabled(false);
        workflowsListBox.addItem(CATALOG_SELECT_WF);

        bucketsListBox.addChangeHandler(event -> {
            String selectedBucket = bucketsListBox.getSelectedValue();
            if (!CATALOG_SELECT_BUCKET.equals(selectedBucket)) {
                String workflowUrl = CATALOG_URL + "/buckets/" + selectedBucket + "/resources?kind=nodesource";
                RequestBuilder req = new RequestBuilder(RequestBuilder.GET, workflowUrl);
                req.setHeader(SESSION_ID_PARAMETER_NAME, LoginModel.getInstance().getSessionId());
                req.setCallback(new RequestCallback() {
                    @Override
                    public void onResponseReceived(Request request, Response response) {
                        JSONArray workflows = JSONParser.parseStrict(response.getText()).isArray();
                        int workflowsSize = workflows.size();
                        workflowsListBox.setEnabled(false);
                        workflowsListBox.clear();
                        workflowsListBox.addItem(CATALOG_SELECT_WF);
                        for (int i = 0; i < workflowsSize; i++) {
                            JSONObject workflow = workflows.get(i).isObject();
                            String workflowName = workflow.get(NAME_KEY).isString().stringValue();
                            workflowsListBox.addItem(workflowName);
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
        });

        RequestBuilder req = new RequestBuilder(RequestBuilder.GET, CATALOG_URL + "/buckets?kind=nodesource");
        req.setHeader(SESSION_ID_PARAMETER_NAME, LoginModel.getInstance().getSessionId());
        req.setCallback(new RequestCallback() {
            @Override
            public void onResponseReceived(Request request, Response response) {
                JSONArray buckets = JSONParser.parseStrict(response.getText()).isArray();
                int bucketSize = buckets.size();
                for (int i = 0; i < bucketSize; i++) {
                    JSONObject bucket = buckets.get(i).isObject();
                    String bucketName = bucket.get(NAME_KEY).isString().stringValue();
                    bucketsListBox.addItem(bucketName);
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
        formContent.add(new Hidden(SESSION_ID_PARAMETER_NAME, LoginModel.getInstance().getSessionId()));

        final FormPanel importFromCatalogformPanel = new FormPanel();
        importFromCatalogformPanel.setEncoding(FormPanel.ENCODING_MULTIPART);
        importFromCatalogformPanel.setMethod(FormPanel.METHOD_POST);
        importFromCatalogformPanel.setAction(URL_UPLOAD_FILE);
        importFromCatalogformPanel.add(formContent);
        importFromCatalogformPanel.addSubmitCompleteHandler(fileUploadCompleteHandler());
        importFromCatalogformPanel.setHeight("30px");
        fromCatalogPanel.add(importFromCatalogformPanel);

        Button sendFromCatalogButton = new Button("Import workflow");
        sendFromCatalogButton.addClickHandler(clickHandlerForUploadFromCatalogButton(formContent,
                                                                                     importFromCatalogformPanel));

        fromCatalogPanel.add(sendFromCatalogButton);
        return fromCatalogPanel;
    }

    private FormPanel.SubmitCompleteHandler fileUploadCompleteHandler() {

        return new FormPanel.SubmitCompleteHandler() {

            @Override
            public void onSubmitComplete(FormPanel.SubmitCompleteEvent event) {
                /*
                 * String jobEditKey = "jobEdit";
                 * String res = event.getResults();
                 * 
                 * try {
                 * JSONValue js = JSONParser.parseStrict(res);
                 * JSONObject obj = js.isObject();
                 * 
                 * if (obj.get(jobEditKey) != null && obj.get(jobEditKey).isString() != null) {
                 * String val = obj.get(jobEditKey).isString().stringValue();
                 * job = new
                 * String(org.ow2.proactive_grid_cloud_portal.common.shared.Base64Utils.fromBase64(
                 * val));
                 * // if the job has an EXECUTION_CALENDAR Generic Information defined, the
                 * startAccordingToPlanningRadioButton becomes visible, and invisible otherwise
                 * setStartAccordingPlanningRadioButtonState(job);
                 * variables = readVars(job);
                 * } else {
                 * GWT.log(JSON_ERROR);
                 * displayErrorMessage(res);
                 * //Force disable check&submit buttons to prevent confusion if a valid job was
                 * uploaded
                 * first but not submitted
                 * setEnabledStartAtPart(false);
                 * startAccordingPlanningRadioButton.setVisible(false);
                 * return;
                 * }
                 * 
                 * } catch (JSONException t) {
                 * GWT.log(JSON_ERROR);
                 * displayErrorMessage(res);
                 * //Force disable check&submit buttons to prevent confusion if a valid job was
                 * uploaded
                 * first but not submitted
                 * setEnabledStartAtPart(false);
                 * startAccordingPlanningRadioButton.setVisible(false);
                 * return;
                 * }
                 * redrawVariables(job);
                 */
            }
        };
    }

    private com.google.gwt.event.dom.client.ClickHandler clickHandlerForUploadFromCatalogButton(
            final VerticalPanel formContent, final FormPanel importFromCatalogformPanel) {

        return new com.google.gwt.event.dom.client.ClickHandler() {

            @Override
            public void onClick(com.google.gwt.event.dom.client.ClickEvent event) {
                /*
                 * // filter only valid items
                 * if (bucketsListBox.getSelectedIndex() > 0 && workflowsListBox.getSelectedIndex()
                 * > 0) {
                 * String selectedWorkflowLabel = workflowsListBox.getSelectedValue();
                 * String selectedBucketName = bucketsListBox.getSelectedValue();
                 * formContent.add(new Hidden("bucketName", selectedBucketName));
                 * formContent.add(new Hidden("workflowName", selectedWorkflowLabel));
                 * displayLoadingMessage();
                 * importFromCatalogformPanel.submit();
                 * }
                 */
            }
        };
    }

}
