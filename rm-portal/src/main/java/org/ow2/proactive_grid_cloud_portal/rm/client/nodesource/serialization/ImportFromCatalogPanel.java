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

import org.ow2.proactive_grid_cloud_portal.common.client.model.LoginModel;
import org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.NodeSourceWindow;
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
import com.google.gwt.user.client.ui.HasHorizontalAlignment;
import com.google.gwt.user.client.ui.HorizontalPanel;
import com.google.gwt.user.client.ui.ListBox;


public class ImportFromCatalogPanel extends HorizontalPanel {

    private static final String NAME_KEY = "name";

    private static final String SELECT_NODE_SOURCE_GENERIC_ENTRY = "Select a Node Source";

    private static final String SESSION_ID_PARAMETER_NAME = "sessionId";

    private NodeSourceWindow.NodeSourcePanelGroupsBuilder.ImportNodeSourcePanelBuilder importNodeSourcePanel;

    public ImportFromCatalogPanel(
            NodeSourceWindow.NodeSourcePanelGroupsBuilder.ImportNodeSourcePanelBuilder importNodeSourcePanel) {
        this.importNodeSourcePanel = importNodeSourcePanel;
        String catalogUrl = getCatalogUrl();
        configureSize();

        ListBox nodeSourceListBox = getListBox();
        nodeSourceListBox.addChangeHandler(event -> {
            String selectedNodeSourceInList = nodeSourceListBox.getSelectedValue();
            if (!selectedNodeSourceInList.equals(SELECT_NODE_SOURCE_GENERIC_ENTRY)) {
                String workflowUrl = catalogUrl + "/buckets/node-sources/resources/" + selectedNodeSourceInList +
                                     "/raw";
                RequestBuilder req = new RequestBuilder(RequestBuilder.GET, workflowUrl);
                req.setHeader(SESSION_ID_PARAMETER_NAME, LoginModel.getInstance().getSessionId());
                req.setCallback(new RequestCallback() {
                    @Override
                    public void onResponseReceived(Request request, Response response) {
                        importNodeSourcePanel.importNodeSourceFromJson(response.getText());
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

        RequestBuilder req = new RequestBuilder(RequestBuilder.GET,
                                                catalogUrl + "/buckets/node-sources/resources?kind=nodesource");
        req.setHeader(SESSION_ID_PARAMETER_NAME, LoginModel.getInstance().getSessionId());
        req.setCallback(new RequestCallback() {
            @Override
            public void onResponseReceived(Request request, Response response) {
                JSONArray buckets = JSONParser.parseStrict(response.getText()).isArray();
                int bucketSize = buckets.size();
                for (int i = 0; i < bucketSize; i++) {
                    JSONObject bucket = buckets.get(i).isObject();
                    String bucketName = bucket.get(NAME_KEY).isString().stringValue();
                    nodeSourceListBox.addItem(bucketName);
                }
                nodeSourceListBox.setEnabled(true);
            }

            @Override
            public void onError(Request request, Throwable exception) {
                GWT.log("Error occurred when fetching buckets from Catalog");
            }
        });

        try {
            req.send();
        } catch (RequestException e) {
            GWT.log("Error occured when fetching buckets from Catalog");
            e.printStackTrace();
        }

        add(nodeSourceListBox);
        nodeSourceListBox.setWidth("130px");
    }

    private ListBox getListBox() {
        ListBox nodeSourceListBox = new ListBox();
        nodeSourceListBox.setEnabled(false);
        nodeSourceListBox.addItem(SELECT_NODE_SOURCE_GENERIC_ENTRY);
        return nodeSourceListBox;
    }

    private void configureSize() {
        setHeight("30px");
        setWidth("100%");
        setSpacing(2);
        setHorizontalAlignment(HasHorizontalAlignment.ALIGN_CENTER);
    }

    private String getCatalogUrl() {
        String catalogUrlFromConfig = RMConfig.get().getCatalogUrl();
        String defaultCatalogUrl = GWT.getHostPageBaseURL().replace("/rm/", "/") + "catalog";
        if (catalogUrlFromConfig == null || catalogUrlFromConfig.isEmpty()) {
            return defaultCatalogUrl;
        } else {
            return catalogUrlFromConfig;
        }
    }

}
