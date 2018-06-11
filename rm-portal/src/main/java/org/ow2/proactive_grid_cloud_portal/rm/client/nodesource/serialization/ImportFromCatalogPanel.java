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
import com.google.gwt.user.client.ui.HorizontalPanel;
import com.google.gwt.user.client.ui.ListBox;


public class ImportFromCatalogPanel extends HorizontalPanel {

    static final String CATALOG_OPTION_NAME = "Import from Catalog";

    private static final String NAME_KEY = "name";

    private static final String SELECT_NODE_SOURCE_GENERIC_ENTRY = "Choose a Node Source";

    private static final String SESSION_ID_PARAMETER_NAME = "sessionId";

    private String catalogUrl;

    private ImportNodeSourceLayout importNodeSourcePanel;

    private ListBox nodeSourceListBox;

    ImportFromCatalogPanel(ImportNodeSourceLayout importNodeSourcePanel) {
        this.importNodeSourcePanel = importNodeSourcePanel;
        configureSize();
        getCatalogUrl();
        createListBox();
        requestNodeSourcesList();
    }

    private void configureSize() {
        setHeight("30px");
        setWidth("100%");
    }

    private void getCatalogUrl() {
        String catalogUrlFromConfig = RMConfig.get().getCatalogUrl();
        String defaultCatalogUrl = GWT.getHostPageBaseURL().replace("/rm/", "/") + "catalog";
        if (catalogUrlFromConfig == null || catalogUrlFromConfig.isEmpty()) {
            this.catalogUrl = defaultCatalogUrl;
        } else {
            this.catalogUrl = catalogUrlFromConfig;
        }
    }

    private void createListBox() {
        this.nodeSourceListBox = new ListBox();
        this.nodeSourceListBox.setEnabled(false);
        this.nodeSourceListBox.addItem(SELECT_NODE_SOURCE_GENERIC_ENTRY);
        this.nodeSourceListBox.setWidth("190px");
        this.nodeSourceListBox.addChangeHandler(onNodeSourceSelected -> requestNodeSourceConfiguration());
        add(this.nodeSourceListBox);
    }

    private void requestNodeSourceConfiguration() {
        String selectedNodeSourceInList = this.nodeSourceListBox.getSelectedValue();
        if (!selectedNodeSourceInList.equals(SELECT_NODE_SOURCE_GENERIC_ENTRY)) {
            String nodeSourceConfigurationUrl = this.catalogUrl + "/buckets/node-sources/resources/" +
                                                selectedNodeSourceInList + "/raw";
            RequestBuilder request = new RequestBuilder(RequestBuilder.GET, nodeSourceConfigurationUrl);
            request.setHeader(SESSION_ID_PARAMETER_NAME, LoginModel.getInstance().getSessionId());
            request.setCallback(new RequestCallback() {
                @Override
                public void onResponseReceived(Request request, Response response) {
                    ImportFromCatalogPanel.this.importNodeSourcePanel.handleNodeSourceImport(response.getText());
                }

                @Override
                public void onError(Request request, Throwable t) {
                    GWT.log("Import node source from catalog failed. Request was " + request.toString(), t);
                }
            });
            try {
                request.send();
            } catch (RequestException e) {
                GWT.log("Request sent to catalog failed", e);
            }
        }
    }

    private void requestNodeSourcesList() {
        String nodeSourcesListUrl = catalogUrl + "/buckets/node-sources/resources?kind=nodesource";
        RequestBuilder request = new RequestBuilder(RequestBuilder.GET, nodeSourcesListUrl);
        request.setHeader(SESSION_ID_PARAMETER_NAME, LoginModel.getInstance().getSessionId());
        request.setCallback(new RequestCallback() {
            @Override
            public void onResponseReceived(Request request, Response response) {
                JSONArray nodeSources = JSONParser.parseStrict(response.getText()).isArray();
                for (int i = 0; i < nodeSources.size(); i++) {
                    JSONObject nodeSource = nodeSources.get(i).isObject();
                    String nodeSourceName = nodeSource.get(NAME_KEY).isString().stringValue();
                    ImportFromCatalogPanel.this.nodeSourceListBox.addItem(nodeSourceName);
                }
                ImportFromCatalogPanel.this.nodeSourceListBox.setEnabled(true);
            }

            @Override
            public void onError(Request request, Throwable exception) {
                GWT.log("Error occurred when fetching buckets from Catalog");
            }
        });
        try {
            request.send();
        } catch (RequestException e) {
            GWT.log("Error occured when fetching buckets from Catalog");
            e.printStackTrace();
        }
    }

}
