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

import com.google.gwt.http.client.Request;
import com.google.gwt.http.client.RequestBuilder;
import com.google.gwt.http.client.RequestCallback;
import com.google.gwt.http.client.RequestException;
import com.google.gwt.http.client.Response;
import com.google.gwt.json.client.JSONArray;
import com.google.gwt.json.client.JSONObject;
import com.google.gwt.json.client.JSONParser;


public class CatalogRequestBuilder {

    public static final String SESSION_ID_PARAMETER_NAME = "sessionId";

    private static final String NAME_KEY = "name";

    private ImportFromCatalogPanel importFromCatalogPanel;

    private String catalogUrl;

    public CatalogRequestBuilder(ImportFromCatalogPanel importFromCatalogPanel) {
        this.importFromCatalogPanel = importFromCatalogPanel;
        this.catalogUrl = new CatalogUrlBuilder().getCatalogUrl();
    }

    public void requestNodeSourcesFromAllBuckets() {
        String listBucketsUrl = this.catalogUrl + "/buckets";
        sendRequestToCatalog(listBucketsUrl, getBucketsRequestCallBack());
    }

    private RequestCallback getBucketsRequestCallBack() {
        return new RequestCallback() {
            @Override
            public void onResponseReceived(Request request, Response response) {
                JSONArray bucketsArray = JSONParser.parseStrict(response.getText()).isArray();
                for (int i = 0; i < bucketsArray.size(); i++) {
                    JSONObject bucketObject = bucketsArray.get(i).isObject();
                    String bucketName = bucketObject.get(NAME_KEY).isString().stringValue();
                    requestNodeSourcesForBucket(bucketName);
                }
                CatalogRequestBuilder.this.importFromCatalogPanel.enableNodeSourceListBox();
            }

            @Override
            public void onError(Request request, Throwable t) {
                CatalogRequestBuilder.this.importFromCatalogPanel.setNodeSourceWindowLabelWithError("List buckets from catalog failed",
                                                                                                    t);
            }
        };
    }

    private void requestNodeSourcesForBucket(String bucketName) {
        String nodeSourcesListUrl = this.catalogUrl + "/buckets/" + bucketName + "/resources?kind=nodesource";
        sendRequestToCatalog(nodeSourcesListUrl, getNodeSourcesRequestCallback(bucketName));
    }

    private void sendRequestToCatalog(String url, RequestCallback callback) {
        RequestBuilder request = new RequestBuilder(RequestBuilder.GET, url);
        request.setHeader(SESSION_ID_PARAMETER_NAME, LoginModel.getInstance().getSessionId());
        request.setCallback(callback);
        try {
            request.send();
        } catch (RequestException e) {
            this.importFromCatalogPanel.setNodeSourceWindowLabelWithError("Request sent to catalog failed", e);
        }
    }

    private RequestCallback getNodeSourcesRequestCallback(String bucketName) {
        return new RequestCallback() {
            @Override
            public void onResponseReceived(Request request, Response response) {
                JSONArray nodeSources = JSONParser.parseStrict(response.getText()).isArray();
                for (int i = 0; i < nodeSources.size(); i++) {
                    JSONObject nodeSource = nodeSources.get(i).isObject();
                    String nodeSourceName = nodeSource.get(NAME_KEY).isString().stringValue();
                    CatalogRequestBuilder.this.importFromCatalogPanel.addItemToNodeSourceListBox(bucketName + " - " +
                                                                                                 nodeSourceName,
                                                                                                 nodeSourceName);
                }
            }

            @Override
            public void onError(Request request, Throwable t) {
                CatalogRequestBuilder.this.importFromCatalogPanel.setNodeSourceWindowLabelWithError("List node sources from catalog failed",
                                                                                                    t);
            }
        };
    }

}
