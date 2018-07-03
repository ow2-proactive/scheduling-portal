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
package org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.serialization.load.catalog;

import java.util.HashMap;
import java.util.Map;

import org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.serialization.CatalogRequestBuilder;

import com.google.gwt.http.client.Request;
import com.google.gwt.http.client.RequestCallback;
import com.google.gwt.http.client.Response;
import com.google.gwt.json.client.JSONArray;
import com.google.gwt.json.client.JSONObject;
import com.google.gwt.json.client.JSONParser;


public class ImportFromCatalogPanelFeeder {

    private static final String NAME_KEY = "name";

    private ImportFromCatalogPanel importFromCatalogPanel;

    private CatalogRequestBuilder catalogRequestBuilder;

    private Map<String, String> bucketNamePerNodeSourceName;

    public ImportFromCatalogPanelFeeder(ImportFromCatalogPanel importFromCatalogPanel) {
        this.importFromCatalogPanel = importFromCatalogPanel;
        this.catalogRequestBuilder = new CatalogRequestBuilder(this);
        this.bucketNamePerNodeSourceName = new HashMap<>();
    }

    public void requestNodeSourcesFromAllBuckets() {
        this.catalogRequestBuilder.sendRequestToCatalog("buckets", getBucketsRequestCallBack());
    }

    public void setNodeSourceWindowLabelWithError(String userMessage, Throwable e) {
        this.importFromCatalogPanel.setNodeSourceWindowLabelWithError(userMessage, e);
    }

    public String getBucketNameForNodeSource(String nodeSourceName) {
        return this.bucketNamePerNodeSourceName.get(nodeSourceName);
    }

    private RequestCallback getBucketsRequestCallBack() {
        return new RequestCallback() {
            @Override
            public void onResponseReceived(Request request, Response response) {
                JSONArray bucketsArray = JSONParser.parseStrict(response.getText()).isArray();
                for (int i = 0; i < bucketsArray.size(); i++) {
                    JSONObject bucketObject = bucketsArray.get(i).isObject();
                    String bucketName = bucketObject.get(NAME_KEY).isString().stringValue();
                    ImportFromCatalogPanelFeeder.this.catalogRequestBuilder.requestNodeSourcesForBucket(bucketName,
                                                                                                        fillBucketInImportPanel(bucketName));
                }
                ImportFromCatalogPanelFeeder.this.importFromCatalogPanel.enableNodeSourceListBox();
            }

            @Override
            public void onError(Request request, Throwable t) {
                ImportFromCatalogPanelFeeder.this.importFromCatalogPanel.setNodeSourceWindowLabelWithError("List buckets from catalog failed",
                                                                                                           t);
            }
        };
    }

    private RequestCallback fillBucketInImportPanel(String bucketName) {
        return new RequestCallback() {
            @Override
            public void onResponseReceived(Request request, Response response) {
                JSONArray nodeSources = JSONParser.parseStrict(response.getText()).isArray();
                for (int i = 0; i < nodeSources.size(); i++) {
                    JSONObject nodeSource = nodeSources.get(i).isObject();
                    String nodeSourceName = nodeSource.get(NAME_KEY).isString().stringValue();
                    ImportFromCatalogPanelFeeder.this.bucketNamePerNodeSourceName.put(nodeSourceName, bucketName);
                    ImportFromCatalogPanelFeeder.this.importFromCatalogPanel.addItemToNodeSourceListBox(bucketName +
                                                                                                        " - " +
                                                                                                        nodeSourceName,
                                                                                                        nodeSourceName);
                }
            }

            @Override
            public void onError(Request request, Throwable t) {
                ImportFromCatalogPanelFeeder.this.importFromCatalogPanel.setNodeSourceWindowLabelWithError("List node sources from catalog failed",
                                                                                                           t);
            }
        };
    }

}
