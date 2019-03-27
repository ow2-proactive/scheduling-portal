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

import org.ow2.proactive_grid_cloud_portal.common.client.model.LoginModel;
import org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.serialization.CatalogUrlRmClientBuilder;
import org.ow2.proactive_grid_cloud_portal.rm.shared.CatalogConstants;
import org.ow2.proactive_grid_cloud_portal.rm.shared.CatalogKind;

import com.google.gwt.http.client.RequestBuilder;
import com.google.gwt.http.client.RequestCallback;
import com.google.gwt.http.client.RequestException;
import com.google.gwt.user.client.ui.HorizontalPanel;
import com.google.gwt.user.client.ui.ListBox;


public class ImportFromCatalogPanel extends HorizontalPanel {

    public static final String CATALOG_OPTION_NAME = "Import from Catalog";

    private String selectItemGenericEntry;

    private ListBox nodeSourceListBox;

    private ImportFromCatalogPanelFeeder importFromCatalogPanelFeeder;

    private RequestCallback nodeSourceConfigurationRequestCallback;

    public ImportFromCatalogPanel(CatalogKind kind, RequestCallback nodeSourceConfigurationRequestCallback) {
        this.selectItemGenericEntry = "Choose " + kind.getDescription();
        this.nodeSourceConfigurationRequestCallback = nodeSourceConfigurationRequestCallback;
        this.importFromCatalogPanelFeeder = new ImportFromCatalogPanelFeeder(this);
        configureSize();
        createListBox();
        this.importFromCatalogPanelFeeder.requestCatalogObjectsFromAllBuckets(kind);
    }

    void addItemToNodeSourceListBox(String displayName, String valueName) {
        this.nodeSourceListBox.addItem(displayName, valueName);
    }

    void enableNodeSourceListBox() {
        this.nodeSourceListBox.setEnabled(true);
    }

    private void configureSize() {
        setHeight("30px");
        setWidth("100%");
    }

    private void createListBox() {
        this.nodeSourceListBox = new ListBox();
        this.nodeSourceListBox.setEnabled(false);
        this.nodeSourceListBox.addItem(this.selectItemGenericEntry);
        this.nodeSourceListBox.setWidth("190px");
        this.nodeSourceListBox.addChangeHandler(onNodeSourceSelected -> requestNodeSourceConfiguration());
        add(this.nodeSourceListBox);
    }

    private void requestNodeSourceConfiguration() {
        String selectedNodeSourceInList = this.nodeSourceListBox.getSelectedValue();
        if (!selectedNodeSourceInList.equals(this.selectItemGenericEntry)) {
            String nodeSourceConfigurationRequestUrl = new CatalogUrlRmClientBuilder().getCatalogUrl() + "/buckets/" +
                                                       this.importFromCatalogPanelFeeder.getBucketNameForNodeSource(selectedNodeSourceInList) +
                                                       "/resources/" + selectedNodeSourceInList + "/raw";
            RequestBuilder nodeSourceConfigurationRequest = new RequestBuilder(RequestBuilder.GET,
                                                                               nodeSourceConfigurationRequestUrl);
            nodeSourceConfigurationRequest.setHeader(CatalogConstants.SESSION_ID_PARAM,
                                                     LoginModel.getInstance().getSessionId());
            nodeSourceConfigurationRequest.setCallback(nodeSourceConfigurationRequestCallback);
            try {
                nodeSourceConfigurationRequest.send();
            } catch (RequestException e) {
                throw new IllegalStateException("GET " + nodeSourceConfigurationRequestUrl + " failed", e);
            }
        }
    }

}
