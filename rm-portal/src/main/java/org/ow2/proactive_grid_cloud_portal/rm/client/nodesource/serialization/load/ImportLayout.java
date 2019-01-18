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
package org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.serialization.load;

import org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.NodeSourceWindow;
import org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.serialization.load.catalog.ImportFromCatalogPanel;
import org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.serialization.load.file.ImportFromFilePanel;
import org.ow2.proactive_grid_cloud_portal.rm.shared.CatalogKind;
import org.ow2.proactive_grid_cloud_portal.rm.shared.NodeSourceAction;

import com.google.gwt.event.dom.client.ChangeEvent;
import com.google.gwt.http.client.Request;
import com.google.gwt.http.client.RequestCallback;
import com.google.gwt.http.client.Response;
import com.google.gwt.user.client.ui.HasVerticalAlignment;
import com.google.gwt.user.client.ui.ListBox;
import com.google.gwt.user.client.ui.VerticalPanel;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.layout.VLayout;


public abstract class ImportLayout extends VLayout {

    private static final String GENERIC_OPTION_NAME = "Choose Import Method";

    protected NodeSourceWindow nodeSourceWindow;

    private VerticalPanel importPanel;

    private VerticalPanel selectedImportMethodPanel;

    private ListBox importMethodList;

    private boolean disabled;

    ImportLayout(NodeSourceWindow nodeSourceWindow, String layoutTitle, NodeSourceAction nodeSourceAction) {
        this.nodeSourceWindow = nodeSourceWindow;
        this.disabled = !nodeSourceAction.isFullEditAllowed();
        createImportPanel(layoutTitle);
        createImportMethodList();
        addImportMethodsIfEnabled();
    }

    public abstract CatalogKind getKind();

    public abstract void handleImport(String submitResult);

    private void createImportPanel(String layoutTitle) {
        this.importPanel = new VerticalPanel();
        this.importPanel.setSpacing(10);
        this.importPanel.setHeight("70px");
        this.importPanel.setWidth("272px");
        Label importLabel = new Label(layoutTitle);
        importLabel.setHeight("15px");
        importLabel.setWidth("250px");
        this.importPanel.add(importLabel);
        addMember(this.importPanel);
    }

    private void createImportMethodList() {
        this.importMethodList = new ListBox();
        this.importMethodList.setWidth("190px");
        this.importMethodList.addItem(GENERIC_OPTION_NAME);
    }

    private void addImportMethodsIfEnabled() {
        if (disabled) {
            this.importMethodList.setEnabled(false);
        } else {
            this.importMethodList.addItem(ImportFromFilePanel.FILE_OPTION_NAME);
            this.importMethodList.addItem(ImportFromCatalogPanel.CATALOG_OPTION_NAME);
            this.importMethodList.addChangeHandler(this::replaceSelectedImportMethodPanelContent);
        }
        addSelectImportMethodPanel();
        addSelectedImportMethodPanel();
    }

    private void replaceSelectedImportMethodPanelContent(ChangeEvent changeEvent) {
        this.selectedImportMethodPanel.clear();
        String selectedOption = this.importMethodList.getSelectedValue();
        if (selectedOption.equals(ImportFromFilePanel.FILE_OPTION_NAME)) {
            this.selectedImportMethodPanel.add(new ImportFromFilePanel(importCompleteEvent -> handleImport(importCompleteEvent.getResults())));
        } else if (selectedOption.equals(ImportFromCatalogPanel.CATALOG_OPTION_NAME)) {
            addImportFromCatalogPanelOrFail();
        }
    }

    private void addImportFromCatalogPanelOrFail() {
        try {
            this.selectedImportMethodPanel.add(new ImportFromCatalogPanel(getKind(), new RequestCallback() {
                @Override
                public void onResponseReceived(Request request, Response response) {
                    handleImport(response.getText());
                    ImportLayout.this.nodeSourceWindow.setNormalNodeSourceWindowLabel();
                }

                @Override
                public void onError(Request request, Throwable t) {
                    ImportLayout.this.nodeSourceWindow.setNodeSourceWindowLabelWithError("Import from catalog failed",
                                                                                         t);
                }
            }));
        } catch (Exception e) {
            this.nodeSourceWindow.setNodeSourceWindowLabelWithError("Request sent to catalog failed", e);
        }
    }

    private void addSelectImportMethodPanel() {
        VerticalPanel selectImportMethodPanel = new VerticalPanel();
        selectImportMethodPanel.setHeight("20px");
        selectImportMethodPanel.setVerticalAlignment(HasVerticalAlignment.ALIGN_MIDDLE);
        selectImportMethodPanel.add(this.importMethodList);
        this.importPanel.add(selectImportMethodPanel);
    }

    private void addSelectedImportMethodPanel() {
        this.selectedImportMethodPanel = new VerticalPanel();
        this.selectedImportMethodPanel.setHeight("20px");
        this.selectedImportMethodPanel.setVerticalAlignment(HasVerticalAlignment.ALIGN_MIDDLE);
        this.importPanel.add(this.selectedImportMethodPanel);
    }

}
