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

import org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.NodeSourceWindow;

import com.google.gwt.event.dom.client.ChangeEvent;
import com.google.gwt.user.client.ui.FormPanel;
import com.google.gwt.user.client.ui.HasVerticalAlignment;
import com.google.gwt.user.client.ui.ListBox;
import com.google.gwt.user.client.ui.VerticalPanel;
import com.smartgwt.client.widgets.layout.VLayout;


public class ImportNodeSourceLayout extends VLayout {

    private static final String GENERIC_OPTION_NAME = "Choose Import Method";

    private NodeSourceWindow nodeSourceWindow;

    private VerticalPanel importPanel;

    private VerticalPanel selectedImportMethodPanel;

    private ListBox importMethodList;

    public ImportNodeSourceLayout(NodeSourceWindow nodeSourceWindow) {
        this.nodeSourceWindow = nodeSourceWindow;
        createImportPanel();
        createImportMethodList();
        addSelectImportMethodPanel();
        addSelectedImportMethodPanel();
    }

    public void setNodeSourceWindowLabelWithError(String errorMessage, Throwable e) {
        this.nodeSourceWindow.setNodeSourceWindowLabelWithError(errorMessage, e);
    }

    void handleNodeSourceImport(FormPanel.SubmitCompleteEvent importCompleteEvent) {
        this.nodeSourceWindow.importNodeSourceFromJson(importCompleteEvent.getResults());
    }

    void handleNodeSourceImport(String importedNodeSourceJsonString) {
        this.nodeSourceWindow.importNodeSourceFromJson(importedNodeSourceJsonString);
    }

    private void createImportPanel() {
        setGroupTitle("or Import Node Source");
        setIsGroup(true);
        this.importPanel = new VerticalPanel();
        this.importPanel.setSpacing(10);
        this.importPanel.setHeight("70px");
        this.importPanel.setWidth("240px");
        addMember(this.importPanel);
    }

    private void createImportMethodList() {
        this.importMethodList = new ListBox();
        this.importMethodList.setWidth("190px");
        this.importMethodList.addItem(GENERIC_OPTION_NAME);
        this.importMethodList.addItem(ImportFromFilePanel.FILE_OPTION_NAME);
        this.importMethodList.addItem(ImportFromCatalogPanel.CATALOG_OPTION_NAME);
        this.importMethodList.addChangeHandler(this::replaceSelectedImportMethodPanelContent);
    }

    private void replaceSelectedImportMethodPanelContent(ChangeEvent changeEvent) {
        this.selectedImportMethodPanel.clear();
        String selectedOption = this.importMethodList.getSelectedValue();
        if (selectedOption.equals(ImportFromFilePanel.FILE_OPTION_NAME)) {
            this.selectedImportMethodPanel.add(new ImportFromFilePanel(this));
        } else if (selectedOption.equals(ImportFromCatalogPanel.CATALOG_OPTION_NAME)) {
            this.selectedImportMethodPanel.add(new ImportFromCatalogPanel(this));
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
