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
package org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.serialization.export.file;

import org.ow2.proactive_grid_cloud_portal.common.client.json.JSONUtils;
import org.ow2.proactive_grid_cloud_portal.rm.server.nodesource.serialization.export.ExportToFileServlet;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.google.gwt.user.client.ui.FormPanel;
import com.google.gwt.user.client.ui.Hidden;
import com.google.gwt.user.client.ui.VerticalPanel;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Window;


public abstract class ExportToFileHandler {

    private String nodeSourceName;

    private Window exportToFileWindow;

    private FormPanel exportToFileFormPanel;

    protected Hidden nodeSourceJsonContent;

    public ExportToFileHandler(String nodeSourceName) {
        this.nodeSourceName = nodeSourceName;
        createSubmitWindow();
    }

    public AsyncCallback<String> exportFromNodeSourceConfiguration() {
        return new ExportFromNodeSourceConfigurationCallback();
    }

    protected abstract String getFormTarget();

    protected abstract void handleNodeSourceConfigurationResult(String result);

    private void createSubmitWindow() {
        this.exportToFileWindow = new Window();
        this.exportToFileFormPanel = new FormPanel();
        configureFormPanel(this.exportToFileFormPanel);
        VerticalPanel panel = new VerticalPanel();

        this.nodeSourceJsonContent = new Hidden(ExportToFileServlet.MAIN_FORM_ITEM_NAME);

        panel.add(this.nodeSourceJsonContent);
        this.exportToFileFormPanel.setWidget(panel);
        this.exportToFileWindow.addChild(this.exportToFileFormPanel);
        this.exportToFileWindow.show();
    }

    private void configureFormPanel(FormPanel formPanel) {
        formPanel.setEncoding(FormPanel.ENCODING_MULTIPART);
        formPanel.setMethod(FormPanel.METHOD_POST);
        formPanel.setAction(GWT.getModuleBaseURL() + getFormTarget());
    }

    private class ExportFromNodeSourceConfigurationCallback implements AsyncCallback<String> {

        @Override
        public void onSuccess(String result) {
            handleNodeSourceConfigurationResult(result);
            exportToFileFormPanel.submit();
            exportToFileWindow.hide();
            exportToFileWindow.destroy();
        }

        @Override
        public void onFailure(Throwable caught) {
            String msg = JSONUtils.getJsonErrorMessage(caught);
            SC.warn("Failed to fetch configuration of node source " + nodeSourceName + ":<br>" + msg);
        }

    }

}
