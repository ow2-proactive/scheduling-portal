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
import org.ow2.proactive_grid_cloud_portal.rm.shared.CatalogConstants;
import org.ow2.proactive_grid_cloud_portal.rm.shared.CatalogKind;
import org.ow2.proactive_grid_cloud_portal.rm.shared.NodeSourceAction;

import com.google.gwt.event.dom.client.ChangeEvent;
import com.google.gwt.user.client.ui.HasVerticalAlignment;
import com.google.gwt.user.client.ui.ListBox;
import com.google.gwt.user.client.ui.VerticalPanel;
import com.smartgwt.client.widgets.layout.VLayout;


public class ImportNodeSourceLayout extends ImportLayout {

    public ImportNodeSourceLayout(NodeSourceWindow nodeSourceWindow, String layoutTitle,
            NodeSourceAction nodeSourceAction) {
        super(nodeSourceWindow, layoutTitle, nodeSourceAction);
    }

    @Override
    public CatalogKind getKind() {
        return CatalogKind.NODE_SOURCE;
    }

    @Override
    public void handleImport(String submitResult) {
        try {
            this.nodeSourceWindow.setCreatedFromImport();
            this.nodeSourceWindow.importNodeSourceFromJson(submitResult);
            this.nodeSourceWindow.resetCreatedFromImport();
        } catch (Exception e) {
            this.nodeSourceWindow.setNodeSourceWindowLabelWithError("Import Node Source failed", e);
        }
    }

}
