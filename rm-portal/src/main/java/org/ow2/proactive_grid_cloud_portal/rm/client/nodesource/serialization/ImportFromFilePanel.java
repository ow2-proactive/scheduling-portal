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

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.ui.FileUpload;
import com.google.gwt.user.client.ui.FormPanel;


public class ImportFromFilePanel extends FormPanel {

    static final String FILE_OPTION_NAME = "Import from File";

    ImportFromFilePanel(ImportNodeSourceLayout importNodeSourceLayout) {
        setEncoding(FormPanel.ENCODING_MULTIPART);
        setMethod(FormPanel.METHOD_POST);
        setAction(GWT.getModuleBaseURL() + SerializationType.IMPORT_NODE_SOURCE_FROM_FILE);
        add(getFileUploadItem());
        addSubmitCompleteHandler(importNodeSourceLayout::handleNodeSourceImport);
    }

    private FileUpload getFileUploadItem() {
        FileUpload fileUpload = new FileUpload();
        fileUpload.setName(FILE_OPTION_NAME);
        fileUpload.addChangeHandler(onFileChosen -> {
            if (!fileUpload.getFilename().isEmpty()) {
                submit();
            }
        });
        return fileUpload;
    }

}
