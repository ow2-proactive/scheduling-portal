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
package org.ow2.proactive_grid_cloud_portal.rm.server;

import static org.ow2.proactive_grid_cloud_portal.rm.server.ServletConfiguration.FILE_ITEM_THRESHOLD_SIZE;
import static org.ow2.proactive_grid_cloud_portal.rm.server.ServletConfiguration.MAX_FILE_UPLOAD_SIZE;

import java.io.File;
import java.io.IOException;
import java.util.List;

import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.fileupload.FileItem;
import org.apache.commons.fileupload.FileUploadException;
import org.apache.commons.fileupload.disk.DiskFileItemFactory;
import org.apache.commons.fileupload.servlet.ServletFileUpload;
import org.codehaus.jettison.json.JSONException;
import org.codehaus.jettison.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


/**
 * Servlet invoked when a node source is exported to a file. The request
 * content is expected to be in JSON format, containing the configuration of a
 * node source.
 */
public class ExportNodeSourceServlet extends HttpServlet {

    private static final Logger LOGGER = LoggerFactory.getLogger(ExportNodeSourceServlet.class);

    private static final String NODE_SOURCE_FILE_NAME_SUFFIX = "-configuration.json";

    public static final String SERVLET_MAPPING = "exportnodesource";

    public static final String MAIN_FORM_ITEM_NAME = "nodeSourceJson";

    public static final String NODE_SOURCE_NAME_KEY = "nodeSourceName";

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) {
        downloadJsonFile(request, response);
    }

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) {
        downloadJsonFile(request, response);
    }

    private void downloadJsonFile(HttpServletRequest request, HttpServletResponse response) {
        try {
            response.setContentType("application/json");
            String jsonContent = extractJsonStringFromRequest(request);
            String nodeSourceName = extractNodeSourceName(jsonContent);

            response.setHeader("Content-disposition",
                               "attachment; filename=" + nodeSourceName + NODE_SOURCE_FILE_NAME_SUFFIX);
            response.setHeader("Location", nodeSourceName + NODE_SOURCE_FILE_NAME_SUFFIX);
            response.getWriter().write(jsonContent);
        } catch (Exception e) {
            try {
                LOGGER.warn("Export node source failed", e);
                response.getWriter().write(e.getMessage());
            } catch (IOException ioe) {
                LOGGER.warn("Failed to return node source export error to client", ioe);
            }
        }
    }

    private String extractJsonStringFromRequest(HttpServletRequest request) throws FileUploadException {
        DiskFileItemFactory factory = new DiskFileItemFactory();
        factory.setSizeThreshold(FILE_ITEM_THRESHOLD_SIZE);
        factory.setRepository(new File(System.getProperty("java.io.tmpdir")));
        ServletFileUpload upload = new ServletFileUpload(factory);
        upload.setSizeMax(MAX_FILE_UPLOAD_SIZE);

        List<FileItem> requestItems = upload.parseRequest(request);
        String jsonContent = "";
        for (FileItem requestItem : requestItems) {
            if (requestItem.isFormField()) {
                String name = requestItem.getFieldName();
                String value = requestItem.getString();
                if (name.equalsIgnoreCase(MAIN_FORM_ITEM_NAME)) {
                    jsonContent = value;
                }
            }
            requestItem.delete();
        }
        return jsonContent;
    }

    private String extractNodeSourceName(String jsonContent) throws JSONException {
        JSONObject jsonObject = new JSONObject(jsonContent);
        return jsonObject.getString(NODE_SOURCE_NAME_KEY);
    }

}
