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
package org.ow2.proactive_grid_cloud_portal.rm.server.nodesource.serialization.export;

import static org.ow2.proactive_grid_cloud_portal.rm.shared.ExportToFileConstants.*;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.fileupload.FileItem;
import org.ow2.proactive_grid_cloud_portal.rm.server.ServletRequestTransformer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


/**
 * Servlet invoked when a node source is exported to a file. The request
 * content is expected to be in JSON format, containing the configuration of a
 * node source.
 */
public class ExportToFileServlet extends HttpServlet {

    private static final Logger LOGGER = LoggerFactory.getLogger(ExportToFileServlet.class);

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

            Map<String, String> exportParameters = new HashMap<>();
            for (FileItem requestItem : new ServletRequestTransformer().getFormItems(request)) {
                if (requestItem.isFormField()) {
                    String name = requestItem.getFieldName();
                    String value = requestItem.getString();
                    if (name.equalsIgnoreCase(FILE_CONTENT_PARAM)) {
                        exportParameters.put(FILE_CONTENT_PARAM, value);
                    } else if (name.equalsIgnoreCase(FILE_SUFFIX_PARAM)) {
                        exportParameters.put(FILE_SUFFIX_PARAM, value);
                    } else if (name.equalsIgnoreCase(NODE_SOURCE_NAME_PARAM)) {
                        exportParameters.put(NODE_SOURCE_NAME_PARAM, value);
                    }
                }
                requestItem.delete();
            }

            response.setHeader("Content-disposition",
                               "attachment; filename=" + exportParameters.get(NODE_SOURCE_NAME_PARAM) +
                                                      exportParameters.get(FILE_SUFFIX_PARAM));
            response.setHeader("Location",
                               exportParameters.get(NODE_SOURCE_NAME_PARAM) + exportParameters.get(FILE_SUFFIX_PARAM));
            response.getWriter().write(exportParameters.get(FILE_CONTENT_PARAM));
        } catch (Exception e) {
            try {
                LOGGER.warn("Export node source failed", e);
                response.getWriter().write(e.getMessage());
            } catch (IOException ioe) {
                LOGGER.warn("Failed to return node source export error to client", ioe);
            }
        }
    }

}
