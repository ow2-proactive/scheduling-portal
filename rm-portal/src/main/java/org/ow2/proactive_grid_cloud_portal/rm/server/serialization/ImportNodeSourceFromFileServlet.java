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
package org.ow2.proactive_grid_cloud_portal.rm.server.serialization;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringWriter;
import java.nio.charset.StandardCharsets;

import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.fileupload.FileItem;
import org.apache.commons.io.IOUtils;
import org.codehaus.jettison.json.JSONException;
import org.codehaus.jettison.json.JSONWriter;
import org.ow2.proactive_grid_cloud_portal.rm.server.ServletRequestTransformer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


/**
 * Servlet invoked when a node source is imported from a file. The request
 * content is expected to be in JSON format, containing the configuration of a
 * node source.
 */
public class ImportNodeSourceFromFileServlet extends HttpServlet {

    private static final Logger LOGGER = LoggerFactory.getLogger(ImportNodeSourceFromFileServlet.class);

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) {
        readNodeSourceConfigurationFile(request, response);
    }

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) {
        readNodeSourceConfigurationFile(request, response);
    }

    private void readNodeSourceConfigurationFile(HttpServletRequest request, HttpServletResponse response) {
        response.setContentType("text/html");
        try {
            for (FileItem formItem : new ServletRequestTransformer().getFormItems(request)) {
                try (InputStream in = formItem.getInputStream()) {
                    ByteArrayOutputStream out = new ByteArrayOutputStream();
                    IOUtils.copy(in, out);
                    response.getWriter().write(new String(out.toByteArray(), StandardCharsets.UTF_8));
                }
            }
        } catch (Exception e) {
            handleNodeSourceImportException(response, e);
            throw new RuntimeException(e);
        }
    }

    private void handleNodeSourceImportException(HttpServletResponse response, Exception e) {
        LOGGER.warn("Failed to read node source configuration file", e);
        try {
            String jsonErrorString = new JSONWriter(new StringWriter()).object()
                                                                       .key("result")
                                                                       .value(false)
                                                                       .key("errorMessage")
                                                                       .value("Failed to read node source configuration file: " +
                                                                              e.getMessage())
                                                                       .endObject()
                                                                       .toString();
            response.getWriter().write(jsonErrorString);
        } catch (IOException | JSONException e1) {
            LOGGER.warn("Failed to return error message", e);
        }
    }

}
