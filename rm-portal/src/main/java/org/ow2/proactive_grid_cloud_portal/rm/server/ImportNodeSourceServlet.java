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

import static org.ow2.proactive_grid_cloud_portal.rm.server.NSCreationServlet.MAX_UPLOAD_SIZE;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.fileupload.FileItem;
import org.apache.commons.fileupload.disk.DiskFileItemFactory;
import org.apache.commons.fileupload.servlet.ServletFileUpload;
import org.codehaus.jettison.json.JSONException;
import org.codehaus.jettison.json.JSONObject;
import org.codehaus.jettison.json.JSONWriter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.gwt.core.client.GWT;


/**
 * Servlet invoked when a node source is imported from a file. The request
 * content is expected to be in JSON format, containing the configuration of a
 * node source.
 */
public class ImportNodeSourceServlet extends HttpServlet {

    private static final Logger LOGGER = LoggerFactory.getLogger(ImportNodeSourceServlet.class);

    private static final String NODE_SOURCE_FILE_NAME_SUFFIX = "-configuration.json";

    public static final String SERVLET_MAPPING = "importnodesource";

    public static final String MAIN_FORM_ITEM_NAME = "nodeSourceJson";

    public static final String NODE_SOURCE_NAME_KEY = "nodeSourceName";

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) {
        LOGGER.warn("request=" + request);
        readNodeSourceConfigurationFile(request, response);
    }

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) {
        LOGGER.warn("request=" + request);
        readNodeSourceConfigurationFile(request, response);
    }

    private void readNodeSourceConfigurationFile(HttpServletRequest request, HttpServletResponse response) {
        response.setContentType("text/html");

        try {
            DiskFileItemFactory factory = new DiskFileItemFactory();
            factory.setSizeThreshold(4096);
            factory.setRepository(new File(System.getProperty("java.io.tmpdir")));
            ServletFileUpload upload = new ServletFileUpload(factory);
            upload.setSizeMax(MAX_UPLOAD_SIZE);

            List<FileItem> fileItems = upload.parseRequest(request);
            LOGGER.warn("response=" + Arrays.toString(fileItems.toArray()));
            Iterator<?> i = fileItems.iterator();
            while (i.hasNext()) {

                FileItem item = (FileItem) i.next();
                String name = item.getFieldName();
                InputStream stream = item.getInputStream();

                // Process the input stream
                ByteArrayOutputStream out = new ByteArrayOutputStream();
                int len;
                byte[] buffer = new byte[8192];
                while ((len = stream.read(buffer, 0, buffer.length)) != -1) {
                    out.write(buffer, 0, len);
                }

                LOGGER.warn("response=" + new String(out.toByteArray(), StandardCharsets.UTF_8));

                response.getWriter().write(new String(out.toByteArray(), StandardCharsets.UTF_8));
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
        } catch (IOException e1) {
            e1.printStackTrace();
        } catch (JSONException e1) {
            e1.printStackTrace();
        }
    }

}
