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

import java.io.File;
import java.io.IOException;
import java.util.Iterator;
import java.util.List;

import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.fileupload.FileItem;
import org.apache.commons.fileupload.disk.DiskFileItemFactory;
import org.apache.commons.fileupload.servlet.ServletFileUpload;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


/**
 * Servlet invoked when a node source is exported to a file. The request
 * content is expected to be in JSON format, containing the configuration of a
 * node source.
 */
public class ExportNodeSourceServlet extends HttpServlet {

    private static final Logger LOGGER = LoggerFactory.getLogger(ExportNodeSourceServlet.class);

    public static final String SERVLET_MAPPING = "exportnodesource";

    public static final String MAIN_FORM_ITEM_NAME = "nodeSourceJson";

    public static final String NODE_SOURCE_FILE_NAME_SUFFIX = "-configuration.json";

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) {
        login(request, response);
    }

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) {
        login(request, response);
    }

    private void login(HttpServletRequest request, HttpServletResponse response) {
        response.setContentType("application/json");

        try {
            DiskFileItemFactory factory = new DiskFileItemFactory();
            factory.setSizeThreshold(4096);
            factory.setRepository(new File(System.getProperty("java.io.tmpdir")));
            ServletFileUpload upload = new ServletFileUpload(factory);
            upload.setSizeMax(1000000);

            List<?> fileItems = upload.parseRequest(request);
            Iterator<?> i = fileItems.iterator();

            String result = "";

            while (i.hasNext()) {
                FileItem fi = (FileItem) i.next();

                if (fi.isFormField()) {
                    String name = fi.getFieldName();
                    String value = fi.getString();

                    if (name.equals(MAIN_FORM_ITEM_NAME)) {
                        result = value;

                    }
                }

                fi.delete();
            }

            String jsonContent = result;
            response.setHeader("Content-disposition",
                               "attachment; filename=nodesourceexport" + NODE_SOURCE_FILE_NAME_SUFFIX);
            response.setHeader("Location", "nodesourceexport" + NODE_SOURCE_FILE_NAME_SUFFIX);
            response.getWriter().write(jsonContent);

        } catch (Throwable t) {
            try {
                response.getWriter().write(t.getMessage());
            } catch (IOException e1) {
                LOGGER.warn("Failed to return login error to client, error was:" + t.getMessage(), e1);
            }
        }
    }

}
