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
package org.ow2.proactive_grid_cloud_portal.scheduler.server;

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
import org.apache.commons.io.FileUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


/**
 * Servlet invoked when a filter configuration is imported from a file. The request
 * content is expected to be in JSON format, containing the configuration of a
 * filter.
 */
public class ImportFilterServlet extends HttpServlet {

    private static final Logger LOGGER = LoggerFactory.getLogger(ImportFilterServlet.class);

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) {
        readFilterConfigurationFile(request, response);
    }

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) {
        readFilterConfigurationFile(request, response);
    }

    private void readFilterConfigurationFile(HttpServletRequest request, HttpServletResponse response) {
        response.setContentType("application/json");
        File job = null;

        try {
            DiskFileItemFactory factory = new DiskFileItemFactory();
            factory.setSizeThreshold(4096);
            factory.setRepository(new File(System.getProperty("java.io.tmpdir")));

            ServletFileUpload upload = new ServletFileUpload(factory);
            List<?> fileItems = upload.parseRequest(request);
            Iterator<?> i = fileItems.iterator();

            while (i.hasNext()) {
                FileItem fi = (FileItem) i.next();
                job = File.createTempFile("filter_upload", ".json");
                fi.write(job);
                fi.delete();
            }
            writeResponse(job, response);

        } catch (Exception e) {
            LOGGER.error("Error uploading the filter configuration", e.getMessage());
        } finally {
            if (job != null)
                job.delete();
        }
    }

    private void writeResponse(File job, HttpServletResponse response) {
        try {
            String fileContent = FileUtils.readFileToString(job);
            response.getWriter().write(fileContent);
        } catch (IOException e) {
            LOGGER.error("Failed to return the filter file content to the client", e);
        }
    }
}
