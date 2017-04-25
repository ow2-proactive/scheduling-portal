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

import java.io.IOException;
import java.io.InputStream;

import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.codehaus.jettison.json.JSONObject;
import org.ow2.proactive_grid_cloud_portal.common.client.json.JSONUtils;
import org.ow2.proactive_grid_cloud_portal.common.server.Service;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


/**
 * The servlet which is called when the result of a task is wanted to be downloaded
 * @author ahagea
 *
 */
@SuppressWarnings("serial")
public class DownloadTaskResultServlet extends HttpServlet {

    private static final Logger LOGGER = LoggerFactory.getLogger(DownloadTaskResultServlet.class);

    public static final String METADATA_CONTENT_TYPE = "content.type";

    public static final String METADATA_FILE_NAME = "file.name";

    public static final String METADATA_FILE_EXTENSION = "file.extension";

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws IOException {
        download(request, response);
    }

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException {
        download(request, response);
    }

    private void download(HttpServletRequest request, HttpServletResponse response) throws IOException {
        String jobId = request.getParameter("jobId");
        String taskId = request.getParameter("taskId");
        String destination = request.getParameter("destination");
        String sessionId = request.getParameter("sessionId");

        InputStream is = null;
        ServletOutputStream out = null;
        try {

            JSONObject json = new JSONObject(((SchedulerServiceImpl) Service.get()).getTaskResultMetadata(sessionId,
                                                                                                          jobId,
                                                                                                          taskId));
            String contentType;
            if (destination.equals("file")) {
                contentType = "application/octet-stream";
            } else if (json.has(METADATA_CONTENT_TYPE)) {
                contentType = json.get(METADATA_CONTENT_TYPE).toString();
            } else {
                contentType = "text/plain";
            }
            response.setContentType(contentType);

            if (destination.equals("file")) {
                if (json.has(METADATA_FILE_NAME)) {
                    response.setHeader("Content-disposition",
                                       "attachment; filename=" + json.get(METADATA_FILE_NAME).toString());
                } else if (json.has(METADATA_FILE_EXTENSION)) {
                    response.setHeader("Content-disposition",
                                       "attachment; filename=job" + jobId + "_" + taskId + "_result" +
                                                              json.get(METADATA_FILE_EXTENSION).toString());
                } else {
                    response.setHeader("Content-disposition",
                                       "attachment; filename=job" + jobId + "_" + taskId + "_result");
                }
            }
            response.setHeader("Location", "job" + jobId + "_" + taskId + ".result");

            out = response.getOutputStream();
            if (contentType.equals("text/plain")) {
                is = ((SchedulerServiceImpl) Service.get()).getTaskResult(sessionId, jobId, taskId);
            } else {
                is = ((SchedulerServiceImpl) Service.get()).getTaskSerializedResult(sessionId, jobId, taskId);
            }

            int buf;
            while ((buf = is.read()) != -1) {
                out.write(buf);
            }

        } catch (Throwable t) {
            LOGGER.warn("Failed to download result", t);
            String str = "Failed to download result: " + JSONUtils.getJsonErrorMessage(t);
            out.write(str.getBytes());
        } finally {
            if (is != null)
                is.close();
            out.flush();
            out.close();
        }
    }
}
