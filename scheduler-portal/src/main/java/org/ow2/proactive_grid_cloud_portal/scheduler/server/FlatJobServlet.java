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
import org.apache.commons.io.IOUtils;
import org.ow2.proactive_grid_cloud_portal.common.shared.RestServerException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


/**
 * Job submission servlet using a flat command file
 * 
 * 
 * @author mschnoor
 *
 */
@SuppressWarnings("serial")
public class FlatJobServlet extends HttpServlet {

    private static final Logger LOGGER = LoggerFactory.getLogger(FlatJobServlet.class);

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) {
        upload(request, response);
    }

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) {
        upload(request, response);
    }

    private void upload(HttpServletRequest request, HttpServletResponse response) {
        response.setContentType("text/html");

        DiskFileItemFactory factory = new DiskFileItemFactory();
        factory.setSizeThreshold(4096);
        factory.setRepository(new File(System.getProperty("java.io.tmpdir")));

        ServletFileUpload upload = new ServletFileUpload(factory);
        upload.setSizeMax(1000000);
        String callbackName = null;

        try {
            List<?> fileItems = upload.parseRequest(request);
            Iterator<?> i = fileItems.iterator();

            String commandFile = null;
            String name = null;
            String selectionScript = null;
            String selectionScriptExtension = null;
            String sessionId = null;

            while (i.hasNext()) {
                FileItem fi = (FileItem) i.next();

                if (fi.isFormField()) {
                    if (fi.getFieldName().equals("jobName")) {
                        name = fi.getString();
                        if (name.trim().length() == 0)
                            name = null;
                    } else if (fi.getFieldName().equals("sessionId")) {
                        sessionId = fi.getString();
                    } else if (fi.getFieldName().equals("flatCallback")) {
                        callbackName = fi.getString();
                    }
                } else {
                    if (fi.getFieldName().equals("commandFile")) {
                        commandFile = IOUtils.toString(fi.getInputStream());
                        if (commandFile.trim().length() == 0)
                            commandFile = null;
                    } else if (fi.getFieldName().equals("selectionScript")) {
                        if (fi.getName().indexOf('.') == -1) {
                            selectionScriptExtension = "js";
                        } else {
                            selectionScriptExtension = fi.getName().substring(fi.getName().lastIndexOf('.') + 1);
                        }
                        selectionScript = IOUtils.toString(fi.getInputStream());
                        if (selectionScript.trim().length() == 0)
                            selectionScript = null;
                    }
                }
            }

            String ret;
            if (commandFile == null) {
                ret = "{ \"errorMessage\" : \"Missing parameter: command file\" }";
            } else if (sessionId == null) {
                ret = "{ \"errorMessage\" : \"Missing parameter: sessionId\" }";
            } else if (name == null) {
                ret = "{ \"errorMessage\" : \"Missing parameter: job name\" }";
            } else {
                ret = ((SchedulerServiceImpl) SchedulerServiceImpl.get()).submitFlatJob(sessionId,
                                                                                        commandFile,
                                                                                        name,
                                                                                        selectionScript,
                                                                                        selectionScriptExtension);
            }
            /*
             * writing the callback name in as an inlined script,
             * so that the browser, upon receiving it, will evaluate
             * the JS and call the function
             */
            response.getWriter().write("<script type='text/javascript'>");
            response.getWriter().write("window.top." + callbackName + "(" + ret + ");");
            response.getWriter().write("</script>");

        } catch (RestServerException e) {
            try {
                response.getWriter().write("<script type='text/javascript'>");
                response.getWriter().write("window.top." + callbackName + " (" + e.getMessage() + ")");
                response.getWriter().write("</script>");
            } catch (Throwable e1) {
                LOGGER.warn("Failed to write script back to client", e);
            }
        } catch (Exception e) {
            try {
                String tw = "<script type='text/javascript'>";
                tw += "window.top." + callbackName + "({ \"errorMessage\" : \"" + e.getMessage() + "\" });";
                tw += "</script>";
                response.getWriter().write(tw);
            } catch (IOException e1) {
                LOGGER.warn("Failed to write script back to client", e);
            }
        }
    }
}
