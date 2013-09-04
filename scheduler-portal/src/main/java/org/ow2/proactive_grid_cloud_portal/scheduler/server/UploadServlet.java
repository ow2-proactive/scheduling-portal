/*
 * ################################################################
 *
 * ProActive Parallel Suite(TM): The Java(TM) library for
 *    Parallel, Distributed, Multi-Core Computing for
 *    Enterprise Grids & Clouds
 *
 * Copyright (C) 1997-2011 INRIA/University of
 *                 Nice-Sophia Antipolis/ActiveEon
 * Contact: proactive@ow2.org or contact@activeeon.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Affero General Public License
 * as published by the Free Software Foundation; version 3 of
 * the License.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
 * USA
 *
 * If needed, contact us to obtain a release under GPL Version 2 or 3
 * or a different license than the AGPL.
 *
 *  Initial developer(s):               The ProActive Team
 *                        http://proactive.inria.fr/team_members.htm
 *  Contributor(s):
 *
 * ################################################################
 * $$PROACTIVE_INITIAL_DEV$$
 */
package org.ow2.proactive_grid_cloud_portal.scheduler.server;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.Iterator;
import java.util.List;
import java.util.jar.JarFile;

import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.ow2.proactive_grid_cloud_portal.common.server.Service;
import com.google.gwt.user.server.Base64Utils;
import org.apache.commons.fileupload.FileItem;
import org.apache.commons.fileupload.disk.DiskFileItemFactory;
import org.apache.commons.fileupload.servlet.ServletFileUpload;
import org.apache.commons.io.IOUtils;
import org.w3c.dom.Document;


/**
 * Default job submission servlet
 * <p>
 * 
 * form must be multipart and contain:
 * . one file, name ignored, must be a valid XML job descriptor or a Job ARchive (a ZIP file
 *   containing a specific file hierarchy).
 * . one form field named 'sessionId' used to connect to the server
 * . one optional form field named 'edit'. if edit == "1",
 *   the servlet does not submit the job, but simply writes the following string as application/json response:
 *   { "jobEdit" : "<DESC_64>" }
 *   where <DESC_64> is a base64 encoded version of the job sent as parameter.
 *   This will allow client to edit the descriptor, as javascript runtimes are not allow to open local files.
 *   Edition of a job descriptor contained in an archive is not supported.
 */
@SuppressWarnings("serial")
public class UploadServlet extends HttpServlet {

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
        File job = null;

        try {
            DiskFileItemFactory factory = new DiskFileItemFactory();
            factory.setSizeThreshold(4096);
            factory.setRepository(new File(System.getProperty("java.io.tmpdir")));

            ServletFileUpload upload = new ServletFileUpload(factory);
            upload.setSizeMax(1000000);

            List<?> fileItems = upload.parseRequest(request);
            Iterator<?> i = fileItems.iterator();

            String sessionId = null;
            boolean edit = false;

            /* 
             * * edit=0, simply submit the job descriptor
             * * edit=1, open the descriptor and return it as a string 
             */

            while (i.hasNext()) {
                FileItem fi = (FileItem) i.next();

                if (fi.isFormField()) {
                    if (fi.getFieldName().equals("sessionId")) {
                        sessionId = fi.getString();
                    } else if (fi.getFieldName().equals("edit")) {
                        if (fi.getString().equals("1")) {
                            edit = true;
                        } else {
                            edit = false;
                        }
                    }
                } else {
                    String fileName = fi.getName();
                    job = new File(System.getProperty("java.io.tmpdir"), fileName);
                    fi.write(job);
                }

                fi.delete();
            }

            boolean isJar = false;
            try {
                JarFile jf = new JarFile(job);
                isJar = (jf != null);
            } catch (IOException e) {
                // not a jar;
            }

            if (!isJar && job != null) {
                // this _loosely_ checks that the file we got is an XML file 
                try {
                    DocumentBuilderFactory docFactory = DocumentBuilderFactory.newInstance();
                    DocumentBuilder docBuilder = docFactory.newDocumentBuilder();
                    Document doc = docBuilder.parse(job);

                    if (edit) {
                        // don't go on with edition if there are no variables
                        if (doc.getElementsByTagName("variables").getLength() < 1 ||
                            doc.getElementsByTagName("variable").getLength() < 1) {
                            response
                                    .getWriter()
                                    .write(
                                            "This job descriptor contains no variable definition.<br>"
                                                + "Uncheck <strong>Edit variables</strong> or submit another descriptor.");
                            return;
                        }
                    }
                } catch (Throwable e) {
                    response.getWriter().write("Job descriptor must be valid XML<br>" + e.getMessage());
                    return;
                }
            }

            if (edit && !isJar) {
                String ret = IOUtils.toString(new FileInputStream(job), "UTF-8");
                response.getWriter().write(
                        "{ \"jobEdit\" : \"" + Base64Utils.toBase64(ret.getBytes()) + "\" }");
            } else {
                String responseS = ((SchedulerServiceImpl) Service.get()).submitXMLFile(sessionId, job);
                if (responseS == null || responseS.length() == 0) {
                    response.getWriter().write("Job submission returned without a value!");
                } else {
                    response.getWriter().write(responseS);
                }
            }

        } catch (Exception e) {
            try {
                String msg = e.getMessage().replace("<", "&lt;").replace(">", "&gt;");
                response.getWriter().write(msg);
            } catch (IOException e1) {
            }
        } finally {
            if (job != null)
                job.delete();
        }

    }

}
