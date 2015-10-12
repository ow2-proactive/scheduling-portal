/*
 * ################################################################
 *
 * ProActive Parallel Suite(TM): The Java(TM) library for
 *    Parallel, Distributed, Multi-Core Computing for
 *    Enterprise Grids & Clouds
 *
 * Copyright (C) 1997-2015 INRIA/University of
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
package org.ow2.proactive_grid_cloud_portal.common.server;

import java.io.File;
import java.io.IOException;
import java.util.Iterator;
import java.util.List;

import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.google.gwt.safehtml.shared.SafeHtmlUtils;
import org.apache.commons.fileupload.FileItem;
import org.apache.commons.fileupload.disk.DiskFileItemFactory;
import org.apache.commons.fileupload.servlet.ServletFileUpload;
import org.apache.commons.io.IOUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


/**
 * Login servlet for Credential authentication
 * <p>
 * It's easier to pass file parameters such as credentials using
 * a form and a servlet rather than pure javascript
 * 
 * 
 * @author mschnoor
 *
 */
@SuppressWarnings("serial")
public class LoginServlet extends HttpServlet {

    private static final Logger LOGGER = LoggerFactory.getLogger(LoginServlet.class);

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) {
        login(request, response);
    }

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) {
        login(request, response);
    }

    private void login(HttpServletRequest request, HttpServletResponse response) {
        response.setContentType("text/html");
        File cred = null;
        try {
            DiskFileItemFactory factory = new DiskFileItemFactory();
            factory.setSizeThreshold(4096);
            factory.setRepository(new File(System.getProperty("java.io.tmpdir")));
            ServletFileUpload upload = new ServletFileUpload(factory);
            upload.setSizeMax(1000000);

            List<?> fileItems = upload.parseRequest(request);
            Iterator<?> i = fileItems.iterator();

            String user = "";
            String pass = "";
            String sshKey = "";

            while (i.hasNext()) {
                FileItem fi = (FileItem) i.next();

                if (fi.isFormField()) {
                    String name = fi.getFieldName();
                    String value = fi.getString();

                    if (name.equals("username")) {
                        user = value;
                    } else if (name.equals("password")) {
                        pass = value;
                    }
                } else {
                    String field = fi.getFieldName();

                    byte[] bytes = IOUtils.toByteArray(fi.getInputStream());

                    if (field.equals("credential")) {
                        cred = File.createTempFile("credential", null);
                        cred.deleteOnExit();
                        fi.write(cred);
                    } else if (field.equals("sshkey")) {
                        sshKey = new String(bytes);
                    }
                }

                fi.delete();
            }

            String responseS = Service.get().login(user, pass, cred, sshKey);
            String s = "{ \"sessionId\" : \"" + responseS + "\" }";
            response.getWriter().write(SafeHtmlUtils.htmlEscape(s));
        } catch (Throwable t) {
            try {
                response.getWriter().write(SafeHtmlUtils.htmlEscape(t.getMessage()));
            } catch (IOException e1) {
                LOGGER.warn("Failed to return login error to client, error was:" + t.getMessage(), e1);
            }
        } finally {
            if (cred != null)
                cred.delete();
        }
    }
}
