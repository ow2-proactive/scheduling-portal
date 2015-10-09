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
package org.ow2.proactive_grid_cloud_portal.rm.server;

import java.io.File;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.fileupload.FileItem;
import org.apache.commons.fileupload.disk.DiskFileItemFactory;
import org.apache.commons.fileupload.servlet.ServletFileUpload;
import org.apache.commons.io.IOUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.ow2.proactive_grid_cloud_portal.common.shared.RestServerException;


/**
 * NS Creation requires reading one or multiple files from the client,
 * which cannot be done client-side
 * 
 * @author mschnoor
 *
 */
@SuppressWarnings("serial")
public class NSCreationServlet extends HttpServlet {

    private static final Logger LOGGER = LoggerFactory.getLogger(NSCreationServlet.class);

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) {
        createNs(request, response);
    }

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) {
        createNs(request, response);
    }

    private void createNs(HttpServletRequest request, HttpServletResponse response) {

        String sessionId = "";
        String callbackName = "";
        String nsName = "";
        String infra = "";
        ArrayList<String> infraParams = new ArrayList<String>();
        ArrayList<String> infraFileParams = new ArrayList<String>();
        String policy = "";
        ArrayList<String> policyParams = new ArrayList<String>();
        ArrayList<String> policyFileParams = new ArrayList<String>();
        boolean readingInfraParams = false, readingPolicyParams = false;

        try {
            DiskFileItemFactory factory = new DiskFileItemFactory();
            factory.setSizeThreshold(4096);
            factory.setRepository(new File(System.getProperty("java.io.tmpdir")));
            ServletFileUpload upload = new ServletFileUpload(factory);
            upload.setSizeMax(1000000);

            List<?> fileItems = upload.parseRequest(request);
            Iterator<?> i = fileItems.iterator();
            while (i.hasNext()) {
                FileItem fi = (FileItem) i.next();
                if (fi.isFormField()) {

                    if (fi.getFieldName().equals("sessionId")) {
                        sessionId = fi.getString();
                    } else if (fi.getFieldName().equals("nsCallback")) {
                        callbackName = fi.getString();
                    } else if (fi.getFieldName().equals("nsName")) {
                        nsName = fi.getString();
                    } else if (fi.getFieldName().equals("infra")) {
                        infra = fi.getString();
                        readingInfraParams = true;
                    } else if (fi.getFieldName().equals("policy")) {
                        policy = fi.getString();
                        readingPolicyParams = true;
                        readingInfraParams = false;
                    } else if (readingInfraParams) {
                        infraParams.add(fi.getString());
                    } else if (readingPolicyParams) {
                        policyParams.add(fi.getString());
                    } else {
                        LOGGER.warn("unexpected param " + fi.getFieldName());
                    }
                } else {
                    if (readingInfraParams) {
                        byte[] bytes = IOUtils.toByteArray(fi.getInputStream());
                        infraFileParams.add(new String(bytes));
                    } else if (readingPolicyParams) {
                        byte[] bytes = IOUtils.toByteArray(fi.getInputStream());
                        policyFileParams.add(new String(bytes));
                    } else {
                        LOGGER.warn("unexpected param " + fi.getFieldName());
                    }
                }
            }
            String failFast = null;
            if (nsName.length() == 0) {
                failFast = "You need to pick a name for the new Node Source";
            } else if (policy.length() == 0 || policy.equals("undefined")) {
                failFast = "No Policy selected";
            } else if (infra.length() == 0 || infra.equals("undefined")) {
                failFast = "No Infrastructure selected";
            }
            if (failFast != null) {
                throw new RestServerException("{ \"errorMessage\" : \"" + failFast + "\" } ");
            }

            String[] infAr = infraParams.toArray(new String[infraParams.size()]);
            String[] polAr = policyParams.toArray(new String[policyParams.size()]);
            String[] infFileAr = infraFileParams.toArray(new String[infraFileParams.size()]);
            String[] polFileAr = policyFileParams.toArray(new String[policyFileParams.size()]);

            String ret = ((RMServiceImpl) RMServiceImpl.get()).createNodeSource(sessionId, nsName, infra,
                    infAr, infFileAr, policy, polAr, polFileAr);

            if (ret.equals("true")) {
                ret = "{ \"result\" : true }";
            }

            /* writing the callback name in as an inlined script,
             * so that the browser, upon receiving it, will evaluate
             * the JS and call the function */
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
        } catch (Throwable t) {
            try {
                response.getWriter().write("<script type='text/javascript'>");
                response.getWriter()
                        .write(
                                "window.top." + callbackName + "({ \"errorMessage\" : \"" + t.getMessage() +
                                    "\" });");
                response.getWriter().write("</script>");
            } catch (Throwable e1) {
                LOGGER.warn("Failed to write script back to client", e1);
            }
        }
    }
}
