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
package org.ow2.proactive_grid_cloud_portal.common.server;

import org.apache.commons.io.FileUtils;
import org.apache.http.client.HttpClient;
import org.apache.http.client.ResponseHandler;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.BasicResponseHandler;
import org.apache.http.impl.client.DefaultHttpClient;
import org.ow2.proactive_grid_cloud_portal.common.shared.Config;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.File;
import java.io.IOException;


/**
 * Return the content of the motd.txt file,
 * of the reponse of the *.motd.url if defined 
 * 
 * @author mschnoor
 *
 */
@SuppressWarnings("serial")
public class MotdServlet extends HttpServlet {

    private static final Logger LOGGER = LoggerFactory.getLogger(MotdServlet.class);

    private static long lastModified = 0L;
    private static String fileContent = "";
    private static final String motdFileName = "motd.txt";

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) {

        response.setContentType("text/html");

        try {

            String url = Config.get().getMotdUrl();

            // no MOTD URL : use local file
            if (url == null || url.trim().length() == 0) {

                File f = new File(this.getServletContext().getRealPath(motdFileName));
                long ft = f.lastModified();
                if (ft != lastModified) {
                    lastModified = ft;
                    try {
                        fileContent = FileUtils.readFileToString(f);
                    } catch (IOException e) {
                        LOGGER.debug("Failed to read MOTD file", e);
                        response.getWriter().write("");
                        return;
                    }
                }
                response.getWriter().write(fileContent);

            } else {
                HttpClient httpclient = new DefaultHttpClient();

                try {
                    HttpGet httpget = new HttpGet(url);

                    ResponseHandler<String> responseHandler = new BasicResponseHandler();
                    String responseBody = httpclient.execute(httpget, responseHandler);
                    response.setStatus(200);
                    response.getWriter().write(responseBody);
                }
                catch (Exception e) {
                    response.setStatus(500);
                    response.getWriter().write("Server error");
                } finally {
                    httpclient.getConnectionManager().shutdown();
                }
            }

        } catch (IOException e) {
            LOGGER.debug("Failed to provide MOTD file", e);
        }
    }
}
