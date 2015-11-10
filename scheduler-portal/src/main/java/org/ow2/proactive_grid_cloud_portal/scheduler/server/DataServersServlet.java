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
package org.ow2.proactive_grid_cloud_portal.scheduler.server;

import java.io.IOException;

import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;


/**
 * Creates the JNLP to download the DataServers launcher
 * 
 * 
 * @author mschnoor
 *
 */
@SuppressWarnings("serial")
public class DataServersServlet extends HttpServlet {

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws IOException {
        servers(request, response);
    }

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException {
        servers(request, response);
    }

    private void servers(HttpServletRequest request, HttpServletResponse response) throws IOException {

        String codebase = request.getParameter("codebase");

        response.setContentType("application/x-java-jnlp-file");
        response.setHeader("Content-disposition", "attachment; filename=servers.jnlp");
        response.setHeader("Location", "servers.jnlp");

        ServletOutputStream out = response.getOutputStream();
        String ret = "<?xml version='1.0' encoding='UTF-8'?>\n" + "<jnlp spec='1.0+' codebase='' href=''>\n" //
            + "  <information>\n" //
            + "    <title>ProActive Web Portal Data Servers</title>\n" + "    <vendor>INRIA</vendor>\n" // 
            + "    <homepage href='http://proactive.inria.fr'/>\n" //
            + "    <offline-allowed />\n" //
            + "  </information>\n" //
            + "  <resources>\n" //
            + "    <j2se version='1.5+'" //
            + "    href='http://java.sun.com/products/autodl/j2se'/>\n" //
            + "    <jar href='" + codebase + "servers.jar' main='true' />\n" + "  </resources>\n" //
            + "  <security>\n" // 
            + "    <all-permissions/>\n" // 
            + "  </security>\n" // 
            + "  <application-desc" //
            + "   name='Data Servers'" //
            + "   main-class='org.ow2.proactive_grid_cloud_portal.extra.DataServerLauncher'>\n" //
            + "  </application-desc>\n" //
            + "</jnlp>\n";

        out.println(ret);
        out.close();

    }
}
