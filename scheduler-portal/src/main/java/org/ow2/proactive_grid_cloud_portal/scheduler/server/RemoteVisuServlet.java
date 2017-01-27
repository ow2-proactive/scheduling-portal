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

import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;


/**
 * Creates the JNLP to download the visualization launcher
 * 
 * 
 * @author mschnoor
 *
 */
@SuppressWarnings("serial")
public class RemoteVisuServlet extends HttpServlet {

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws IOException {
        visu(request, response);
    }

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException {
        visu(request, response);
    }

    private void visu(HttpServletRequest request, HttpServletResponse response) throws IOException {

        String codebase = request.getParameter("codebase");
        String type = request.getParameter("apptype");
        String argument = request.getParameter("argument");

        ServletOutputStream out = response.getOutputStream();

        if (type == null) {
            out.println("Missing parameter : type");
            out.close();
            return;
        }
        if (argument == null) {
            out.println("Missing parameter : argument");
            out.close();
            return;
        }

        response.setContentType("application/x-java-jnlp-file");
        response.setHeader("Content-disposition", "attachment; filename=visu.jnlp");
        response.setHeader("Location", "visu.jnlp");

        String ret = "<?xml version='1.0' encoding='UTF-8'?>\n" + "<jnlp spec='1.0+' codebase='" + codebase +
                     "' href=''>\n" //
                     + "  <information>\n" //
                     + "    <title>ProActive Web Portal Remote Visualization</title>\n" + "    <vendor>INRIA</vendor>\n" // 
                     + "    <homepage href='http://proactive.inria.fr'/>\n" //
                     + "    <offline-allowed />\n" //
                     + "  </information>\n" //
                     + "  <resources>\n" //
                     + "    <j2se version='1.5+'" //
                     + "    href='http://java.sun.com/products/autodl/j2se'/>\n" //
                     + "    <jar href='visu.jar' main='true' />\n" + "  </resources>\n" //
                     + "  <security>\n" // 
                     + "    <all-permissions/>\n" // 
                     + "  </security>\n" // 
                     + "  <application-desc" //
                     + "   name='Remote Visualization'" //
                     + "   main-class='org.ow2.proactive_grid_cloud_portal.extra.RemoteViewer'>\n" //
                     + "  <argument>" + type + "</argument>\n" //
                     + "  <argument>" + argument + "</argument>\n" //
                     + "  </application-desc>\n" //
                     + "</jnlp>\n";

        out.println(ret);
        out.close();
    }
}
