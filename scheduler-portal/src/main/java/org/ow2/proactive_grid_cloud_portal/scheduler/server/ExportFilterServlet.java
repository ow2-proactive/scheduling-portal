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

        import java.io.PrintWriter;

        import javax.servlet.http.HttpServlet;
        import javax.servlet.http.HttpServletRequest;
        import javax.servlet.http.HttpServletResponse;

        import org.slf4j.Logger;
        import org.slf4j.LoggerFactory;


/**
 * Servlet invoked when a job filter configuration is exported to a file. The request
 * content is expected to be in JSON format, containing the configuration of a
 * filter.
 */
public class ExportFilterServlet extends HttpServlet {

    private static final Logger LOGGER = LoggerFactory.getLogger(ExportFilterServlet.class);

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) {
        downloadFilterJsonFile(request, response);
    }

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) {
        downloadFilterJsonFile(request, response);
    }

    private void downloadFilterJsonFile(HttpServletRequest request, HttpServletResponse response) {
        try {
            String filterConfigJson = request.getParameter("filterConfigJson");
            String filterName = request.getParameter("filterName");

            response.setContentType("application/json");
            response.setHeader("Content-Disposition", "attachment; filename=\"" + filterName + ".json\"");

            PrintWriter writer = response.getWriter();
            writer.write(filterConfigJson);
            writer.flush();
            writer.close();
        } catch (Exception e) {
            LOGGER.error("Error exporting the filter configuration as a JSON file", e.getMessage());
        }

    }
}
