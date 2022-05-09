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
import java.io.PrintWriter;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVPrinter;
import org.ow2.proactive_grid_cloud_portal.common.server.Service;
import org.ow2.proactive_grid_cloud_portal.common.shared.RestServerException;
import org.ow2.proactive_grid_cloud_portal.common.shared.ServiceException;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.JobUsage;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.TaskUsage;


public class ExportUsageServlet extends HttpServlet {

    private static final String ISO_8601_FORMAT = "yyyy-MM-dd'T'HH:mm:ss.SSSz";

    private static final String LINE_SEPARATOR = "\n";

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException {
        try {
            String sessionId = request.getParameter("sessionId");
            SimpleDateFormat formatter = new SimpleDateFormat(ISO_8601_FORMAT);
            String user = request.getParameter("user");
            Date startDate = getDateParameter(request, formatter, "startDate");
            Date endDate = getDateParameter(request, formatter, "endDate");

            String csvContent = csvExport(sessionId, user, startDate, endDate);

            response.setContentType("text/csv");
            response.setHeader("Content-Disposition", "attachment; filename=\"SchedulerUsage.csv\"");
            PrintWriter writer = response.getWriter();
            writer.write(csvContent);
            writer.flush();
            writer.close();
        } catch (ParseException e) {
            response.sendError(HttpServletResponse.SC_BAD_REQUEST, "Dates parameter should use ISO 8601 format");
        } catch (ServiceException e) {
            response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR,
                               "Failed to retrieve usage data: " + e.getMessage());
        } catch (RestServerException e) {
            response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR,
                               "Failed to retrieve usage data: " + e.getMessage());
        }
    }

    private Date getDateParameter(HttpServletRequest request, SimpleDateFormat formatter, String parameterName)
            throws ParseException {
        return formatter.parse(request.getParameter(parameterName));
    }

    private String csvExport(String sessionId, String user, Date startDate, Date endDate)
            throws ServiceException, RestServerException, IOException {
        Object[] header = { "Owner", "Tenant", "Project", "Job Id", "Parent Job Id", "Job Status", "Job Name",
                            "Submitted Time", "Job Duration", "Task Id", "Task Name", "Task Status", "Task Tag",
                            "Task Execution Host Name", "Task Node Number", "Executions", "Node Failures",
                            "Task Start Time", "Task Finished Time", "Task Duration", "Task Description" };
        List<JobUsage> jobUsages = ((SchedulerServiceImpl) Service.get()).getUsage(sessionId, user, startDate, endDate);
        StringBuilder sb = new StringBuilder();
        CSVFormat csvFileFormat = CSVFormat.DEFAULT.withRecordSeparator(LINE_SEPARATOR);
        CSVPrinter csvFilePrinter = new CSVPrinter(sb, csvFileFormat);
        csvFilePrinter.printRecord(header);
        for (JobUsage jobUsage : jobUsages) {
            for (TaskUsage taskUsage : jobUsage.getTaskUsages()) {
                csvFilePrinter.printRecord(jobUsage.getOwner(),
                                           jobUsage.getTenant() == null ? "" : jobUsage.getTenant(),
                                           jobUsage.getProject(),
                                           jobUsage.getJobId(),
                                           jobUsage.getParentId(),
                                           jobUsage.getStatus(),
                                           jobUsage.getJobName(),
                                           jobUsage.getSubmittedTime(),
                                           jobUsage.getJobDuration(),
                                           taskUsage.getTaskId(),
                                           taskUsage.getTaskName(),
                                           taskUsage.getTaskStatus(),
                                           taskUsage.getTaskTag() == null ? "" : taskUsage.getTaskTag(),
                                           taskUsage.getExecutionHostName(),
                                           taskUsage.getTaskNodeNumber(),
                                           (taskUsage.getMaxNumberOfExecution() -
                                            taskUsage.getNumberOfExecutionLeft()) + " / " +
                                                                          taskUsage.getMaxNumberOfExecution(),
                                           (taskUsage.getMaxNumberOfExecutionOnFailure() -
                                            taskUsage.getNumberOfExecutionOnFailureLeft()) + " / " + taskUsage.getMaxNumberOfExecutionOnFailure(),
                                           taskUsage.getTaskStartTime(),
                                           taskUsage.getTaskFinishedTime(),
                                           taskUsage.getTaskExecutionDuration(),
                                           taskUsage.getTaskDescription());
            }
        }

        csvFilePrinter.close();
        return sb.toString();
    }
}
