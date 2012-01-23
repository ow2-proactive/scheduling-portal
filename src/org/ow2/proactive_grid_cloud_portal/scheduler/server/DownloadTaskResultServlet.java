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

import java.io.IOException;
import java.io.InputStream;

import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ow2.proactive_grid_cloud_portal.common.client.Controller;
import org.ow2.proactive_grid_cloud_portal.common.server.Service;


/**
 * The servlet which is called when the result of a task is wanted to be downloaded
 * @author ahagea
 *
 */
@SuppressWarnings("serial")
public class DownloadTaskResultServlet extends HttpServlet {

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
		String media = request.getParameter("media");
		String sessionId = request.getParameter("sessionId");

		InputStream is = null;
		ServletOutputStream out = null;
		try {

			response.setContentType(media);
			if (!media.equals("text/plain")) {
				response.setHeader("Content-disposition", "attachment; filename=job" + jobId + "_" + taskId +
					"_result");
			}
			response.setHeader("Location", "job" + jobId + "_" + taskId + ".result");

			out = response.getOutputStream();

			is = ((SchedulerServiceImpl) Service.get()).getTaskResult(sessionId, jobId, taskId);

			int buf;
			while ((buf = is.read()) != -1) {
				out.write(buf);
			}

		} catch (Throwable t) {
			t.printStackTrace();
			String str = "Failed to download result: " + Controller.getJsonErrorMessage(t);
			out.write(str.getBytes());
		} finally {
			if (is != null)
				is.close();
			out.flush();
			out.close();
		}
	}
}
