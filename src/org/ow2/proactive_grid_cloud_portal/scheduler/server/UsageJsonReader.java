/*
 *  *
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
 *  * $$PROACTIVE_INITIAL_DEV$$
 */
package org.ow2.proactive_grid_cloud_portal.scheduler.server;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.JobUsage;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.TaskUsage;

import java.util.ArrayList;
import java.util.List;

import org.codehaus.jettison.json.JSONArray;
import org.codehaus.jettison.json.JSONException;
import org.codehaus.jettison.json.JSONObject;

public class UsageJsonReader {

    public static List<JobUsage> readJobUsages(String jsonString) throws JSONException {
        JSONArray jsonArray = new JSONArray(jsonString);
        List<JobUsage> jobUsages = new ArrayList<JobUsage>();
        for (int i = 0; i < jsonArray.length(); i++) {
            JSONObject jobUsageAsJson = jsonArray.getJSONObject(i);
            JobUsage jobUsage = read(jobUsageAsJson);
            jobUsages.add(jobUsage);
        }
        return jobUsages;
    }

    private static JobUsage read(JSONObject json) throws JSONException {
        String jobId = json.getString("jobId");
        String jobName = json.getString("jobName");
        long jobDuration = json.getLong("jobDuration");

        JobUsage jobUsage = new JobUsage(jobId, jobName, jobDuration);

        JSONArray tasks = json.getJSONArray("taskUsages");
        for (int i = 0; i < tasks.length(); i++) {
            JSONObject task = tasks.getJSONObject(i);
            jobUsage.add(parseJsonTask(task));
        }
        return jobUsage;
    }

    private static TaskUsage parseJsonTask(JSONObject json) throws JSONException {
        String taskId = json.getString("taskId");
        String taskName = json.getString("taskName");
        long taskStartTime = json.getLong("taskStartTime");
        long taskFinishedTime = json.getLong("taskFinishedTime");
        long taskExecutionDuration = json.getLong("taskExecutionDuration");
        long taskNodeNumber = json.getInt("taskNodeNumber");
        return new TaskUsage(taskId, taskName, taskStartTime, taskFinishedTime, taskExecutionDuration, taskNodeNumber);
    }
}
