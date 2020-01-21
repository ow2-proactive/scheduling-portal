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

import java.util.ArrayList;
import java.util.List;

import org.codehaus.jettison.json.JSONArray;
import org.codehaus.jettison.json.JSONException;
import org.codehaus.jettison.json.JSONObject;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.JobUsage;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.TaskUsage;


public class UsageJsonReader {

    public static List<JobUsage> readJobUsages(String jsonString) throws JSONException {
        JSONArray jsonArray = new JSONArray(jsonString);
        List<JobUsage> jobUsages = new ArrayList<>();
        for (int i = 0; i < jsonArray.length(); i++) {
            JSONObject jobUsageAsJson = jsonArray.getJSONObject(i);
            JobUsage jobUsage = read(jobUsageAsJson);
            jobUsages.add(jobUsage);
        }
        return jobUsages;
    }

    private static JobUsage read(JSONObject json) throws JSONException {
        String owner = json.getString("owner");
        String project = json.getString("project");
        String jobId = json.getString("jobId");
        String jobName = json.getString("jobName");
        long jobDuration = json.getLong("jobDuration");
        String status = json.getString("status");
        long submittedTime = json.getLong("submittedTime");
        Long parentId = null;
        if (!json.isNull("parentId")) {
            parentId = json.getLong("parentId");
        }

        JobUsage jobUsage = new JobUsage(owner, project, jobId, jobName, jobDuration, status, submittedTime, parentId);

        JSONArray tasks = json.getJSONArray("taskUsages");
        for (int i = 0; i < tasks.length(); i++) {
            JSONObject task = tasks.getJSONObject(i);
            jobUsage.add(parseJsonTask(task));
        }
        return jobUsage;
    }

    private static String nullToEmpty(JSONObject json, String propertyName) throws JSONException {
        if (json.isNull(propertyName)) {
            return "";
        } else {
            return json.getString(propertyName);
        }
    }

    private static TaskUsage parseJsonTask(JSONObject json) throws JSONException {
        String taskId = json.getString("taskId");
        String taskName = json.getString("taskName");
        long taskStartTime = json.getLong("taskStartTime");
        long taskFinishedTime = json.getLong("taskFinishedTime");
        long taskExecutionDuration = json.getLong("taskExecutionDuration");
        long taskNodeNumber = json.getInt("taskNodeNumber");
        String taskStatus = json.getString("taskStatus");
        String taskTag = nullToEmpty(json, "taskTag");
        String taskDescription = json.getString("taskDescription");
        String executionHostName = json.getString("executionHostName");
        int numberOfExecutionLeft = json.getInt("numberOfExecutionLeft");
        int numberOfExecutionOnFailureLeft = json.getInt("numberOfExecutionOnFailureLeft");
        int maxNumberOfExecution = json.getInt("maxNumberOfExecution");
        int maxNumberOfExecutionOnFailure = json.getInt("maxNumberOfExecutionOnFailure");

        return new TaskUsage(taskId,
                             taskName,
                             taskStartTime,
                             taskFinishedTime,
                             taskExecutionDuration,
                             taskNodeNumber,
                             taskStatus,
                             taskTag,
                             taskDescription,
                             executionHostName,
                             numberOfExecutionLeft,
                             numberOfExecutionOnFailureLeft,
                             maxNumberOfExecution,
                             maxNumberOfExecutionOnFailure);
    }
}
