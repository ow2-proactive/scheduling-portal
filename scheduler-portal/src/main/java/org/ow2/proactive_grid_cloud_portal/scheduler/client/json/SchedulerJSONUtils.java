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
package org.ow2.proactive_grid_cloud_portal.scheduler.client.json;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

import org.ow2.proactive_grid_cloud_portal.common.client.json.JSONException;
import org.ow2.proactive_grid_cloud_portal.common.client.json.JSONUtils;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Job;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Task;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.JobsPaginationModel;

import com.google.gwt.json.client.JSONArray;
import com.google.gwt.json.client.JSONNumber;
import com.google.gwt.json.client.JSONObject;
import com.google.gwt.json.client.JSONValue;


/**
 * JSON utils to parse data from the scheduler server.
 * @author The activeeon team
 *
 */
public class SchedulerJSONUtils extends JSONUtils {

    private static Logger LOGGER = Logger.getLogger(SchedulerJSONUtils.class.getName());

    /**
     * Parse a paginated list of tasks
     * @param jsonString the JSON as a string representing the paginated list of tasks.
     * @return An object wrapping the list of tasks and the total number of tasks without pagination.
     * @throws JSONException if it fails to parse the JSON.
     */
    public static JSONPaginatedTasks parseJSONPaginatedTasks(String jsonString) throws JSONException {
        JSONValue val = parseJSON(jsonString);
        return parseJSONPaginatedTasks(val);
    }

    /**
     * Parse a paginated list of jobs
     * @param jsonString the JSON as a string representing the paginated list of tasks.
     * @param paginationController 
     * @return An object wrapping the list of tasks and the total number of tasks without pagination.
     * @throws JSONException if it fails to parse the JSON.
     */
    public static Map<Integer, Job> parseJSONJobs(String jsonString, JobsPaginationModel paginationModel)
            throws JSONException {
        JSONValue val = parseJSON(jsonString);
        return getJobsFromJson(val, paginationModel);
    }

    protected static JSONObject getObject(JSONValue value) throws JSONException {
        JSONObject jsonObject = value.isObject();
        if (jsonObject == null) {
            throw new JSONException("Expected JSON Object: " + value.toString());
        }
        return jsonObject;
    }

    protected static JSONValue getProperty(JSONObject obj, String propertyName) throws JSONException {
        JSONValue jsonValue = obj.get(propertyName);
        if (jsonValue == null) {
            throw new JSONException("Expected JSON Object with attribute " + propertyName + ": " + obj.toString());
        }
        return jsonValue;
    }

    protected static JSONArray getArray(JSONValue value) throws JSONException {
        JSONArray arr = value.isArray();
        if (arr == null) {
            throw new JSONException("Expected JSON Array: " + value.toString());
        }
        return arr;
    }

    protected static long getSize(JSONObject obj) throws JSONException {
        JSONValue jsonTotalValue = obj.get("size");
        if (jsonTotalValue == null) {
            throw new JSONException("Expected JSON Object with attribute total: " + obj.toString());
        }
        JSONNumber jsonTotal = jsonTotalValue.isNumber();
        if (jsonTotal == null) {
            throw new JSONException("Expected JSON number: " + jsonTotalValue.toString());
        }
        return (long) jsonTotal.doubleValue();
    }

    /**
     * Parse a paginated list of tasks
     * @param jsonString the JSON representing the paginated list of tasks.
     * @return An object wrapping the list of tasks and the total number of tasks without pagination.
     * @throws JSONException if it fails to parse the JSON.
     */
    public static JSONPaginatedTasks parseJSONPaginatedTasks(JSONValue value) throws JSONException {
        JSONObject jsonTasksTotal = getObject(value);

        JSONArray arr = getArray(getProperty(jsonTasksTotal, "list"));

        List<Task> tasks = new ArrayList<Task>();

        for (int i = 0; i < arr.size(); i++) {
            JSONObject jsonTask = arr.get(i).isObject();
            tasks.add(Task.parseJson(jsonTask));
        }

        long totalTasks = getSize(jsonTasksTotal);

        JSONPaginatedTasks result = new JSONPaginatedTasks(tasks, totalTasks);
        return result;
    }

    /**
     * @param arr list of tags as a JSON array
     * @return the list of tags
     */
    public static List<String> getTagsFromJson(String result) throws JSONException {
        JSONValue val = parseJSON(result);
        JSONArray arr = val.isArray();
        if (arr == null) {
            throw new JSONException("Expected JSON Array: " + val.toString());
        }

        List<String> tags = new ArrayList<String>(arr.size());

        for (int i = 0; i < arr.size(); i++) {
            tags.add(arr.get(i).isString().stringValue());
        }

        return tags;
    }

    private static void setPageInfoFromJson(JSONObject jsonPageInfo, JobsPaginationModel paginationModel)
            throws JSONException {
        paginationModel.setCurrentEndCursor(getProperty(jsonPageInfo, "endCursor").isString().stringValue());
        paginationModel.setCurrentStartCursor(getProperty(jsonPageInfo, "startCursor").isString().stringValue());

        if (paginationModel.getEndCursor() != null)
            paginationModel.setHasNextPage(true);
        else
            paginationModel.setHasNextPage(getProperty(jsonPageInfo, "hasNextPage").isBoolean().booleanValue());

        if (paginationModel.getStartCursor() != null)
            paginationModel.setHasPreviousPage(true);
        else
            paginationModel.setHasPreviousPage(getProperty(jsonPageInfo, "hasPreviousPage").isBoolean().booleanValue());
    }

    private static Map<Integer, Job> getJobsFromJson(JSONValue value, JobsPaginationModel paginationModel)
            throws JSONException {
        Map<Integer, Job> jobs = new HashMap<>();

        JSONObject jsonMain = getObject(value);
        JSONObject jsonData = getObject(getProperty(jsonMain, "data"));
        JSONObject jsonJobs = getObject(getProperty(jsonData, "jobs"));
        JSONObject jsonPageInfo = getObject(getProperty(jsonJobs, "pageInfo"));
        setPageInfoFromJson(jsonPageInfo, paginationModel);
        JSONArray jsonEdges = getArray(getProperty(jsonJobs, "edges"));

        for (int i = 0; i < jsonEdges.size(); i++) {
            JSONObject jsonJob = jsonEdges.get(i).isObject();
            Job j = Job.parseJson(jsonJob);
            jobs.put(j.getId(), j);
        }

        return jobs;
    }

    public static Job getJobInfoFromJson(String jsonString) throws JSONException {
        JSONValue val = parseJSON(jsonString);
        JSONObject jsonJobInfo = val.isObject();
        if (jsonJobInfo == null) {
            throw new JSONException("Expected JSON Object: " + jsonString);
        }

        return Job.parseJSONInfo(jsonJobInfo);
    }
}
