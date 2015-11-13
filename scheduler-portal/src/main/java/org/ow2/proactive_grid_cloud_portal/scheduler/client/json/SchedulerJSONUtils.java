/*
 *  *
 * ProActive Parallel Suite(TM): The Java(TM) library for
 *    Parallel, Distributed, Multi-Core Computing for
 *    Enterprise Grids & Clouds
 *
 * Copyright (C) 1997-2014 INRIA/University of
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

package org.ow2.proactive_grid_cloud_portal.scheduler.client.json;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.ow2.proactive_grid_cloud_portal.common.client.json.JSONException;
import org.ow2.proactive_grid_cloud_portal.common.client.json.JSONUtils;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Job;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Task;

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

    /**
     * Parse a paginated list of tasks
     * @param jsonString the JSON as a string representing the paginated list of tasks.
     * @return An object wrapping the list of tasks and the total number of tasks without pagination.
     * @throws JSONException if it fails to parse the JSON.
     */
    public static JSONPaginatedTasks parseJSONPaginatedTasks(String jsonString) throws JSONException{
        JSONValue val = parseJSON(jsonString);
        return parseJSONPaginatedTasks(val);
    }
    
    
    /**
     * Parse a paginated list of jobs
     * @param jsonString the JSON as a string representing the paginated list of tasks.
     * @return An object wrapping the list of tasks and the total number of tasks without pagination.
     * @throws JSONException if it fails to parse the JSON.
     */
    public static JSONPaginatedJobs parseJSONPaginatedJobs(String jsonString) throws JSONException{
        JSONValue val = parseJSON(jsonString);
        return getJobsFromJson(val);
    }
    
    
    protected static JSONObject getObject(JSONValue value) throws JSONException{
        JSONObject jsonObject = value.isObject();
        if(jsonObject == null){
            throw new JSONException("Expected JSON Object: " + value.toString());
        }
        return jsonObject;
    }
    
    
    protected static JSONValue getProperty(JSONObject obj, String propertyName) throws JSONException{
        JSONValue jsonValue = obj.get(propertyName);
        if(jsonValue == null){
            throw new JSONException("Expected JSON Object with attribute " + propertyName + ": " + obj.toString());
        }
        return jsonValue;
    }
    
    
    protected static JSONArray getArray(JSONValue value) throws JSONException{
        JSONArray arr = value.isArray();
        if (arr == null) {
            throw new JSONException("Expected JSON Array: " + value.toString());
        }
        return arr;
    }
    
    
    
    protected static long getSize(JSONObject obj) throws JSONException{
        JSONValue jsonTotalValue = obj.get("size");
        if(jsonTotalValue == null){
            throw new JSONException("Expected JSON Object with attribute total: " + obj.toString());
        }
        JSONNumber jsonTotal = jsonTotalValue.isNumber();
        if(jsonTotal == null){
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
    public static JSONPaginatedTasks parseJSONPaginatedTasks(JSONValue value) throws JSONException{
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
    public static List<String> getTagsFromJson(String result) throws JSONException{
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

    

    public static JSONPaginatedJobs getJobsFromJson(JSONValue value) throws JSONException{
        JSONPaginatedJobs resultJobs = new JSONPaginatedJobs();
        Map<Integer, Job> jobs = resultJobs.getJobs();
        
        JSONObject jsonInfo = getObject(value);
        JSONObject jsonMap = getObject(getProperty(jsonInfo, "map"));
        
        String key = jsonMap.keySet().iterator().next();
        resultJobs.setRevision(Long.parseLong(key));

        JSONArray jsonArr = getArray(getProperty(jsonMap, key));

        for (int i = 0; i < jsonArr.size(); i++) {
            JSONObject jsonJob = jsonArr.get(i).isObject();
            Job j = Job.parseJson(jsonJob);
            jobs.put(j.getId(), j);
        }

        long total = getSize(jsonInfo);
        
        resultJobs.setTotal(total);

        return resultJobs;
    }
    
    
    public static Job getJobInfoFromJson(String jsonString) throws JSONException{
        JSONValue val = parseJSON(jsonString);
        JSONObject jsonJobInfo = val.isObject();
        if(jsonJobInfo == null){
            throw new JSONException("Expected JSON Object: " + jsonString);
        }
        
        return Job.parseJSONInfo(jsonJobInfo);
    }
}
