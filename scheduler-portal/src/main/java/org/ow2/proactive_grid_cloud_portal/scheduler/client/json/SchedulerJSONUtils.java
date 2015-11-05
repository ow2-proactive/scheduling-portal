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
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.ow2.proactive_grid_cloud_portal.common.client.json.JSONException;
import org.ow2.proactive_grid_cloud_portal.common.client.json.JSONUtils;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LogModel;
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
     * Parse a paginated list of tasks
     * @param jsonString the JSON representing the paginated list of tasks.
     * @return An object wrapping the list of tasks and the total number of tasks without pagination.
     * @throws JSONException if it fails to parse the JSON.
     */
    public static JSONPaginatedTasks parseJSONPaginatedTasks(JSONValue value) throws JSONException{
        JSONObject jsonTasksTotal = value.isObject();
        if(jsonTasksTotal == null){
            throw new JSONException("Expected JSON Object: " + value.toString());
        }
        
        JSONValue jsonTasksValue = jsonTasksTotal.get("list");
        if(jsonTasksValue == null){
            throw new JSONException("Expected JSON Object with attribute tasks: " + value.toString());
        }
        
        JSONArray arr = jsonTasksValue.isArray();
        if (arr == null) {
            throw new JSONException("Expected JSON Array: " + jsonTasksValue.toString());
        }
        
        List<Task> tasks = new ArrayList<Task>();

        for (int i = 0; i < arr.size(); i++) {
            JSONObject jsonTask = arr.get(i).isObject();
            tasks.add(Task.parseJson(jsonTask));
        }
        
        JSONValue jsonTotalValue = jsonTasksTotal.get("size");
        if(jsonTotalValue == null){
            throw new JSONException("Expected JSON Object with attribute total: " + value.toString());
        }
        JSONNumber jsonTotal = jsonTotalValue.isNumber();
        if(jsonTotal == null){
            throw new JSONException("Expected JSON number: " + jsonTasksValue.toString());
        }
        long totalTasks = (long) jsonTotal.doubleValue();
        
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
    
    
    
    public static JSONPaginatedJobs getJobsFromJson(String result) throws JSONException{
        JSONPaginatedJobs resultJobs = new JSONPaginatedJobs();
        Map<Integer, Job> jobs = resultJobs.getJobs();

        JSONValue jsonVal = parseJSON(result);
        JSONObject jsonInfo = jsonVal.isObject();
        if (jsonInfo == null) {
            throw new JSONException("Expected JSON Object: " + result);
        }

        String key = jsonInfo.keySet().iterator().next();
        resultJobs.setRevision(Long.parseLong(key));

        JSONArray jsonArr = jsonInfo.get(key).isArray();
        if (jsonArr == null)
            throw new JSONException("Expected JSONArray: " + jsonInfo.toString());
        
        for (int i = 0; i < jsonArr.size(); i++) {
            JSONObject jsonJob = jsonArr.get(i).isObject();
            Job j = Job.parseJson(jsonJob);
            jobs.put(j.getId(), j);
        }
        
        resultJobs.setTotal(jobs.size());
        
        return resultJobs;
    }
}
