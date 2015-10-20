package org.ow2.proactive_grid_cloud_portal.scheduler.client.json;

import java.util.ArrayList;
import java.util.List;

import org.ow2.proactive_grid_cloud_portal.common.client.json.JSONException;
import org.ow2.proactive_grid_cloud_portal.common.client.json.JSONUtils;
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
        
        JSONValue jsonTasksValue = jsonTasksTotal.get("tasks");
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
    
    
}
