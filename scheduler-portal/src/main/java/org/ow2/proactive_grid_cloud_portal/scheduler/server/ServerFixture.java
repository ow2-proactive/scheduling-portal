package org.ow2.proactive_grid_cloud_portal.scheduler.server;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import objectFaker.ConflictTypeGeneratorException;
import objectFaker.DataFaker;
import objectFaker.NoSuchPropertyException;
import objectFaker.propertyGenerator.RandomDatePropertyGenerator;
import objectFaker.propertyGenerator.RandomLongPropertyGenerator;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.Task;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.TaskStatus;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;

public class ServerFixture {

    private static ServerFixture instance;
    
    protected List<Task> tasks;
    
    public static ServerFixture getInstance(){
        if(instance == null){
            instance = new ServerFixture();
        }
        
        return instance;
    }
    
    protected enum State{
        PENDING,
        RUNNING,
        FINISHED;
    }
    
    
    protected ServerFixture(){
      //build the mock
        DataFaker<Task> taskFaker = new DataFaker<>(Task.class);
        long end = new Date().getTime();
        long start1 = end - (1000 * 60 * 60 * 24 * 14);
        long start2 = end - (1000 * 60 * 60 * 24 * 7);
        try {
            taskFaker.setGenerator("startTime", new RandomDatePropertyGenerator(start1, start2));
            taskFaker.setGenerator("finishTime", new RandomDatePropertyGenerator(start2, end));
            taskFaker.setGenerator("jobId", new RandomLongPropertyGenerator(50, 60));
            this.tasks = taskFaker.fakeList(300);
        } catch (NoSuchPropertyException | ConflictTypeGeneratorException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        
        
    }
    
    
    public List<Task> getTasks(String tag, long from, long to, boolean pending, boolean running, boolean finished){
        List<Task> tasks = new ArrayList<>();
        for(Task current: this.tasks){
            if(tag != null && !current.getTag().equals(tag)){
                continue;
            }
            
            if(from > 0 && current.getStartTime() < from){
                continue;
            }
            
            if(to > 0 && current.getStartTime() > to){
                continue;
            }
            
            State state = getTaskState(current.getStatus());
            if(!pending && state == State.PENDING){
                continue;
            }
            if(!running && state == State.RUNNING){
                continue;
            }
            if(!finished && state == State.FINISHED){
                continue;
            }
            
            tasks.add(current);
        }
        
        return tasks;
        
    }
    
    
    
    public String getTasksAsJson(String tag, long from, long to, boolean pending, boolean running, boolean finished, int offset, int limit){
        List<Task> tasks = getTasks(tag, from, to, pending, running, finished);
        int size = tasks.size();
        JsonObject result = new JsonObject();
        result.addProperty("size", size);
        JsonArray arrayTasks = new JsonArray();
        
        if(size > 0){
            if(limit > size - 1){
                limit = size - 1;
            }
            
            for(Task t: tasks.subList(offset, limit)){
                JsonElement elt = taskToJson(t);
                arrayTasks.add(elt);
            }
        }
        
        result.add("tasks", arrayTasks);
        
        return result.toString();
    }
    
    
    public static JsonElement taskToJson(Task t){
        JsonObject taskInfo = new JsonObject();
        taskInfo.addProperty("executionHostName", t.getHostName());
        
        JsonObject taskId = new JsonObject();
        taskId.addProperty("id", t.getId());
        
        taskInfo.add("taskId", taskId);
        taskInfo.addProperty("taskStatus", jsonValue(t.getStatus()));
        taskInfo.addProperty("startTime", t.getStartTime());
        taskInfo.addProperty("finishedTime", t.getFinishTime());
        taskInfo.addProperty("executionDuration", t.getExecutionTime());
        taskInfo.addProperty("jobId", t.getJobId());
        taskInfo.addProperty("jobName", t.getJobName());
        taskInfo.addProperty("numberOfExecutionLeft", t.getNumberOfExecLeft());
        taskInfo.addProperty("numberOfExecutionOnFailureLeft", t.getNumberOfExecOnFailureLeft());
        
        JsonObject parallel = new JsonObject();
        parallel.addProperty("nodesNumber", t.getNodeCount());
        
        JsonObject jsonTask = new JsonObject();
        jsonTask.add("taskInfo", taskInfo);
        
        jsonTask.addProperty("name", t.getName());
        jsonTask.addProperty("description", t.getDescription());
        jsonTask.addProperty("tag", t.getTag());
        jsonTask.addProperty("maxNumberOfExecution", t.getMaxNumberOfExec());
        jsonTask.addProperty("maxNumberOfExecutionOnFailure", t.getMaxNumberOfExecOnFailure());
        jsonTask.add("parallelEnvironment", parallel);
        
        return jsonTask;
    }
    
    
    
    public static String jsonValue(TaskStatus status){
        switch(status){
        case ABORTED:
            return "ABORTED";
        case FAILED:
            return "FAILED";
        case FAULTY:
            return "FAULTY";
        case FINISHED:
            return "FINISHED";
        case NOT_RESTARTED:
            return "NOT_RESTARTED";
        case NOT_STARTED:
            return "NOT_RESTARTED";
        case PAUSED:
            return "PAUSED";
        case PENDING:
            return "PENDING";
        case RUNNING:
            return "RUNNING";
        case SKIPPED:
            return "SKIPPED";
        case SUBMITTED:
            return "SUBMITTED";
        case WAITING_ON_ERROR:
            return "WAITING_ON_ERROR";
        case WAITING_ON_FAILURE:
            return "WAITING_ON_FAILURE";
        default:
            return null;
        }
    }
    
    
    protected State getTaskState(TaskStatus status){
        switch(status){
        case ABORTED : case FAILED: case FAULTY: case FINISHED: case SKIPPED: case NOT_STARTED:
            return State.FINISHED;
        case PAUSED: case RUNNING: case WAITING_ON_ERROR: case WAITING_ON_FAILURE: case NOT_RESTARTED:
            return State.RUNNING;
        case PENDING: case SUBMITTED: 
            return State.PENDING;
        default:
            return null;
        }
    }
}
