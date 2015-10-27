package org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid;

public enum TasksColumns implements GridColumns{

    ID_ATTR("id", "Id", 60, false, true),
    STATUS_ATTR("status", "Status", 80, false, true),
    NAME_ATTR("name", "Name", 100, false, true),
    TAG_ATTR("tag", "Tag", 100, false, true),
    EXEC_DURATION_ATTR("execDuration", "Duration", 80, false, true),
    NODE_COUNT_ATTR("nodeCount", "Nodes", 40, false, true),
    EXECUTIONS_ATTR("executions", "Executions", 60, false, true),
    NODE_FAILURE_ATTR("nodeFailure", "Failures", 60, false, true),
    HOST_ATTR("host", "Host", 100, true, true),
    START_TIME_ATTR("startTime", "Started at", 100, true, true),
    FINISHED_TIME_ATTR("finishedTime", "Finished at", 100, true, true),
    DESCRIPTION_ATTR("description", "Description", 100, true, true),
    VISU_ATTR("visu", " ", 20, false, false);
    
    private final String name;
    private final String title;
    private final int width;
    private final boolean detail;
    private final boolean hasData;
    
    private TasksColumns(String name, String title, int width, boolean detail, boolean hasData) {
        this.name = name;
        this.title = title;
        this.width = width;
        this.detail = detail;
        this.hasData = hasData;
    }

    @Override public String getName() {return name;}
    @Override public String getTitle() {return title;}
    @Override public int getWidth() {return width;}
    @Override public boolean hasData() {return hasData;}
    public boolean getDetail() {return detail;}
    
    
    public static int getNumDetails(){
        int result = 0;
        for(TasksColumns col: TasksColumns.values()){
            if(col.getDetail()) result++;
        }
        return result;
    }
}
