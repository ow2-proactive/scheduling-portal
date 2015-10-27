package org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid;

public enum JobsColumns implements GridColumns{
    ID_ATTR("id", "id", 80, true),
    STATE_ATTR("state", "State", 100, true),
    USER_ATTR("user","User", 140, true),
    PROGRESS_ATTR("progress", "Progress", 100, true),
    PRIORITY_ATTR("priority", "Priority",150, true),
    DURATION_ATTR("duration", "Duration", 100, true),
    NAME_ATTR("name", "Name", -1, true);
    
    private final String name;
    private final String title;
    private final int width;
    private final boolean hasData;
    
    private JobsColumns(String name, String title, int width, boolean hasData) {
        this.name = name;
        this.title = title;
        this.width = width;
        this.hasData = hasData;
    }

    @Override public String getName() {return name;}
    @Override public String getTitle() {return title;}
    @Override public int getWidth() {return width;}
    @Override public boolean hasData() {return hasData;}
}