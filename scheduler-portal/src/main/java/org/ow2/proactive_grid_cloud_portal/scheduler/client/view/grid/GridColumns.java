package org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid;

/**
 * A class that groups columns and datasource settings for all columns of a grid.
 * @author the activeeon team.
 *
 */
public class GridColumns{
    
    /**
     * The name of the column
     */
    protected final String name;
    
    /**
     * The title of the column, as shown in the grid header.
     */
    protected final String title;
    
    /**
     * The width of the column in the grid.
     */
    protected final int width;
    
    /**
     * True if this column is bound to a data in the datasource, false otherwise.
     */
    protected final boolean hasData;
    
    /**
     * True if this column is part of the primary key.
     */
    protected final boolean isPrimaryKey;
    
    
    public GridColumns(String name, String title, int width, boolean hasData, boolean isPrimaryKey) {
        this.name = name;
        this.title = title;
        this.width = width;
        this.hasData = hasData;
        this.isPrimaryKey = isPrimaryKey;
    }

    public String getName() {return name;}
    public String getTitle() {return title;}
    public int getWidth() {return width;}
    public boolean hasData() {return hasData;}
    public boolean isPrimaryKey() {return isPrimaryKey;}
 
}