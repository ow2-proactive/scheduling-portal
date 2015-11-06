package org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.tasks;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.Task;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.GridColumns;

import com.smartgwt.client.data.Record;

/**
 * The factory to get the columns specifications, and build record acoording to this specification, for the expand section of a expandable task grid.
 * @author the activeeon team.
 *
 */
public class ExpandTasksColumnsFactory extends TasksColumnsFactory{

    @Override
    public GridColumns[] getColumns() {
        return new GridColumns[]{HOST_ATTR, START_TIME_ATTR, FINISHED_TIME_ATTR,DESCRIPTION_ATTR};
    }
    
    @Override
    public void buildRecord(Task item, Record record) {
        //DetailViewerRecord record = new DetailViewerRecord();
        this.buildDetailsColumns(record, item);
    }
}
