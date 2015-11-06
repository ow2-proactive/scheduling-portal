package org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid;

import com.smartgwt.client.data.Record;

/**
 * A factory that allows to get the list of columns, and build record according to these columns.
 * @author The activeeon team.
 *
 * @param <I> the type of the item used to build records for the columns.
 */
public interface ColumnsFactory<I> {

    /**
     * Gets the list of columns.
     * @return the list of columns.
     */
    public GridColumns [] getColumns();
    
    
    /**
     * Builds a record from a given item according to the columns provided by this factory.
     * @param item the item used to build a new record.
     * @param record the new record to be built with new attributes.
     */
    public void buildRecord(I item, Record record);
    
}
