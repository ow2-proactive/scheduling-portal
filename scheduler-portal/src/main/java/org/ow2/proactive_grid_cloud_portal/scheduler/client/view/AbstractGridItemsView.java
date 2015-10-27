package org.ow2.proactive_grid_cloud_portal.scheduler.client.view;

import java.util.EnumMap;
import java.util.Map;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.GridColumns;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.JobsColumns;

import com.smartgwt.client.data.DataSourceField;
import com.smartgwt.client.data.fields.DataSourceIntegerField;
import com.smartgwt.client.data.fields.DataSourceTextField;
import com.smartgwt.client.widgets.grid.ListGridField;

public abstract class AbstractGridItemsView {

    protected DataSourceField [] buildDatasourceFields(){
        JobsColumns [] columns = JobsColumns.values();
        DataSourceField [] result = new DataSourceField[columns.length];
        for(int i = 0; i < columns.length; i++){
            switch(columns[i]){
            case ID_ATTR:
                result[i] = new DataSourceIntegerField(columns[i].getName());
                result[i].setPrimaryKey(true);
            default:
                result[i] = new DataSourceTextField(columns[i].getName(), columns[i].getTitle());
                result[i].setRequired(true);
            }   
        }
        return result;
    }
    
    protected abstract <K extends Enum<K>> EnumMap<K, ListGridField> getColumnsForListGridField();
    
    protected <K extends Enum<K>> EnumMap<K, ListGridField> buildListGridField(){
        EnumMap<K, ListGridField> fields = getColumnsForListGridField();
        for(Map.Entry<K, ListGridField> current: fields.entrySet()){
            GridColumns key = (GridColumns) current.getKey();
            ListGridField field = new ListGridField(key.getName(), key.getTitle());
            if(key.getWidth() > 0){
                field.setWidth(key.getWidth());
            }
            current.setValue(field);
        }
        return fields;
    }
}
