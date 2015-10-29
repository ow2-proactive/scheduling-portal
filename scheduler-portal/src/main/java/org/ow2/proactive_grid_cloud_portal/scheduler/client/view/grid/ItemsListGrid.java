package org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid;

import java.util.Collection;
import java.util.EnumMap;
import java.util.Map;

import org.ow2.proactive_grid_cloud_portal.common.client.model.LoginModel;

import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.data.DSCallback;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.DataSourceField;
import com.smartgwt.client.data.RecordList;
import com.smartgwt.client.data.fields.DataSourceIntegerField;
import com.smartgwt.client.data.fields.DataSourceTextField;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
import com.smartgwt.client.widgets.grid.events.CellOutEvent;
import com.smartgwt.client.widgets.grid.events.CellOutHandler;
import com.smartgwt.client.widgets.grid.events.SelectionChangedHandler;
import com.smartgwt.client.widgets.grid.events.SelectionEvent;
import com.smartgwt.client.widgets.menu.Menu;

public abstract class ItemsListGrid extends ListGrid{

    protected String datasourceNamePrefix;
    
    protected String emptyMessage;
    
    /**
     * current job filtering criteria, or null
     */
    protected AdvancedCriteria filter = null;
    
    /**
     * data-source: contains the actual data
     */
    protected ItemDS ds = null;
    
    /** To disable selection listener while fetching data */
    protected boolean fetchingData;
    
    protected class ItemDS extends DataSource {

        public ItemDS(String id) {
            setID(id);
            DataSourceField [] fields = buildDatasourceFields();
            setFields(fields);
            setClientOnly(true);
        }
    }
    
    
    public void build(){
        this.ds = new ItemDS(this.datasourceNamePrefix + LoginModel.getInstance().getSessionId());
        
        this.setDataSource(this.ds);
        
        this.setCanGroupBy(false);
        this.setCanReorderFields(true);
        this.setCanPickFields(true);
        this.setCanFreezeFields(false);
        this.setEmptyMessage(this.emptyMessage);
        this.setAutoFetchData(true);
        this.setShowSortNumerals(false);
        
        Collection<ListGridField> fieldsCollection = this.buildListGridField().values();
        ListGridField [] fields = new ListGridField[fieldsCollection.size()];
        this.setFields(fieldsCollection.toArray(fields));

        this.setWidth100();
        this.setHeight100();
        
        
     // right click on an entry : popup a menu for job-contextual operations
        this.addCellContextClickHandler(new CellContextClickHandler() {
            public void onCellContextClick(CellContextClickEvent event) {
                Menu menu = new Menu();
                menu.setShowShadow(true);
                menu.setShadowDepth(10);
                buildCellContextualMenu(menu);
                setContextMenu(menu);
            }
        });

        this.addCellOutHandler(new CellOutHandler() {
            public void onCellOut(CellOutEvent event) {
                // cancel contextual menu when moving the mouse away to
                // avoid keeping a menu on items we lost track of.
                // Not perfect but works in most cases
                setContextMenu(null);
            }
        });

        this.addSelectionChangedHandler(new SelectionChangedHandler() {
            public void onSelectionChanged(SelectionEvent event) {
                selectionChangedHandler(event);
            }
        });
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
    
    
    protected abstract void buildCellContextualMenu(Menu menu);
    
    protected abstract void selectionChangedHandler(SelectionEvent event);
    
    public void applyFilter(AdvancedCriteria filter) {
        this.filter = filter;
        transparentUpdate(ds.getTestData().length);
    }
    
 // as found in https://isomorphic.atlassian.net/wiki/display/Main/Refresh+ListGrid+Periodically+(Smart+GWT)#RefreshListGridPeriodically(SmartGWT)-Transparentupdate
    protected void transparentUpdate(int nbOfJobs) {
        DataSource dataSource = this.getDataSource();
        Integer[] visibleRows = this.getVisibleRows();

        DSRequest request = new DSRequest();
        request.setStartRow(0);
        request.setEndRow(nbOfJobs + visibleRows[1]);
        request.setSortBy(this.getSort());

        dataSource.fetchData(this.filter, new DSCallback() {
            @Override
            public void execute(DSResponse response, Object rawData, DSRequest request) {
                ListGridRecord[] keepSelectedRecords = getSelectedRecords();
                setData(new RecordList(response.getData()));
                // manual reset of selection (otherwise it is lost)
                if (keepSelectedRecords != null) {
                    fetchingData = true;
                    selectRecords(keepSelectedRecords);
                    fetchingData = false;
                }
            }

        }, request);
    }
}
