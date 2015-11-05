package org.ow2.proactive_grid_cloud_portal.scheduler.client.view;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.ColumnsFactory;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.GridColumns;

import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.viewer.DetailViewer;
import com.smartgwt.client.widgets.viewer.DetailViewerField;
import com.smartgwt.client.widgets.viewer.DetailViewerRecord;

public class InfoView<T> {

	/** label when no job is selected */
    protected Label label = null;
    /** widget shown when a job is selected */
    protected DetailViewer details = null;
    
    /** currently displayed job */
    protected T displayedItem = null;
    
    protected ColumnsFactory<T> factory;
    
    public InfoView(ColumnsFactory<T> factory) {
		this.factory = factory;
	}
    
    
    /**
     * @return the Widget to display, ready to be added in a container
     */
    public Layout build() {
        /* widget that has been returned as the root layout */
        Layout root = new Layout();
        root.setWidth100();
        root.setHeight100();

        this.label = new Label("No job selected.");
        this.label.setWidth100();
        this.label.setAlign(Alignment.CENTER);

        this.details = new DetailViewer();
        this.details.setWidth100();
        this.details.setHeight100();
        this.details.setCanSelectText(true);
        this.details.hide();

        GridColumns[] lines = this.factory.getColumns();
        DetailViewerField [] fields = new DetailViewerField[lines.length];
        for(int i = 0; i < lines.length; i++){
        	fields[i] = new DetailViewerField(lines[i].getName(), lines[i].getTitle());
        }

        this.details.setFields(fields);

        root.addMember(label);
        root.addMember(details);

        return root;
    }
    
    public void hideDetails(){
    	this.details.hide();
        this.label.show();
        this.displayedItem = null;
    }
    
    
    public void displayItem(){
    	DetailViewerRecord [] records = new DetailViewerRecord[1];
        records[0] = (DetailViewerRecord) this.factory.buildRecord(this.displayedItem);

        this.details.setData(records);

        this.label.hide();
        this.details.show();
    }
}
