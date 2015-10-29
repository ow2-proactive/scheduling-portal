package org.ow2.proactive_grid_cloud_portal.scheduler.client.view;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.ItemsListGrid;

import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;


public abstract class AbstractGridItemsView {

    /**
     * the Grid widget displayed in the view
     */
    protected ItemsListGrid itemsGrid = null;
    /**
     * shown when loading
     */
    protected Label loadingLabel = null;
    /**
     * shown upon error
     */
    protected Label errorLabel = null;
    
    protected String itemName;
    
    
    protected void itemUpdating(){
        this.errorLabel.hide();
        this.itemsGrid.hide();
        this.loadingLabel.show();
    }
    
    
    protected void itemUpdatedFailure(String message){
        this.errorLabel.setContents(message);
        this.itemsGrid.hide();
        this.loadingLabel.hide();
        this.errorLabel.show();
    }
    
    
    protected void itemUpdated(){
        this.errorLabel.hide();
        this.loadingLabel.hide();
        this.itemsGrid.show();
    }
    
    protected abstract Layout buildToolbar();
    
    protected abstract Layout buildPagination();
    
    protected abstract void buildGrid();
    
    
    protected Layout buildContent(){
        VLayout layout = new VLayout();
        this.buildGrid();
        
        this.loadingLabel = new Label("fetching " + this.itemName + "...");
        this.loadingLabel.setIcon("loading.gif");
        this.loadingLabel.setWidth100();
        this.loadingLabel.setHeight100();
        this.loadingLabel.setAlign(Alignment.CENTER);
        this.loadingLabel.hide();

        this.errorLabel = new Label("");
        this.errorLabel.setWidth100();
        this.errorLabel.setAlign(Alignment.CENTER);
        this.errorLabel.hide();
        
        layout.addMember(this.itemsGrid);
        layout.addMember(this.loadingLabel);
        layout.addMember(this.errorLabel);
        
        return layout;
    }
    
    
    public Layout build() {
        Layout content = this.buildContent();
        Layout navTools = this.buildToolbar();
        Layout paginationBar = this.buildPagination();

        VLayout itemsViewLayout = new VLayout();
        itemsViewLayout.addMember(navTools);
        itemsViewLayout.addMember(content);
        itemsViewLayout.addMember(paginationBar);

        return itemsViewLayout;
    }
}
