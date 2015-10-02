package org.ow2.proactive_grid_cloud_portal.scheduler.client.controller;

import org.ow2.proactive_grid_cloud_portal.common.client.Settings;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.PaginatedModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.PaginationModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.shared.SchedulerConfig;

public abstract class PaginationController {

	protected PaginationModel model;
	
	
	protected PaginatedController paginatedController;
	
	protected String pageSizePropertyName;
	
	
	public PaginationController(PaginatedController paginatedController, String pageSizePropertyName){
		this.paginatedController = paginatedController;
		this.pageSizePropertyName = pageSizePropertyName;
	}
	
	
	/**
     * Back to page 0
     * invalidate the current page, set the jobs views in indeterminate mode
     */
    public void resetPage() {
        model.setPage(0);
        this.paginatedController.fetch();
    }

    /**
     * Fetch the next job list page
     * invalidate the current page, set the jobs views in indeterminate mode
     */
    public void nextPage() {
        model.setPage(model.getPage() + 1);
        this.paginatedController.fetch();
    }
    
    
    /**
     * Fetch the previous job list page
     * invalidate the current page, set the jobs views in indeterminate mode
     */
    public void previousPage() {
        int curPage = model.getPage();
        if (curPage == 0)
            return;
        model.setPage(curPage - 1);
        this.paginatedController.fetch();
    }
    
    
    public abstract void fetch();
   
    
    
    public void setUserSettings(String pageSize, boolean forceRefresh) {
        boolean pageChanged = !pageSize.equals("" + SchedulerConfig.get().getPageSize(this.model.getItemType()));
        SchedulerConfig.get().set(this.pageSizePropertyName, pageSize);
        Settings.get().setSetting(this.pageSizePropertyName, pageSize);

        if (pageChanged || forceRefresh) {
            resetPage();
        }        
    }
    
    
    public int getOffset(){
    	return this.model.getPage() * this.model.getPageSize();
    }
    
    
    public int getRange(){
    	return this.model.getPageSize();
    }
}
