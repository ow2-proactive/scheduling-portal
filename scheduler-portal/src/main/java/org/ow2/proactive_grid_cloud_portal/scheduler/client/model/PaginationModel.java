package org.ow2.proactive_grid_cloud_portal.scheduler.client.model;

import org.ow2.proactive_grid_cloud_portal.common.client.Settings;
import org.ow2.proactive_grid_cloud_portal.scheduler.shared.PaginatedItemType;
import org.ow2.proactive_grid_cloud_portal.scheduler.shared.SchedulerConfig;

public class PaginationModel {

	private int currentPage = 0;
	
	private PaginatedItemType itemType;
	
	
	public PaginationModel(PaginatedItemType itemType, PaginatedModel paginatedModel){
		this.itemType = itemType;
	}
	
	
	
    public int getPage() {
        return this.currentPage;
    }
	
	
	/**
     * change current page
     * 
     * @param page new page number
     */
    public void setPage(int page) {
        this.currentPage = page;
    }
    
    
    
    public int getPageSize() {
        return SchedulerConfig.get().getPageSize(this.itemType);
    }
    
    
    public void setUserSettings(String pageSize) {
        SchedulerConfig.get().set(SchedulerConfig.JOBS_PAGE_SIZE, pageSize);
        Settings.get().setSetting(SchedulerConfig.JOBS_PAGE_SIZE, pageSize);
    }



	public PaginatedItemType getItemType() {
		return itemType;
	}
    
    
    
}
