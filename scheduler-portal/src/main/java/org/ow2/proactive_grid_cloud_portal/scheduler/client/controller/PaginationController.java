/*
 *  *
 * ProActive Parallel Suite(TM): The Java(TM) library for
 *    Parallel, Distributed, Multi-Core Computing for
 *    Enterprise Grids & Clouds
 *
 * Copyright (C) 1997-2014 INRIA/University of
 *                 Nice-Sophia Antipolis/ActiveEon
 * Contact: proactive@ow2.org or contact@activeeon.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Affero General Public License
 * as published by the Free Software Foundation; version 3 of
 * the License.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
 * USA
 *
 * If needed, contact us to obtain a release under GPL Version 2 or 3
 * or a different license than the AGPL.
 *
 *  Initial developer(s):               The ProActive Team
 *                        http://proactive.inria.fr/team_members.htm
 *  Contributor(s):
 *
 *  * $$PROACTIVE_INITIAL_DEV$$
 */

package org.ow2.proactive_grid_cloud_portal.scheduler.client.controller;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.PaginationModel;

import com.smartgwt.client.widgets.layout.Layout;

/**
 * The controller for the item pagination logic.
 * @author the activeeon team
 *
 */
public abstract class PaginationController {

    /**
     * The model for the pagination.
     */
    protected PaginationModel model;

   

    /**
     * Builds a controller for the pagination logic.
     * @param schedulerController the main controller.
     */
    public PaginationController(){
    }


    /**
     * Fetch the next item list page
     */
    public void nextPage() {
        model.setPage(model.getPage() + 1);
        this.fetch(false);
    }


    /**
     * Fetch the previous item list page
     */
    public void previousPage() {
        int curPage = model.getPage();
        if (curPage == 0)
            return;
        model.setPage(curPage - 1);
        this.fetch(false);
    }
    
    
    /**
     * Fetch the first item list page.
     */
    public void firstPage(){
        model.setPage(0);
        this.fetch(false);
    }
    
    /**
     * Fetch the last item list page.
     */
    public void lastPage(){
        this.model.setPage(this.model.getMaxPage());
        this.fetch(false);
    }
    
    
    /**
     * Fetch the page with the given number.
     * @param pageNumber the number of the page to be displayed.
     */
    public void goToPage(int pageNumber){
        if(pageNumber < 0){
            pageNumber = 0;
        }
        
        int maxPage = this.model.getMaxPage();
        if(pageNumber > maxPage){
            pageNumber = maxPage;
        }
        
        this.model.setPage(pageNumber);
        this.fetch(false);
    }

    
    /**
     * Computes the number of the last page of items.
     * @param nbItem total number of items without pagination.
     */
    public void computeMaxPage(long nbItem){
        this.model.setTotalItems(nbItem);
    }
    

    /**
     * Fetch the items for the current page.
     */
    public abstract void fetch(boolean silentFetch);
    
    
    public abstract Layout buildView();



    /**
     * Gets the text that displays the pagination status.
     * @return the text that displays the pagination status.
     */
    public String getPaginationRangeLabel(){
        int page = this.model.getPage();
        int size = this.model.getPageSize();
        
        long index = page * size;
        long total = this.model.getTotalItems();
        long range = index + size;
        
        index++;
        if(index < 0){
            index = 0;
        }
        
        if(range < total){
            return index + " - " + range;
        }
        else{
            return index + " - " + total;
        }
    }
    
    
    public String getNumberPageText(){
        return "" + (this.model.getPage() + 1);
    }
    

    /**
     * Returns true if there is item before the current list of items, false otherwise.
     * @return true if there is item before the current list of items, false otherwise.
     */
    public boolean hasPrevious(){
        return (this.model.getPage() > 0);
    }

    /**
     * Returns true if there is item after the current list of items, false otherwise.
     * @return true if there is item after the current list of items, false otherwise.
     */
    public boolean hasNext(){
        return (this.model.getRange() < this.model.getTotalItems());
    }


    /**
     * Gets the pagination model.
     * @return the pagination model.
     */
    public PaginationModel getModel() {
        return model;
    }
    
    /**
     * Get the numero of the last page. 
     * @return the numero of the last page.
     */
    public String getMaxPageNumberLabel(){
        return "" + (this.model.getMaxPage() + 1);
    }

    /**
     * Reset the pagination, no items are displayed in the paginated list.
     */
    public void resetPagination(){
        this.model.setPage(-1);
        this.model.setTotalItems(0);
    }
    
    
    public void refresh(){
        this.fetch(true);
    }
}
