/*
 * ProActive Parallel Suite(TM):
 * The Open Source library for parallel and distributed
 * Workflows & Scheduling, Orchestration, Cloud Automation
 * and Big Data Analysis on Enterprise Grids & Clouds.
 *
 * Copyright (c) 2007 - 2017 ActiveEon
 * Contact: contact@activeeon.com
 *
 * This library is free software: you can redistribute it and/or
 * modify it under the terms of the GNU Affero General Public License
 * as published by the Free Software Foundation: version 3 of
 * the License.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 *
 * If needed, contact us to obtain a release under GPL Version 2 or 3
 * or a different license than the AGPL.
 */
package org.ow2.proactive_grid_cloud_portal.scheduler.client.controller;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.TasksPaginationModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.TasksPaginationView;

import com.smartgwt.client.widgets.layout.Layout;


/**
 * Controller for the tasks pagination logic.
 * @author activeeon team.
 *
 */
public class TasksPaginationController extends PaginationController<TasksPaginationModel> {

    protected TasksController itemsController;

    protected TasksPaginationView view;

    public TasksPaginationController(TasksController itemsController) {
        super(itemsController.getModel().getTasksNavigationModel().getPaginationModel());
        this.itemsController = itemsController;
    }

    @Override
    public void fetch(boolean silentUpdate) {
        this.itemsController.updateTasks(!silentUpdate);
    }

    @Override
    public Layout buildView() {
        this.view = new TasksPaginationView(itemsController);
        return this.view.build();
    }

    /**
     * Fetch the next item list page
     */
    public void nextPage() {
        model.setPage(model.getPage() + 1);
        this.fetch(false);
    }

    @Override
    public void previousPage() {
        int curPage = model.getPage();
        if (curPage == 0)
            return;
        model.setPage(curPage - 1);
        this.fetch(false);
    }

    @Override
    public void firstPage() {
        model.setPage(0);
        this.fetch(false);
    }

    public void lastPage() {
        this.model.setPage(this.model.getMaxPage());
        this.fetch(false);
    }

    public void goToPage(int pageNumber) {
        if (pageNumber < 0) {
            pageNumber = 0;
        }

        int maxPage = this.model.getMaxPage();
        if (pageNumber > maxPage) {
            pageNumber = maxPage;
        }

        this.model.setPage(pageNumber);
        this.fetch(false);
    }

    public void computeMaxPage(long nbItem) {
        this.model.setTotalItems(nbItem);
    }

    /**
     * Gets the text that displays the pagination status.
     * @return the text that displays the pagination status.
     */
    public String getPaginationRangeLabel() {
        //The index of the current displayed page
        int page = this.model.getPage();
        //The size of a page
        int size = this.model.getPageSize();
        //The total number of jobs
        long total = this.model.getTotalItems();

        //The index of the first job of the current page; cannot be below 0 or over the max number of jobs
        long firstJobIndex = Math.min(Math.max(page * size + 1, 0), total);
        //The index of the last job of the current page; cannot be more than the max number of jobs
        long lastJobIndex = Math.min((page + 1) * size, total);

        return firstJobIndex + " - " + lastJobIndex;
    }

    public String getNumberPageText() {
        if (this.model.getTotalItems() > 0) {
            return "" + (this.model.getPage() + 1);
        } else {
            return "0";
        }
    }

    @Override
    public boolean hasPrevious() {
        return (this.model.getPage() > 0);
    }

    @Override
    public boolean hasNext() {
        return this.model.getTotalItems() > (this.model.getOffset() + this.model.getPageSize());
    }

    public String getMaxPageNumberLabel() {
        return "" + (this.model.getMaxPage() + 1);
    }

    public void resetPagination() {
        this.model.setPage(-1);
        this.model.setTotalItems(0);
    }

}
