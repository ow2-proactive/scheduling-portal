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

import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.TasksNavigationModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.TasksNavigationView;

import com.smartgwt.client.widgets.layout.Layout;


/**
 * Controller for the task navigation logic.
 * @author The activeeon team
 *
 */
public class TasksNavigationController {

    /**
     * The main controller of the project.
     */
    protected TasksController parentController;

    /**
     * The controller for the tasks pagination logic.
     */
    protected TasksPaginationController paginationController;

    /**
     * The controller for the tag suggestions logic.
     */
    protected PrefixWordSuggestOracle tagSuggestionOracle;

    /**
     * The model for the tasks navigation.
     */
    protected TasksNavigationModel model;

    protected TasksNavigationView view;

    /**
     * Builds a controller for the tasks navigation logic.
     * @param parentController the main controller.
     */
    public TasksNavigationController(TasksController parentController) {
        this.parentController = parentController;
        this.model = parentController.getModel().getTasksNavigationModel();
        this.paginationController = new TasksPaginationController(parentController);
        this.tagSuggestionOracle = new PrefixWordSuggestOracle(model);
    }

    public Layout buildView() {
        this.view = new TasksNavigationView(this);
        return this.view.build();
    }

    /**
     * Gets the controller for the tasks pagination logic.
     * @return the controller for the tasks pagination logic.
     */
    public TasksPaginationController getPaginationController() {
        return paginationController;
    }

    /**
     * Gets the controller for the tag suggestions logic.
     * @return the controller for the tag suggestions logic.
     */
    public PrefixWordSuggestOracle getTagSuggestionOracle() {
        return tagSuggestionOracle;
    }

    /**
     * Sets auto-refresh tasks list option to the new value. 
     * @param value the new value.
     */
    public void setTaskAutoRefreshOption(boolean value) {
        this.model.setTaskAutoRefreshOption(value);
    }

    /**
     * Applies a tag filter to the tasks list.
     * @param tag the tag filter to be applied.
     */
    public void setTaskTagFilter(String tag) {
        boolean changed = this.model.setCurrentTagFilter(tag);
        if (changed) {
            this.parentController.resetPendingTasksRequests();
            paginationController.firstPage();
        }
    }

    /**
     * Sets the controller for the tasks pagination logic.
     * @param taskPaginationController the controller for the tasks pagination logic.
     */
    public void setTaskPaginationController(TasksPaginationController taskPaginationController) {
        this.paginationController = taskPaginationController;
    }

    /**
     * Reset the item navigation.
     */
    public void resetNavigation() {
        this.model.resetTagFilter();
        this.model.clearTagSuggestions();
        this.paginationController.firstPage();
    }

    public void stopNavigation() {
        this.tagSuggestionOracle.resetTagSuggestions();
        this.paginationController.resetPagination();
    }

    /**
     * Gets the item navigation model. 
     * @return the item navigation model.
     */
    public TasksNavigationModel getModel() {
        return model;
    }

    /**
     * Refresh the paginated tasks list.
     */
    public void refresh() {
        if (this.model.getTaskAutoRefreshOption()) {
            this.paginationController.refresh();
        }
    }

    /**
     * Set new status filters and triggers paginationController
     * to updated task list
     * @param allFilters new status filters, e.g. "Pending;Error", etc.
     */
    public void fitlerByStatuses(String allFilters) {
        model.setStatusFilter(allFilters);
        parentController.resetPendingTasksRequests();
        paginationController.firstPage();
    }
}
