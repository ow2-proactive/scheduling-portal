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

import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerController;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerModelImpl;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerServiceAsync;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.TasksNavigationModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.suggestions.PrefixWordSuggestOracle;

/**
 * Controller for the task navigation logic.
 * @author The activeeon team
 *
 */
public class TasksNavigationController {

    /**
     * The main controller of the project.
     */
    private SchedulerController schedulerController;

    /**
     * The controller for the tasks pagination logic.
     */
    private TasksPaginationController paginationController;

    /**
     * The controller for the tag suggestions logic.
     */
    private PrefixWordSuggestOracle tagSuggestionOracle;

    /**
     * The model for the tasks navigation.
     */
    private TasksNavigationModel model;

    /**
     * Builds a controller for the tasks navigation logic.
     * @param controller the main controller.
     */
    public TasksNavigationController(SchedulerController controller){
        this.schedulerController = controller;
        this.model = new TasksNavigationModel();
        ((SchedulerModelImpl) controller.getModel()).setTasksNavigationModel(this.model);
        this.paginationController = new TasksPaginationController(controller);
        SchedulerServiceAsync scheduler = controller.getScheduler();
        this.tagSuggestionOracle = new PrefixWordSuggestOracle((SchedulerModelImpl) controller.getModel(), scheduler);

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
    public void setTaskAutoRefreshOption(boolean value){
        this.model.setTaskAutoRefreshOption(value);
    }


    /**
     * Applies a tag filter to the tasks list.
     * @param tag the tag filter to be applied.
     */
    public void setTaskTagFilter(String tag){
        boolean changed = this.model.setCurrentTagFilter(tag);
        if(changed){
            this.schedulerController.resetPendingTasksRequests();
            paginationController.firstPage();
        }
    }

    /**
     * Sets the controller for the tasks pagination logic.
     * @param taskPaginationController the controller for the tasks pagination logic.
     */
    public void setTaskPaginationController(
            TasksPaginationController taskPaginationController) {
        this.paginationController = taskPaginationController;
    }


    /**
     * Reset the item navigation.
     */
    public void resetNavigation(){
        this.model.clearTagSuggestions();
        paginationController.firstPage();
    }
    
    
    public void stopNavigation(){
        this.model.clearTagSuggestions();
        this.paginationController.resetPagination();
    }


    /**
     * Gets the item navigation model. 
     * @return the item navigation model.
     */
    public TasksNavigationModel getModel() {
        return model;
    }
    
    
    public void refresh(){
        if(this.model.getTaskAutoRefreshOption()){
            this.paginationController.fetch();
        }
    }
}
