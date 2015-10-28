/*
 * ################################################################
 *
 * ProActive Parallel Suite(TM): The Java(TM) library for
 *    Parallel, Distributed, Multi-Core Computing for
 *    Enterprise Grids & Clouds
 *
 * Copyright (C) 1997-2011 INRIA/University of
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
 * ################################################################
 * $$PROACTIVE_INITIAL_DEV$$
 */
package org.ow2.proactive_grid_cloud_portal.scheduler.client.view;

import java.util.List;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.TasksUpdatedListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Task;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.TasksController;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.ExpandableTasksListGrid;

import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;


/**
 * Contains the ListGrid that displays tasks
 *
 * @author mschnoor
 */
public class TasksView extends AbstractGridItemsView implements TasksUpdatedListener {
   

    /**
     * the Grid widget displayed in the view
     */
    private ExpandableTasksListGrid tasksGrid = null;
    /**
     * shown when loading
     */
    private Label loadingLabel = null;
    /**
     * shown upon error
     */
    private Label errorLabel = null;
    
   
    protected TasksController controller;


    public TasksView(TasksController controller) {
        this.controller = controller;
        this.controller.getModel().addTasksUpdatedListener(this);
    }


    public void tasksUpdating(boolean jobChanged) {
        if (jobChanged) {
            this.errorLabel.hide();
            this.tasksGrid.hide();
            this.loadingLabel.show();
        }
    }

    public void tasksUpdatedFailure(String message) {
        this.errorLabel.setContents(message);
        this.tasksGrid.hide();
        this.loadingLabel.hide();
        this.errorLabel.show();
    }

    public void tasksUpdated(List<Task> tasks, long totalTasks) {
        this.errorLabel.hide();
        this.loadingLabel.hide();
        this.tasksGrid.show();
    }

    

    
    public Layout build() {
        this.tasksGrid = new ExpandableTasksListGrid(this.controller);
        this.tasksGrid.build();
 
        this.loadingLabel = new Label("fetching tasks...");
        this.loadingLabel.setIcon("loading.gif");
        this.loadingLabel.setWidth100();
        this.loadingLabel.setHeight100();
        this.loadingLabel.setAlign(Alignment.CENTER);
        this.loadingLabel.hide();

        this.errorLabel = new Label("");
        this.errorLabel.setWidth100();
        this.errorLabel.setAlign(Alignment.CENTER);
        this.errorLabel.hide();
        
        Layout navTools = this.controller.getTaskNavigationController().buildView();
        Layout paginationBar = this.controller.getTaskNavigationController().getPaginationController().buildView();

        VLayout tasksViewLayout = new VLayout();
        tasksViewLayout.addMember(navTools);
        tasksViewLayout.addMember(this.tasksGrid);
        tasksViewLayout.addMember(this.loadingLabel);
        tasksViewLayout.addMember(this.errorLabel);
        tasksViewLayout.addMember(paginationBar);

        return tasksViewLayout;
    }
}
