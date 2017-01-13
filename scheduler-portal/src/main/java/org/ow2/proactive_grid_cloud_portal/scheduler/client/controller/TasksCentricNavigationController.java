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

import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.TasksCentricNavigationModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.TasksCentricNavigationView;

import com.smartgwt.client.widgets.layout.Layout;


public class TasksCentricNavigationController extends TasksNavigationController {

    public TasksCentricNavigationController(TasksCentricController parentController) {
        super(parentController);
    }

    public Layout buildView() {
        this.view = new TasksCentricNavigationView(this);
        return this.view.build();
    }

    public void changeFromDate(long fromDate) {
        TasksCentricNavigationModel navigationModel = (TasksCentricNavigationModel) this.model;
        if (fromDate != navigationModel.getFromDate()) {
            navigationModel.setFromDate(fromDate);
            this.parentController.resetPendingTasksRequests();
            this.paginationController.firstPage();
        }
    }

    public void changeToDate(long toDate) {
        TasksCentricNavigationModel navigationModel = (TasksCentricNavigationModel) this.model;
        if (toDate != navigationModel.getToDate()) {
            navigationModel.setToDate(toDate);
            this.parentController.resetPendingTasksRequests();
            this.paginationController.firstPage();
        }
    }

}
