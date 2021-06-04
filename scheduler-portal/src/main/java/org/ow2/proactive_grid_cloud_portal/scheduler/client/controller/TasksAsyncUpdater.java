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

import org.ow2.proactive_grid_cloud_portal.common.client.json.JSONUtils;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LogModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerController;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.json.JSONPaginatedTasks;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.json.SchedulerJSONUtils;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.TasksModel;

import com.google.gwt.user.client.rpc.AsyncCallback;


/**
 * Async callback that refreshes the task list in the given model
 * @author ActiveEon Team
 */
public class TasksAsyncUpdater implements AsyncCallback<String> {

    private TasksModel model;

    private SchedulerController parentController;

    public TasksAsyncUpdater(TasksModel model, SchedulerController parentController) {
        this.model = model;
        this.parentController = parentController;
    }

    @Override
    public void onFailure(Throwable caught) {
        String msg = JSONUtils.getJsonErrorMessage(caught);
        model.taskUpdateError(msg);
        LogModel.getInstance().logImportantMessage("Failed to update tasks for job : " + msg);
        parentController.setExecutionsUpdated(true);
    }

    @Override
    public void onSuccess(String result) {
        try {
            JSONPaginatedTasks tasks = SchedulerJSONUtils.parseJSONPaginatedTasks(result);
            model.setTasksDirty(false);
            model.setTasks(tasks.getTasks(), tasks.getTotalTasks());
            // do not model.logMessage() : this is repeated by a timer
        } catch (org.ow2.proactive_grid_cloud_portal.common.client.json.JSONException e) {
            LogModel.getInstance().logCriticalMessage(e.getMessage());
        }
        parentController.setExecutionsUpdated(true);
    }
}
