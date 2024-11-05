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

import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerController;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerModelImpl;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Task;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.ResultModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.ResultView;

import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.layout.Layout;


public class ResultController {

    protected SchedulerController parentController;

    protected ResultModel model;

    protected ResultView view;

    public ResultController(SchedulerController parentController) {
        this.parentController = parentController;
        SchedulerModelImpl schedulerModel = (SchedulerModelImpl) this.parentController.getModel();
        this.model = new ResultModel(schedulerModel);
    }

    public Layout buildView() {
        this.view = new ResultView(this);
        return this.view.build();
    }

    public void doDownload(DynamicForm form, String contentType, String target) {
        Task task = this.parentController.getSelectedTask();
        if (task != null) {
            String taskId = task.getName();
            form.getField(ResultView.TASK_ID_FIELD_NAME).setValue(taskId);

            String jobId = Long.toString(task.getJobId());
            form.getField(ResultView.JOB_ID_FIELD_NAME).setValue(jobId);

            form.getField(ResultView.DESTINATION_FIELD_NAME).setValue(contentType);

            form.setTarget(target);

            form.submitForm();
        }
    }

    public void openInBrowser() {
        this.view.openInBrowser();
    }

    public SchedulerController getParentController() {
        return parentController;
    }

}
