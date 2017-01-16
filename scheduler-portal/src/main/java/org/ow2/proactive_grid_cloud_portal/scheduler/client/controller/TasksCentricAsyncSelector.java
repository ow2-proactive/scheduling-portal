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

import org.ow2.proactive_grid_cloud_portal.common.client.json.JSONException;
import org.ow2.proactive_grid_cloud_portal.common.client.json.JSONUtils;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LogModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Job;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.json.SchedulerJSONUtils;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.TasksCentricModel;

import com.google.gwt.user.client.rpc.AsyncCallback;


/**
 * Async callback that selects the given task in the model
 * @author ActiveEon Team
 */
public class TasksCentricAsyncSelector implements AsyncCallback<String> {

    private final TasksCentricModel tasksCentricModel;

    private final String jobId;

    public TasksCentricAsyncSelector(TasksCentricModel tasksCentricModel, String jobId) {
        this.tasksCentricModel = tasksCentricModel;
        this.jobId = jobId;
    }

    @Override
    public void onFailure(Throwable caught) {
        caught.printStackTrace();
        String msg = JSONUtils.getJsonErrorMessage(caught);
        LogModel.getInstance().logImportantMessage("Failed to get job info for job " + jobId + ": " + msg);
    }

    @Override
    public void onSuccess(String result) {
        try {
            Job job = SchedulerJSONUtils.getJobInfoFromJson(result);
            tasksCentricModel.setTaskSelectedJob(job);
        } catch (JSONException e) {
            LogModel.getInstance().logCriticalMessage(e.getMessage());
        }
    }
}
