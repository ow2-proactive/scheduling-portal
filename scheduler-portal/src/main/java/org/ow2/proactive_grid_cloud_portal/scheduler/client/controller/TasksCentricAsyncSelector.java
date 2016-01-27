/*
 * ProActive Parallel Suite(TM): The Java(TM) library for
 *    Parallel, Distributed, Multi-Core Computing for
 *    Enterprise Grids & Clouds
 *
 * Copyright (C) 1997-2016 INRIA/University of
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
 * Initial developer(s):               The ProActive Team
 *                         http://proactive.inria.fr/team_members.htm
 */
package org.ow2.proactive_grid_cloud_portal.scheduler.client.controller;

import com.google.gwt.user.client.rpc.AsyncCallback;
import org.ow2.proactive_grid_cloud_portal.common.client.json.JSONException;
import org.ow2.proactive_grid_cloud_portal.common.client.json.JSONUtils;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LogModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Job;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.json.SchedulerJSONUtils;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.TasksCentricModel;

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
