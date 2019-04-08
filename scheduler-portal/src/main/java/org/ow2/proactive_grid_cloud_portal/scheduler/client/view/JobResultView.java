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
package org.ow2.proactive_grid_cloud_portal.scheduler.client.view;

import java.util.Map;
import java.util.stream.Collectors;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.Job;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.JobStatus;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerController;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerModelImpl;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.ExecutionsModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.JobsModel;

import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;


public class JobResultView implements SchedulerListeners.JobSelectedListener, SchedulerListeners.JobsUpdatedListener {

    private static final String NO_JOB_SELECED = "No job selected.";

    private static final String JOB_WITH_ID_NOT_FINISED = "Selected Job[%s] is not finished.";

    /** label when no job is selected or job is not finished*/
    protected Label placeHolderLabel;

    protected SchedulerController controller;

    public JobResultView(SchedulerController controller) {
        this.controller = controller;
        ExecutionsModel executionModel = ((SchedulerModelImpl) controller.getModel()).getExecutionsModel();
        JobsModel jobsModel = executionModel.getJobsModel();
        jobsModel.addJobSelectedListener(this);
        jobsModel.addJobsUpdatedListener(this);

    }

    /**
     * @return the Widget to display, ready to be added in a container
     */
    public Layout build() {
        Layout root = new VLayout();

        placeHolderLabel = new Label(NO_JOB_SELECED);
        placeHolderLabel.setWidth100();
        placeHolderLabel.setAlign(Alignment.CENTER);
        root.addMember(placeHolderLabel);

        return root;
    }

    @Override
    public void jobSelected(Job job) {
        if (job.getStatus().equals(JobStatus.FINISHED) || job.getStatus().equals(JobStatus.FAILED) ||
            job.getStatus().equals(JobStatus.KILLED) || job.getStatus().equals(JobStatus.CANCELED)) {
            final String results = job.getResultMap()
                                      .entrySet()
                                      .stream()
                                      .map(entry -> entry.getKey() + " -> " + entry.getValue())
                                      .collect(Collectors.joining(";"));
            placeHolderLabel.setContents(results);
        } else {
            placeHolderLabel.setContents(String.format(JOB_WITH_ID_NOT_FINISED, job.getId().toString()));
        }
    }

    @Override
    public void jobUnselected() {

    }

    @Override
    public void selectedJobUpdated(Job job) {

    }

    @Override
    public void jobsUpdated(Map<Integer, Job> jobs) {

    }

    @Override
    public void jobsUpdating() {

    }

    @Override
    public void jobSubmitted(Job j) {

    }
}
