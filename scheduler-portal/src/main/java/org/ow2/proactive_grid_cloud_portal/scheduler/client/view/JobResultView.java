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

import java.util.List;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.Job;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.JobStatus;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerController;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.JobSelectedListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.TaskResultListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerModelImpl;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.TaskResultData;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.ExecutionsModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.JobsModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.KeyValueGrid;

import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;


public class JobResultView implements JobSelectedListener, TaskResultListener {

    private static final String NO_JOB_SELECED = "No job selected.";

    /** label when no job is selected or job is not finished*/
    private Label placeHolderLabel;

    private Label preciousResultLabel;

    private KeyValueGrid resultMap;

    private SchedulerController controller;

    private VLayout preciousButtons;

    public JobResultView(SchedulerController controller) {
        this.controller = controller;
        ExecutionsModel executionModel = ((SchedulerModelImpl) controller.getModel()).getExecutionsModel();
        JobsModel jobsModel = executionModel.getJobsModel();
        jobsModel.addJobSelectedListener(this);
        jobsModel.addTaskResultListener(this);
    }

    /**
     * @return the Widget to display, ready to be added in a container
     */
    public Layout build() {
        Layout root = new VLayout();
        root.setWidth100();
        root.setHeight100();
        root.setPadding(20);

        placeHolderLabel = new Label(NO_JOB_SELECED);
        placeHolderLabel.setWidth100();
        placeHolderLabel.setAlign(Alignment.CENTER);
        root.addMember(placeHolderLabel);

        resultMap = new KeyValueGrid("Result Map");
        resultMap.setWidth100();
        resultMap.hide();
        root.addMember(resultMap);

        preciousResultLabel = new Label("Job Precious Results");
        preciousResultLabel.setWidth100();
        preciousResultLabel.hide();
        root.addMember(preciousResultLabel);

        preciousButtons = new VLayout();
        preciousButtons.setMembersMargin(10);
        preciousButtons.setWidth100();
        root.addMember(preciousButtons);

        showNoJobSelected();
        return root;
    }

    @Override
    public void jobSelected(Job job) {
        if (isFinished(job)) {
            showFinishedJobSelected(job);
        } else {
            showJobNotFinished(job);
        }
    }

    private boolean isFinished(Job job) {
        return job.getStatus().equals(JobStatus.FINISHED) || job.getStatus().equals(JobStatus.FAILED) ||
               job.getStatus().equals(JobStatus.KILLED) || job.getStatus().equals(JobStatus.CANCELED);
    }

    @Override
    public void jobUnselected() {
        showNoJobSelected();
    }

    @Override
    public void selectedJobUpdated(Job job) {
        if (isFinished(job)) {
            showFinishedJobSelected(job);
        } else {
            showJobNotFinished(job);
        }
    }

    private void showNoJobSelected() {
        placeHolderLabel.setContents(NO_JOB_SELECED);
        placeHolderLabel.show();
        resultMap.hide();
        preciousButtons.hide();
    }

    private void showJobNotFinished(Job job) {
        placeHolderLabel.setContents("Job[<b>" + job.getId() + "</b>] is not finished.");
        placeHolderLabel.show();
        resultMap.hide();
        preciousButtons.hide();
    }

    private void showFinishedJobSelected(Job job) {
        placeHolderLabel.setContents("Loading...");
        placeHolderLabel.show();
        resultMap.buildEntries(job.getResultMap());
        resultMap.hide();
        preciousResultLabel.hide();

        controller.getExecutionController().getJobsController().fetchMetadataOfPreciousResults();
    }

    @Override
    public void taskResultLoaded(List<TaskResultData> result) {
        IButton[] iButtons = result.stream().map(resultData -> {
            IButton button = new IButton(resultData.getReadableName());
            button.setLeft(20);
            button.setWidth(200);
            return button;
        }).toArray(IButton[]::new);
        preciousButtons.setMembers(iButtons);

        placeHolderLabel.hide();
        resultMap.show();
        preciousResultLabel.show();
        preciousButtons.show();
    }
}
