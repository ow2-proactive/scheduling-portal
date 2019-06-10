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

import static org.ow2.proactive_grid_cloud_portal.scheduler.client.view.ResultView.DESTINATION_FIELD_NAME;
import static org.ow2.proactive_grid_cloud_portal.scheduler.client.view.ResultView.JOB_ID_FIELD_NAME;
import static org.ow2.proactive_grid_cloud_portal.scheduler.client.view.ResultView.SESSION_ID_FIELD_NAME;
import static org.ow2.proactive_grid_cloud_portal.scheduler.client.view.ResultView.TASK_ID_FIELD_NAME;

import java.util.List;

import org.ow2.proactive_grid_cloud_portal.common.client.model.LoginModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Job;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.JobStatus;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerController;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.JobSelectedListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.TaskResultListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerModelImpl;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.ExecutionsModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.JobsModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.KeyValueGrid;

import com.google.gwt.core.client.GWT;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.FormMethod;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.HiddenItem;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;


public class JobResultView implements JobSelectedListener, TaskResultListener {

    private static final String NO_JOB_SELECED = "No job selected.";

    private static final String NO_ITEMS_TO_SHOW = "<b>Result List: no items to show.</b>";

    /**
     * label when no job is selected or job is not finished
     */
    private Label placeHolderLabel;

    private Label preciousResultLabel;

    private KeyValueGrid resultMap;

    private SchedulerController controller;

    private VLayout preciousButtons;

    private DynamicForm downloadForm;

    private Job selectedJob;

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

        preciousResultLabel = new Label("<b>Result List:</b>");
        preciousResultLabel.setAutoHeight();
        preciousResultLabel.setWidth100();
        preciousResultLabel.hide();
        root.addMember(preciousResultLabel);

        HiddenItem sess = new HiddenItem(SESSION_ID_FIELD_NAME);
        sess.setValue(LoginModel.getInstance().getSessionId());

        HiddenItem job = new HiddenItem(JOB_ID_FIELD_NAME);
        HiddenItem destination = new HiddenItem(DESTINATION_FIELD_NAME);
        HiddenItem task = new HiddenItem(TASK_ID_FIELD_NAME);

        downloadForm = new DynamicForm();
        downloadForm.setWidth100();
        downloadForm.setAutoHeight();
        downloadForm.setMethod(FormMethod.POST);
        downloadForm.setFields(sess, job, destination, task);
        downloadForm.setAction(GWT.getModuleBaseURL() + "downloader");

        root.addMember(downloadForm);

        preciousButtons = new VLayout();
        preciousButtons.setMembersMargin(10);
        preciousButtons.setWidth100();
        root.addMember(preciousButtons);

        resultMap = new KeyValueGrid("Result Map");
        resultMap.setWidth100();
        resultMap.setAutoHeight();
        resultMap.hide();
        root.addMember(resultMap);

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
        selectedJob = null;
        placeHolderLabel.setContents(NO_JOB_SELECED);
        placeHolderLabel.show();
        resultMap.hide();
        preciousButtons.hide();
        preciousResultLabel.hide();

    }

    private void showJobNotFinished(Job job) {
        selectedJob = null;
        placeHolderLabel.setContents("Job[<b>" + job.getId() + "</b>] is not finished.");
        placeHolderLabel.show();
        resultMap.hide();
        preciousButtons.hide();
        preciousResultLabel.hide();
    }

    private void showFinishedJobSelected(Job job) {
        selectedJob = job;
        placeHolderLabel.setContents("Loading...");
        placeHolderLabel.show();
        resultMap.buildEntries(job.getResultMap());
        resultMap.hide();
        preciousButtons.hide();
        preciousResultLabel.hide();

        controller.getExecutionController().getJobsController().fetchMetadataOfPreciousResults();
    }

    @Override
    public void preciousTaskNamesLoaded(List<String> preciousTaskNames) {
        HLayout[] iButtons = preciousTaskNames.stream().map(preciousTask -> {
            HLayout row = new HLayout();
            row.setWidth100();
            row.setMargin(1);
            row.setPadding(1);
            row.setAutoHeight();
            row.setMembersMargin(10);

            Label label = new Label(preciousTask);
            label.setWidth(100);
            label.setHeight(22);
            label.setValign(VerticalAlignment.CENTER);
            row.addMember(label);

            IButton openInBrowser = new IButton("Open in browser");
            openInBrowser.setWidth(200);
            openInBrowser.setHeight(22);

            openInBrowser.addClickHandler(event -> doDownload(preciousTask, downloadForm, "browser", "_blank"));
            row.addMember(openInBrowser);

            IButton saveAsFile = new IButton("Save as file");
            saveAsFile.setHeight(22);
            saveAsFile.setWidth(200);
            saveAsFile.addClickHandler(event -> doDownload(preciousTask, downloadForm, "file", "_top"));
            row.addMember(saveAsFile);

            return row;
        }).toArray(HLayout[]::new);
        preciousButtons.setMembers(iButtons);

        placeHolderLabel.hide();
        resultMap.show();
        preciousResultLabel.setContents(NO_ITEMS_TO_SHOW);
        preciousResultLabel.show();
        preciousButtons.show();
    }

    private void doDownload(String readableName, DynamicForm form, String contentType, String target) {
        if (selectedJob != null) {
            form.getField(ResultView.TASK_ID_FIELD_NAME).setValue(readableName);

            form.getField(ResultView.JOB_ID_FIELD_NAME).setValue(selectedJob.getId().toString());

            form.getField(ResultView.DESTINATION_FIELD_NAME).setValue(contentType);

            form.setTarget(target);

            form.submitForm();

        }
    }
}
