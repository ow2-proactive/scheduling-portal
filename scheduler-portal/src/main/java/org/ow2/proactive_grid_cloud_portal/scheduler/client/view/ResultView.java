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

import org.ow2.proactive_grid_cloud_portal.common.client.model.LoginModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.JobOutput;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.JobOutputListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.TaskSelectedListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.TasksUpdatedListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerModelImpl;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SelectionTarget;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Task;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.ResultController;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.ExecutionsModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.TasksCentricModel;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.Timer;
import com.smartgwt.client.types.FormMethod;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.HiddenItem;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;


/**
 * Sort of 'result preview' from the original RCP client
 * <p/>
 * Actually delegates previewing to the browser, letting
 * the user pick a task and a mimetype before clicking the link
 *
 * @author mschnoor
 */
public class ResultView implements TaskSelectedListener, TasksUpdatedListener, JobOutputListener {

    public static final String TASK_ID_FIELD_NAME = "taskId";

    public static final String DESTINATION_FIELD_NAME = "destination";

    public static final String JOB_ID_FIELD_NAME = "jobId";

    public static final String SESSION_ID_FIELD_NAME = "sessionId";

    protected final String visuActivatedMessage = "Remote visualization is enabled.";

    protected final String visuDesactivatedMessage = "Remote visualization is disabled. Please toggle streaming in output view for a job in order to enable the remote visualization.";

    protected final String noTaskSelectedMessage = "Please select a task from the Tasks tab on the left panel.";

    protected Layout root = null;

    protected Label visuLabelValue = null;

    protected Layout visuPane = null;

    protected Layout formPane = null;

    protected Label taskSelectedLabel = null;

    protected DynamicForm downloadForm;

    protected IButton openInBrowser;

    protected IButton saveAsFile;

    protected Task selectedTask = null;

    protected ResultController controller = null;

    public ResultView(ResultController controller) {
        this.controller = controller;
        SchedulerModelImpl schedulerModel = (SchedulerModelImpl) controller.getParentController().getModel();

        schedulerModel.getTasksModel().addTaskSelectedListener(this);
        schedulerModel.getTasksModel().addTasksUpdatedListener(this);

        ExecutionsModel executionsModel = schedulerModel.getExecutionsModel();
        TasksCentricModel tasksCentricModel = executionsModel.getTasksModel();
        tasksCentricModel.addTaskSelectedListener(this);
        tasksCentricModel.addTasksUpdatedListener(this);

        schedulerModel.getOutputModel().addJobOutputListener(this);
    }

    /**
     * Create the widget and return it
     *
     * @return the ResultView widget ready to be added in a container
     */
    public Layout build() {
        this.root = new VLayout();
        this.root.setWidth100();
        this.root.setPadding(20);

        this.buildRemoteVisuPane();
        this.buildTaskPreviewPane();

        this.root.setMembers(visuPane, formPane);

        this.goToNoSelectedTaskState();

        return this.root;
    }

    protected void buildRemoteVisuPane() {
        Label visuLabelTitle = new Label("<span style='text-align:center;color:#003168'><b>Remote Visualization</b></span>");
        visuLabelTitle.setHeight(30);

        this.visuLabelValue = new Label(this.visuDesactivatedMessage);
        this.visuLabelValue.setHeight(30);
        this.visuLabelValue.setLeft(40);

        visuPane = new VLayout();

        visuPane.setWidth100();
        visuPane.setHeight(80);
        visuPane.setMembers(visuLabelTitle, this.visuLabelValue);
    }

    protected void buildTaskPreviewPane() {
        Label taskPreviewLabelTitle = new Label("<span style='text-align:center;color:#003168'><b>Task Result</b></span>");
        taskPreviewLabelTitle.setHeight(20);

        this.taskSelectedLabel = new Label(this.noTaskSelectedMessage);
        this.taskSelectedLabel.setHeight(20);
        this.taskSelectedLabel.setLeft(20);

        final HiddenItem sess = new HiddenItem(SESSION_ID_FIELD_NAME);
        sess.setValue(LoginModel.getInstance().getSessionId());

        final HiddenItem job = new HiddenItem(JOB_ID_FIELD_NAME);
        final HiddenItem destination = new HiddenItem(DESTINATION_FIELD_NAME);
        final HiddenItem task = new HiddenItem(TASK_ID_FIELD_NAME);

        this.downloadForm = new DynamicForm();
        this.downloadForm.setWidth100();
        this.downloadForm.setMethod(FormMethod.POST);
        this.downloadForm.setFields(sess, job, destination, task);
        this.downloadForm.setAction(GWT.getModuleBaseURL() + "downloader");

        this.openInBrowser = new IButton("Open in browser");
        this.openInBrowser.setLeft(20);
        this.openInBrowser.setWidth(200);
        openInBrowser.addClickHandler(event -> controller.doDownload(downloadForm, "browser", "_blank"));
        this.saveAsFile = new IButton("Save as file");
        this.saveAsFile.setLeft(20);
        this.saveAsFile.setWidth(200);

        saveAsFile.addClickHandler(event -> controller.doDownload(downloadForm, "file", "_top"));
        formPane = new VLayout();
        formPane.setMembersMargin(10);
        formPane.setWidth100();
        formPane.setMembers(taskPreviewLabelTitle,
                            this.taskSelectedLabel,
                            this.downloadForm,
                            openInBrowser,
                            this.saveAsFile);
    }

    protected void goToNoSelectedTaskState() {
        this.openInBrowser.setDisabled(true);
        this.saveAsFile.setDisabled(true);
        this.taskSelectedLabel.setContents(this.noTaskSelectedMessage);
        this.selectedTask = null;
    }

    @Override
    public void taskSelected(Task task) {
        if (task == null) {
            this.goToNoSelectedTaskState();
        } else {
            String label = "Task " + task.getName() + " (id: " + task.getId() + ") from job " + task.getJobName() +
                           " (id: " + task.getJobId() + ")";
            this.taskSelectedLabel.setContents(label);
            this.selectedTask = task;
            decideButtonsStatus(task);
        }
    }

    public void openInBrowser() {
        Timer timer = new Timer() {
            @Override
            public void run() {
                if (!openInBrowser.isDisabled()) {
                    controller.doDownload(downloadForm, "browser", "_blank");
                }
            }
        };
        timer.schedule(200);
    }

    private void decideButtonsStatus(Task task) {
        switch (task.getStatus()) {
            case FAULTY:
            case IN_ERROR:
            case NOT_RESTARTED:
            case NOT_STARTED:
            case FAILED:
            case WAITING_ON_FAILURE:
            case WAITING_ON_ERROR:
                // allow to see the error
                this.openInBrowser.setDisabled(false);
                this.saveAsFile.setDisabled(true);
                break;
            case RUNNING:
                // if at least one failed execution attempt has been made, it should be possible to view the error
                if (task.getMaxNumberOfExec() - task.getNumberOfExecLeft() > 0) {
                    this.openInBrowser.setDisabled(false);
                    this.saveAsFile.setDisabled(true);
                } else {
                    this.openInBrowser.setDisabled(true);
                    this.saveAsFile.setDisabled(true);
                }
                break;
            case FINISHED:
                this.openInBrowser.setDisabled(false);
                this.saveAsFile.setDisabled(false);
                break;
            default:
                this.openInBrowser.setDisabled(true);
                this.saveAsFile.setDisabled(true);
                break;
        }
    }

    @Override
    public void taskUnselected() {
        this.goToNoSelectedTaskState();
    }

    @Override
    public void liveToggled(boolean newValue) {
        if (newValue) {
            this.visuLabelValue.setContents(this.visuActivatedMessage);
        } else {
            this.visuLabelValue.setContents(this.visuDesactivatedMessage);
        }
    }

    @Override
    public void jobOutputUpdated(JobOutput output, SelectionTarget target) {
    }

    @Override
    public void liveEnabled(boolean newValue) {
    }

    @Override
    public void tasksUpdating() {

    }

    @Override
    public void tasksUpdated(List<Task> tasks, long totalTasks) {
        if (selectedTask != null) {
            for (Task task : tasks) {
                if (task.getId().equals(selectedTask.getId())) {
                    selectedTask = task;
                }
            }
            decideButtonsStatus(selectedTask);
        }
    }

    @Override
    public void tasksUpdatedFailure(String message) {

    }
}
