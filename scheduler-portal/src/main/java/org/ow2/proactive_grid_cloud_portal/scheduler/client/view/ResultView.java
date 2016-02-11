/*
 * ################################################################
 *
 * ProActive Parallel Suite(TM): The Java(TM) library for
 *    Parallel, Distributed, Multi-Core Computing for
 *    Enterprise Grids & Clouds
 *
 * Copyright (C) 1997-2015 INRIA/University of
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

import org.ow2.proactive_grid_cloud_portal.common.client.model.LoginModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.DownloadOption;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.JobOutput;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.JobOutputListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.TaskSelectedListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerModelImpl;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SelectionTarget;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Task;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.ResultController;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.ExecutionsModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.TasksCentricModel;

import com.google.gwt.core.client.GWT;
import com.smartgwt.client.types.FormMethod;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.HiddenItem;
import com.smartgwt.client.widgets.form.fields.RadioGroupItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
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
public class ResultView implements TaskSelectedListener, JobOutputListener {

    public static final String TASK_ID_FIELD_NAME = "taskId";
    public static final String MEDIA_FIELD_NAME = "media";
    public static final String JOB_ID_FIELD_NAME = "jobId";
    public static final String SESSION_ID_FIELD_NAME = "sessionId";

    protected final String visuActivatedMessage = "Remote viszualization activated";

    protected final String visuDesactivatedMessage = "Remote viszualization desactivated. Please toggle streaming in output view for a job to activate the remote visuzalization";

    protected final String noTaskSelectedMessage = "No Task selected";

    protected Layout root = null;

    protected Label visuLabelValue = null;
    protected Layout visuPane = null;

    protected Layout formPane = null;
    protected Label taskSelectedLabel = null;
    protected RadioGroupItem downloadTypeRadio;
    protected DynamicForm downloadForm;
    protected IButton cmdDownload;

    protected ResultController controller = null;

    public ResultView(ResultController controller) {
        this.controller = controller;
        SchedulerModelImpl schedulerModel = (SchedulerModelImpl) controller.getParentController().getModel();

        schedulerModel.getTasksModel().addTaskSelectedListener(this);

        ExecutionsModel executionsModel = schedulerModel.getExecutionsModel();
        TasksCentricModel tasksCentricModel = executionsModel.getTasksModel();
        tasksCentricModel.addTaskSelectedListener(this);

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
        Label visuLabelTitle = new Label(
                "<span style='text-align:center;color:#003168'><b>Remote Visualization :</b></span>");
        visuLabelTitle.setHeight(30);

        this.visuLabelValue = new Label(this.visuDesactivatedMessage);
        this.visuLabelValue.setHeight(50);
        this.visuLabelValue.setLeft(40);

        visuPane = new VLayout();

        visuPane.setWidth100();
        visuPane.setHeight(120);
        visuPane.setMembers(visuLabelTitle, this.visuLabelValue);
    }


    protected void buildTaskPreviewPane() {
        Label taskPreviewLabelTitle = new Label(
                "<span style='text-align:center;color:#003168'><b>Task preview :</b></span>");
        taskPreviewLabelTitle.setHeight(30);

        this.taskSelectedLabel = new Label(this.noTaskSelectedMessage);
        this.taskSelectedLabel.setHeight(30);
        this.taskSelectedLabel.setLeft(20);

        this.downloadTypeRadio = new RadioGroupItem("type");
        this.downloadTypeRadio.setShowTitle(false);
        this.downloadTypeRadio.setWidth(200);
        this.downloadTypeRadio.setValueMap(DownloadOption.OPT_TEXT.label, DownloadOption.OPT_BIN.label);
        this.downloadTypeRadio.setValue(DownloadOption.OPT_TEXT.label);
        this.downloadTypeRadio.addChangedHandler(new ChangedHandler() {
            @Override
            public void onChanged(ChangedEvent event) {
                downloadTypeChangedHandler();
            }
        });

        final HiddenItem sess = new HiddenItem(SESSION_ID_FIELD_NAME);
        sess.setValue(LoginModel.getInstance().getSessionId());

        final HiddenItem job = new HiddenItem(JOB_ID_FIELD_NAME);
        final HiddenItem media = new HiddenItem(MEDIA_FIELD_NAME);
        final HiddenItem task = new HiddenItem(TASK_ID_FIELD_NAME);

        this.downloadForm = new DynamicForm();
        this.downloadForm.setWidth(250);
        this.downloadForm.setMethod(FormMethod.POST);
        this.downloadForm.setTarget("_blank");
        this.downloadForm.setFields(this.downloadTypeRadio, sess, job, media, task);
        this.downloadForm.setAction(GWT.getModuleBaseURL() + "downloader");

        this.cmdDownload = new IButton("Download");
        this.cmdDownload.setLeft(20);
        cmdDownload.addClickHandler(new ClickHandler() {
            public void onClick(ClickEvent event) {
                controller.doDownload(downloadForm);
            }
        });
        formPane = new VLayout();
        formPane.setMembersMargin(10);
        formPane.setWidth100();
        formPane.setMembers(taskPreviewLabelTitle, this.taskSelectedLabel, this.downloadForm, cmdDownload);
    }


    protected void downloadTypeChangedHandler() {
        String val = this.downloadTypeRadio.getValueAsString();
        this.controller.changeDownloadType(val);
    }


    protected void goToNoSelectedTaskState() {
        this.cmdDownload.setDisabled(true);
        this.downloadTypeRadio.setDisabled(true);
        this.taskSelectedLabel.setContents(this.noTaskSelectedMessage);
    }


    @Override
    public void taskSelected(Task task) {
        if (task == null) {
            this.goToNoSelectedTaskState();
        } else {
            this.cmdDownload.setDisabled(false);
            this.downloadTypeRadio.setDisabled(false);
            String label = "Task " + task.getName() + " (id: " + Long.toString(
                    task.getId()) + ") from job " + task.getJobName() + " (id: " + Long.toString(
                    task.getJobId()) + ")";
            this.taskSelectedLabel.setContents(label);
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

}
