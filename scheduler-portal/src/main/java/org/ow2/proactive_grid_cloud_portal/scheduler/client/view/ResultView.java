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

import java.util.ArrayList;
import java.util.List;

import org.ow2.proactive_grid_cloud_portal.common.client.model.LoginModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Job;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerController;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.ExecutionDisplayModeListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.JobSelectedListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.TaskSelectedListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.TasksUpdatedListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerModelImpl;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Task;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.ExecutionListMode;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.ExecutionsModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.JobsModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.TasksCentricModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.TasksModel;

import com.google.gwt.core.client.GWT;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.FormMethod;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.HiddenItem;
import com.smartgwt.client.widgets.form.fields.RadioGroupItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;


/**
 * Sort of 'result preview' from the original RCP client
 * <p>
 * Actually delegates previewing to the browser, letting
 * the user pick a task and a mimetype before clicking the link
 * 
 * @author mschnoor
 *
 */
public class ResultView implements TasksUpdatedListener, JobSelectedListener, TaskSelectedListener, ExecutionDisplayModeListener {

    private static final String OPT_TEXT = "View as text";
    private static final String OPT_BIN = "Download as binary";

    private Layout root = null;

    private SelectItem taskSelect = null;

    private SchedulerController controller = null;

    private Label label = null;

    private Layout formPane = null;
    private Layout visuPane = null;
    private IButton visuButton = null;

    private String jobId = null;

    public ResultView(SchedulerController controller) {
        this.controller = controller;
        SchedulerModelImpl schedulerModel = (SchedulerModelImpl) controller.getModel();
        schedulerModel.getTasksModel().addTasksUpdatedListener(this);
        JobsModel jobsModel = schedulerModel.getExecutionsModel().getJobsModel();
        jobsModel.addJobSelectedListener(this);
        
        ExecutionsModel executionsModel = schedulerModel.getExecutionsModel();
        TasksCentricModel tasksCentricModel = executionsModel.getTasksModel();
        tasksCentricModel.addTaskSelectedListener(this);
        tasksCentricModel.addJobSelectedListener(this);
        
        executionsModel.addExecutionsDisplayModeListener(this);
        
    }

    /**
     * Create the widget and return it
     * 
     * @return the ResultView widget ready to be added in a container
     */
    public Layout build() {
        this.root = new VLayout();
        this.root.setWidth100();
        this.root.setHeight100();

        this.label = new Label("No job selected");
        this.label.setAlign(Alignment.CENTER);
        this.label.setWidth100();
        this.label.setHeight100();

        this.taskSelect = new SelectItem("tid", "Task result");

        final RadioGroupItem radio = new RadioGroupItem("type", "Type");
        radio.setValueMap(OPT_TEXT, OPT_BIN);
        radio.setValue("Text");

        final HiddenItem sess = new HiddenItem("sessionId");
        sess.setValue(LoginModel.getInstance().getSessionId());

        final HiddenItem job = new HiddenItem("jobId");
        final HiddenItem media = new HiddenItem("media");
        final HiddenItem task = new HiddenItem("taskId");

        final DynamicForm form = new DynamicForm();
        form.setWidth(250);
        form.setMethod(FormMethod.POST);
        form.setTarget("_blank");
        form.setFields(taskSelect, radio, sess, job, media, task);
        form.setAction(GWT.getModuleBaseURL() + "downloader");

        final IButton dl = new IButton("Download");
        dl.addClickHandler(new ClickHandler() {
            public void onClick(ClickEvent event) {
                String val = radio.getValueAsString();
                if (OPT_TEXT.equals(val)) {
                    media.setValue("text/plain");
                } else if (OPT_BIN.equals(val)) {
                    media.setValue("application/octet-stream");
                } else {
                    return;
                }
                task.setValue(taskSelect.getValueAsString());
                job.setValue(jobId);
                form.submitForm();
            }
        });
        formPane = new HLayout();
        formPane.setMembersMargin(10);
        formPane.setWidth100();
        formPane.setMembers(form, dl);

        visuButton = new IButton("Activate");
        visuButton.addClickHandler(new ClickHandler() {
            public void onClick(ClickEvent event) {
                controller.getOutputController().getLiveOutput();
                visuButton.setDisabled(true);
            }
        });

        Label visuLabel = new Label(
            "<span style='text-align:center;color:#003168'>Remote Visualization :</span>");
        visuLabel.setAlign(Alignment.RIGHT);
        visuLabel.setWidth(150);

        visuPane = new HLayout();
        visuPane.setMembersMargin(20);
        visuPane.setHeight(22);
        visuPane.setWidth100();
        visuPane.setMembers(visuLabel, visuButton);

        this.root.setMembersMargin(30);
        this.root.setMembers(label, visuPane, formPane);
        jobUnselected();

        return this.root;
    }

    public void tasksUpdating() {
    }

    public void tasksUpdated(List<Task> tasks, long totalTasks) {
        if (tasks.size() == this.taskSelect.getClientPickListData().length) {
            return;
        }

        String[] values = new String[tasks.size()];
        int i = 0;
        String defaultVal = "";

        for (Task t : tasks) {
                    defaultVal = t.getName();
                    values[i] = t.getName();
                    i++;
        }
        this.taskSelect.setValueMap(values);
        this.taskSelect.setValue(defaultVal);
    }

    public void tasksUpdatedFailure(String message) {
        this.tasksUpdated(new ArrayList<Task>(), 0);
    }

    public void jobSelected(Job job) {
        taskSelect.setValues();
        this.jobId = (job != null) ? "" + job.getId() : "";
        root.showMember(formPane);
        root.showMember(visuPane);
        root.hideMember(label);

        ExecutionsModel executionModel = ((SchedulerModelImpl) controller.getModel()).getExecutionsModel();
        JobsModel jobsModel = executionModel.getJobsModel();
        Job j = jobsModel.getSelectedJob();
        
        boolean isLiveOutput = controller.getOutputController().getModel().isLiveOutput("" + j.getId());
        boolean taskCentric = (executionModel.getMode() == ExecutionListMode.TASK_CENTRIC);
        if ((j != null && isLiveOutput) || taskCentric) {
            visuButton.setDisabled(true);
        } else {
            visuButton.setDisabled(false);
        }
    }

    
    protected void reset(){
        root.hideMember(formPane);
        root.hideMember(visuPane);
        root.showMember(label);
    }
    
    public void jobUnselected() {
        reset();
    }
    
    
    @Override
    public void selectedJobUpdated() {    
    }

    @Override
    public void modeSwitched(ExecutionListMode mode) {
        Job job = this.controller.getSelectedJob();
        if(job == null){
            jobUnselected();
        }
        else{
            jobSelected(job);
        }
        
        if(mode == ExecutionListMode.JOB_CENTRIC){
            TasksModel tasksModel = this.controller.getTasksController().getModel();
            List<Task> tasks = tasksModel.getTasks();
            long size = tasksModel.getTasksNavigationModel().getPaginationModel().getTotalItems();
            tasksUpdated(tasks, size);
        }
    }

    @Override
    public void taskSelected(Task task) {
        String taskName = task.getName(); 
        taskSelect.setValueMap(taskName);
        taskSelect.setValue(taskName);
    }

    @Override
    public void taskUnselected() {
        reset();
    }
}
