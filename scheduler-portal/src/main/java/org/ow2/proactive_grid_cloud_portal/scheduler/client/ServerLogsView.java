/*
 * ################################################################
 *
 * ProActive Parallel Suite(TM): The Java(TM) library for
 *    Parallel, Distributed, Multi-Core Computing for
 *    Enterprise Grids & Clouds
 *
 * Copyright (C) 1997-2011 INRIA/University of
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
package org.ow2.proactive_grid_cloud_portal.scheduler.client;

import java.util.List;
import java.util.Map.Entry;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.JobOutputListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.JobSelectedListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.TasksUpdatedListener;

import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.HTMLPane;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;


/**
 * Displays the output of the selected job, or a task in the selected job
 * 
 * 
 * @author mschnoor
 *
 */
public class ServerLogsView implements JobSelectedListener, TasksUpdatedListener {

    private static final String TASKS_ALL = "All Tasks";

    /** contains the layout */
    private Layout root = null;
    /** displays the job output */
    private HTMLPane text = null;
    /** click to fetch/refetch */
    private IButton refreshButton = null;
    /** drop down list of task names */
    private SelectItem taskSelect = null;
    /** display a message */
    private Label label = null;

    private SchedulerController controller;

    /**
     * Default constructor
     * @param controller
     */
    public ServerLogsView(SchedulerController controller) {
        this.controller = controller;
        this.controller.getEventDispatcher().addJobSelectedListener(this);
        this.controller.getEventDispatcher().addTasksUpdatedListener(this);
    }

    /**
     * @return the Widget to display, ready to be added in a container
     */
    public Layout build() {
        this.root = new VLayout();
        this.root.setWidth100();
        this.root.setHeight100();

        this.refreshButton = new IButton("Fetch logs");
        this.refreshButton.setTooltip("Request fetching the Output for this job");
        this.refreshButton.addClickHandler(new ClickHandler() {
            public void onClick(ClickEvent event) {
                ServerLogsView.this.text.hide();
                ServerLogsView.this.label.setContents("Please wait...");
                ServerLogsView.this.label.setIcon("loading.gif");
                ServerLogsView.this.label.show();

                int jobId = controller.getModel().getSelectedJob().getId();
                if (taskSelect.getValue().equals(TASKS_ALL)) {
                    ServerLogsView.this.controller.getJobServerLogs(jobId, new ShowLogsCallback());
                } else {
                    String taskName = (String) taskSelect.getValue();
                    ServerLogsView.this.controller.getTaskServerLogs(jobId, taskName, new ShowLogsCallback());
                }
            }
        });

        this.taskSelect = new SelectItem();
        this.taskSelect.setShowTitle(false);
        this.taskSelect.addChangedHandler(new ChangedHandler() {
            public void onChanged(ChangedEvent event) {
                //                Job sel = controller.getModel().getSelectedJob();
                //                if (sel != null) {
                //                    List<Task> tasks = controller.getModel().getTasks();
                //                }
            }
        });

        DynamicForm form = new DynamicForm();
        form.setColWidths("*", "*");
        form.setNumCols(2);
        form.setFields(taskSelect);

        HLayout buttons = new HLayout();
        buttons.setWidth100();
        buttons.setHeight(22);
        buttons.setMembersMargin(5);

        Canvas fill = new Canvas();
        fill.setWidth100();

        buttons.setMembers(form, fill, refreshButton);

        this.label = new Label();
        this.label.setWidth100();
        this.label.setAlign(Alignment.CENTER);
        this.label.hide();

        this.text = new HTMLPane();
        this.text.setHeight100();
        this.text.setWidth100();
        this.text.setShowEdges(true);
        this.text.hide();

        this.root.addMember(buttons);
        this.root.addMember(this.text);
        this.root.addMember(this.label);

        jobUnselected();

        return this.root;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.ow2.proactive_grid_cloud_portal.client.Listeners.JobSelectedListener#jobSelected(org.
     * ow2.proactive_grid_cloud_portal.shared.job.Job)
     */
    public void jobSelected(Job job) {
        this.refreshButton.setDisabled(false);

        this.taskSelect.setValueMap(TASKS_ALL);
        this.taskSelect.setValue(TASKS_ALL);

        this.clear();
    }

    public void showLogs(String logs) {
        this.clear();
        this.label.hide();
        this.text.setContents("<pre>" + logs + "</pre>");
        this.text.show();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.ow2.proactive_grid_cloud_portal.client.Listeners.JobSelectedListener#jobUnselected()
     */
    public void jobUnselected() {
        this.clear();
        this.refreshButton.hide();
        this.taskSelect.hide();
        this.taskSelect.setValueMap("<i>all tasks</i>");
        this.label.setContents("No job selected");
    }

    private void clear() {
        this.text.setContents("");
        this.text.hide();
        this.refreshButton.show();
        this.taskSelect.show();
        this.label.setContents("No logs available<br><br>"
            + "Click <strong>Fetch logs</strong> to retrieve logs for tasks<br>");
        this.label.setIcon(null);
        this.label.show();
    }

    public class ShowLogsCallback {
        public void show(String logs) {
            showLogs(logs);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.ow2.proactive_grid_cloud_portal.client.Listeners.TasksUpdatedListener#tasksUpdating(boolean
     * )
     */
    public void tasksUpdating(boolean jobChanged) {
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.ow2.proactive_grid_cloud_portal.client.Listeners.TasksUpdatedListener#tasksUpdated(org
     * .ow2.proactive_grid_cloud_portal.shared.task.TaskSet)
     */
    public void tasksUpdated(List<Task> tasks) {
        if (tasks.size() + 1 == this.taskSelect.getClientPickListData().length) {
            return;
        }

        String[] values = new String[tasks.size() + 1];
        values[0] = TASKS_ALL;
        int i = 1;

        for (Task t : tasks) {
            values[i] = t.getName();
            i++;
        }
        this.taskSelect.setValueMap(values);
        this.taskSelect.setValue(TASKS_ALL);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.ow2.proactive_grid_cloud_portal.client.Listeners.TasksUpdatedListener#tasksUpdatedFailure
     * (java.lang.String)
     */
    public void tasksUpdatedFailure(String message) {
    }
}
