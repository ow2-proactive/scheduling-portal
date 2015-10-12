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
package org.ow2.proactive_grid_cloud_portal.scheduler.client;

import java.util.List;
import java.util.Map.Entry;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.JobOutputListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.JobSelectedListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.TasksUpdatedListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.shared.SchedulerConfig;
import com.google.gwt.user.client.Window;
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
public class OutputView implements JobSelectedListener, JobOutputListener, TasksUpdatedListener {

    private static final String TASKS_ALL = "All Tasks";

    private static final String LOG_OUT_ERR = "Out & Err (1024 lines)";
    private static final String LOG_ERR = "Std Err";
    private static final String LOG_OUT = "Std Out";
    private static final String LOG_FULL = "Full logs (download)";

    /** displays the job output */
    private HTMLPane text = null;
    /** click to fetch/refetch */
    private IButton refreshButton = null;
    /** whether fetch live logs */
    private CheckboxItem liveCheck = null;
    /** drop down list of task names */
    private SelectItem taskSelect = null;
    /** stdout, stderr or both */
    private SelectItem outSelect = null;
    /** display a message */
    private Label label = null;
    /** id of the job currently displayed, or 0 */
    private int jobId = 0;
    /** true if the log for the current task is live */
    private boolean isLive = false;

    private SchedulerController controller;

    public OutputView(SchedulerController controller) {
        this.controller = controller;
        this.controller.getEventDispatcher().addJobSelectedListener(this);
        this.controller.getEventDispatcher().addJobOutputListener(this);
        this.controller.getEventDispatcher().addTasksUpdatedListener(this);
    }

    /**
     * @return the Widget to display, ready to be added in a container
     */
    public Layout build() {
        /* contains the layout */
        Layout root = new VLayout();
        root.setWidth100();
        root.setHeight100();

        this.refreshButton = new IButton("Fetch output");
        this.refreshButton.setTooltip("Request fetching the Output for this job");
        this.refreshButton.addClickHandler(new ClickHandler() {
            public void onClick(ClickEvent event) {

                String selMode = outSelect.getValueAsString();
                if(selMode.equals(LOG_FULL)) {
                    if (taskSelect.getValue().equals(TASKS_ALL)) {
                        downloadFullJobLogs(controller.getModel().getSessionId(), String.valueOf(jobId));
                    } else {
                        downloadFullTaskLogs(controller.getModel().getSessionId(), String.valueOf(jobId), (String) taskSelect.getValue());
                    }
                    return;
                }

                OutputView.this.text.hide();
                OutputView.this.label.setContents("Please wait...");
                OutputView.this.label.setIcon("loading.gif");
                OutputView.this.label.show();

                isLive = false;
                int mode;
                if (selMode.equals(LOG_OUT_ERR)) {
                    mode = SchedulerServiceAsync.LOG_ALL;
                } else if (selMode.equals(LOG_ERR)) {
                    mode = SchedulerServiceAsync.LOG_STDERR;
                } else {
                    mode = SchedulerServiceAsync.LOG_STDOUT;
                }

                int jobId = controller.getModel().getSelectedJob().getId();
                if (taskSelect.getValue().equals(TASKS_ALL)) {
                    OutputView.this.controller.getJobOutput(mode);
                } else {

                    String taskName = (String) taskSelect.getValue();
                    Task task = null;
                    for (Task t : controller.getModel().getTasks()) {
                        if (taskName.equals(t.getName())) {
                            task = t;
                            break;
                        }
                    }
                    if (task != null) {
                        OutputView.this.controller.getTaskOutput(jobId, task, mode);
                    } else {
                        clear();
                    }
                }
            }
        });

        this.liveCheck = new CheckboxItem("liveLogs", "Streaming &nbsp;&nbsp;");
        this.liveCheck.setHeight(22);
        this.liveCheck.setTooltip("Stream output to peek in currently running tasks");
        this.liveCheck.addChangedHandler(new ChangedHandler() {
            public void onChanged(ChangedEvent event) {
                if (liveCheck.getValueAsBoolean()) {
                    taskSelect.hide();
                    outSelect.hide();
                    refreshButton.hide();

                    OutputView.this.text.hide();
                    OutputView.this.label.setContents("Please wait...");
                    OutputView.this.label.setIcon("loading.gif");
                    OutputView.this.label.show();

                    OutputView.this.controller.getLiveOutput();
                    isLive = true;
                } else {
                    taskSelect.show();
                    outSelect.show();
                    refreshButton.show();
                    OutputView.this.controller.deleteLiveLogJob();
                }
            }
        });

        this.taskSelect = new SelectItem();
        this.taskSelect.setShowTitle(false);
        this.taskSelect.addChangedHandler(new ChangedHandler() {
            public void onChanged(ChangedEvent event) {
                if (isLive)
                    return;

                Job sel = controller.getModel().getSelectedJob();
                if (sel != null) {
                    JobOutput out = controller.getModel().getJobOutput(sel.getId());
                    if (out != null && !out.getLines().isEmpty()) {
                        update(out);
                    } else {
                        clear();
                    }
                }
            }
        });

        this.outSelect = new SelectItem();
        this.outSelect.setShowTitle(false);
        this.outSelect.setValueMap(LOG_OUT_ERR, LOG_OUT, LOG_ERR, LOG_FULL);
        this.outSelect.setValue(LOG_OUT_ERR);

        DynamicForm form = new DynamicForm();
        form.setColWidths("10", "*", "*", "*");
        form.setNumCols(4);
        form.setFields(liveCheck, taskSelect, outSelect);

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

        root.addMember(buttons);
        root.addMember(this.text);
        root.addMember(this.label);

        jobUnselected();

        return root;
    }

    private void downloadFullJobLogs(String sessionId, String jobId) {
        String url = SchedulerConfig.get().getRestUrl() + "/scheduler/jobs/" + jobId + "/log/full?sessionid="+sessionId;
        Window.open(url, "_blank", "");
    }

    private void downloadFullTaskLogs(String sessionId, String jobId, String taskName) {
        String url = SchedulerConfig.get().getRestUrl() + "/scheduler/jobs/" + jobId + "/tasks/" + taskName + "/result/log/full?sessionid="+sessionId;
        Window.open(url, "_blank", "");
    }

    public void jobSelected(Job job) {
        this.refreshButton.setDisabled(false);
        if (job.getId() == this.jobId)
            return;

        this.taskSelect.setValueMap(TASKS_ALL);
        this.taskSelect.setValue(TASKS_ALL);

        this.jobId = job.getId();
        this.isLive = this.controller.getModel().isLiveOutput("" + this.jobId);

        if (job.isExecuted() && !this.isLive) {
            this.liveCheck.setDisabled(true);
        } else {
            this.liveCheck.setDisabled(false);
        }

        this.liveCheck.setValue(isLive);

        if (isLive) {
            String out = this.controller.getModel().getLiveOutput("" + jobId);
            if (out != null && out.length() > 0) {
                this.updateLive(out);
            } else {
                this.clear();
                this.outSelect.hide();
                this.taskSelect.hide();
            }
        } else {
            this.refreshButton.setDisabled(true);
            JobOutput out = this.controller.getModel().getJobOutput(job.getId());
            if (out != null && !out.getLines().isEmpty()) {
                this.update(out);
            } else {
                this.clear();
            }
        }
    }

    public void jobUnselected() {
        this.jobId = 0;
        this.refreshButton.hide();
        this.clear();
        this.refreshButton.hide();
        this.liveCheck.hide();
        this.taskSelect.hide();
        this.outSelect.hide();
        this.taskSelect.setValueMap("<i>all tasks</i>");
        this.label.setContents("No job selected");
    }

    public void jobOutputUpdated(JobOutput output) {
        if (this.isLive)
            return;

        if (this.jobId == output.getJobId()) {
            if (output.getLines().isEmpty()) {
                this.clear();
            } else {
                this.update(output);
            }
        }
    }

    public void liveOutputUpdated(String jobId, String out) {
        if (!controller.getModel().isLiveOutput(jobId))
            return;

        if (jobId.equals("" + this.jobId)) {
            if (out == null || out.length() == 0) {
                this.clear();
            } else {
                liveCheck.setValue(true);
                this.updateLive(out);
            }
        }
    }

    private void clear() {
        this.text.setContents(" "); // whitespace otherwise it logs are empty, they won't be replaced in text panel
        this.text.hide();
        this.refreshButton.show();
        this.liveCheck.show();
        if (!this.liveCheck.getValueAsBoolean()) {
            this.taskSelect.show();
            this.outSelect.show();
            this.refreshButton.show();
        }
        this.label.setContents("No output available<br><br>"
            + "Click <strong>Fetch output</strong> to retrieve logs for finished tasks<br>"
            + "Use <strong>Streaming</strong> to auto-fetch logs for running tasks.");
        this.label.setIcon(null);
        this.label.show();
    }

    private void updateLive(String out) {
        this.text.setContents(out);
        this.liveCheck.show();
        this.taskSelect.hide();
        this.outSelect.hide();
        this.refreshButton.hide();
        this.label.hide();
        this.text.show();
    }

    private void update(JobOutput out) {
        if (this.isLive)
            return;

        String content = " "; // whitespace otherwise it logs are empty, they won't be replaced in text panel
        if (this.taskSelect.getValueAsString().equals(TASKS_ALL)) {
            // alternate bgcolors for each entry
            boolean even = false;
            for (Entry<Task, List<String>> taskOutputLines : out.getLines().entrySet()) {
                String style = "";
                if (even) {
                    style = "background-color:#FAFAFA; border-bottom: 1px solid #EDEDED; border-top: 1px solid #EDEDED;";
                }
                boolean hasContent = false;
                String tmp = "<div style = '" + style + "'>";
                for (String taskOutputLine : taskOutputLines.getValue()) {
                    hasContent = true;
                    tmp += "<nobr>" + taskOutputLine + "</nobr>";
                }
                if (hasContent) {
                    tmp += "</div>";
                    even = !even;
                    content += tmp;
                }
            }
        } else {
            String taskName = (String) this.taskSelect.getValue();
            Task task = null;
            for (Task t : this.controller.getModel().getTasks()) {
                if (taskName.equals(t.getName())) {
                    task = t;
                    break;
                }
            }

            List<String> lines = out.getLines().get(task);
            if (lines == null || lines.isEmpty()) {
                clear();
                return;
            }
            for (String str : lines) {
                content += str;
            }
        }

        this.text.setContents(content);
        this.taskSelect.show();
        this.outSelect.show();
        this.refreshButton.show();
        this.label.hide();
        this.text.show();
    }

    public void tasksUpdating(boolean jobChanged) {
    }

    public void tasksUpdated(List<Task> tasks) {
        if (tasks.size() + 1 == this.taskSelect.getClientPickListData().length) {
            return;
        }

        if (this.isLive) {

        } else {
            String[] values = new String[tasks.size() + 1];
            values[0] = TASKS_ALL;
            int i = 1;

            for (Task t : tasks) {
                switch (t.getStatus()) {
                    /*
                    	case SKIPPED:
                    	case PENDING:
                    	case SUBMITTED:
                    	case NOT_STARTED:
                    		break;
                     */
                    default:
                        values[i] = t.getName();
                        i++;
                        break;
                }
            }
            this.taskSelect.setValueMap(values);
            this.taskSelect.setValue(TASKS_ALL);
            this.refreshButton.setDisabled(false);
        }
    }

    public void tasksUpdatedFailure(String message) {
    }
}
