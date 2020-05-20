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

import java.util.Collection;
import java.util.List;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.Job;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.JobOutput;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.OutputMode;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.JobOutputListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SelectionTarget;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.OutputController;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.OutputModel;

import com.smartgwt.client.widgets.Canvas;
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
public class OutputView extends AbstractOutputDisplayView<OutputModel, OutputController> implements JobOutputListener {

    /** whether fetch live logs */
    private CheckboxItem liveCheck = null;

    /** stdout, stderr or both */
    private SelectItem outSelect = null;

    public OutputView(OutputController controller) {
        super(controller);

        this.controller.getModel().addJobOutputListener(this);

        this.noOutputMessage = "No output available<br><br><br>" +
                               "Click <strong>Finished Tasks Output</strong> to retrieve logs for already finished tasks within the Job, or for the selected Task. The command does work when Job is still in Running state, as well as when Job is Finished. In case of large number of Tasks (over 20), the command displays only the logs of the current page of Tasks.<br><br>" +
                               "Use <strong>Streaming Output</strong> to auto-fetch logs for running tasks of the entire Job. You cannot select a specific Task in the streaming mode. If you activate streaming while some Tasks are already finished, you will get the logs of those Tasks as well.";

        this.refreshButtonLabel = "Finished Tasks Output";
        this.refreshButtonTooltip = "Request fetching the logs for finished tasks of the entire Job, or for a selected Task";

        this.notAuthorized = "You are not authorized to retrieve this job's output";

    }

    /**
     * build the output
     * @return the Widget to display, ready to be added in a container
     */
    public Layout build() {
        /* contains the layout */
        Layout root = new VLayout();
        root.setWidth100();
        root.setHeight100();

        this.buildRefreshButton();

        this.liveCheck = new CheckboxItem("liveLogs", "Streaming Output &nbsp;&nbsp;");
        this.liveCheck.setHeight(22);
        this.liveCheck.setTooltip("Request fetching logs for running tasks of the entire Job");
        this.liveCheck.addChangedHandler(new ChangedHandler() {
            public void onChanged(ChangedEvent event) {
                liveLogCheckChanged();
            }
        });

        this.buildTargetSelect();

        this.outSelect = new SelectItem();
        this.outSelect.setShowTitle(false);
        this.outSelect.setValueMap(OutputMode.toStringArray());
        this.outSelect.setValue(OutputMode.LOG_OUT_ERR.label);
        this.outSelect.addChangedHandler(new ChangedHandler() {
            @Override
            public void onChanged(ChangedEvent event) {
                outModeChangedHandler();
            }
        });

        DynamicForm form = new DynamicForm();
        form.setColWidths("10", "*", "*", "*");
        form.setNumCols(4);
        form.setFields(liveCheck, targetSelect, outSelect);

        HLayout buttons = new HLayout();
        buttons.setWidth100();
        buttons.setHeight(22);
        buttons.setMembersMargin(5);

        Canvas fill = new Canvas();
        fill.setWidth100();

        buttons.setMembers(form, fill, refreshButton);

        VLayout textLayout = this.buildOutputPane();

        root.addMember(buttons);
        root.addMember(textLayout);

        this.goToNoTargetState();

        return root;
    }

    /**
     * The view when a user is not authorized to fetch the job's output
     */
    public void goToNotAuthorized() {
        this.text.setContents(" "); // whitespace otherwise it logs are empty, they won't be replaced in text panel
        this.text.hide();

        this.label.setContents(this.notAuthorized);
        this.label.setIcon(null);
        this.label.show();
    }

    public void refreshOutputModeSelect(OutputMode mode) {
        this.outSelect.setValue(mode.label);
        this.outSelect.redraw();
    }

    /**
     * Updates the output view to display the given output after update.
     */
    public void jobOutputUpdated(JobOutput output, SelectionTarget target) {
        if (output == null) {
            this.goToNoTargetState();
        } else {
            Collection<List<String>> lines = this.controller.getLinesToDisplay(output);
            if (output.isLive() && target == SelectionTarget.JOB_TARGET) {
                if (lines.isEmpty()) {
                    this.goToLoadingState();
                } else {
                    this.update(lines);
                }
            } else {
                this.goToTargetSelectedState(output);
                if (lines.isEmpty()) {
                    this.goToUnavailableOutputState();
                } else {
                    this.update(lines);
                }
            }
        }
    }

    /**
     * Display the output lines of the current selected job or task
     * @param lines the lines to be displayed.
     */
    protected void update(Collection<List<String>> lines) {
        StringBuilder builder = new StringBuilder();

        for (List<String> outputLines : lines) {
            if (!outputLines.isEmpty()) {
                builder.append("<div>");
                for (String outputLine : outputLines) {
                    builder.append("<nobr>");
                    builder.append(outputLine);
                    builder.append("</nobr>");
                }
                builder.append("</div>");
            }
        }

        this.showContent(builder.toString());
    }

    /**
     * show fetch button, target selection dropdown list and output mode dropdown list.
     */
    protected void showRefreshControls() {
        this.targetSelect.show();
        this.outSelect.show();
        this.refreshButton.show();
    }

    /**
     * Called when the user check or uncheck the livelog option.
     */
    protected void liveLogCheckChanged() {
        this.controller.toggleLive(liveCheck.getValueAsBoolean());
    }

    /**
     * Update the view to show no job or task selected. 
     */
    protected void goToNoTargetState() {
        super.goToNoTargetState();
        this.outSelect.disable();
        this.liveCheck.setValue(false);
        this.liveCheck.disable();
    }

    /**
     * Update view to show the control when a job or task has been selected.
     * @param output
     */
    protected void goToTargetSelectedState(JobOutput output) {
        super.goToTargetSelectedState();

        this.outSelect.enable();
        this.outSelect.setValue(output.getOutputMode().label);
    }

    /**
     * Called when the current selected job has been updated.
     */
    @Override
    public void selectedJobUpdated(Job job) {
        this.controller.checkLiveEnabled(job);
    }

    /**
     * Called when the output mode has been changed by the user interaction.
     */
    protected void outModeChangedHandler() {
        String outMode = this.outSelect.getValueAsString();
        if (outMode.equals(OutputMode.LOG_OUT_ERR.label)) {
            this.controller.changeOutputMode(OutputMode.LOG_OUT_ERR);
            return;
        }
        if (outMode.equals(OutputMode.LOG_OUT.label)) {
            this.controller.changeOutputMode(OutputMode.LOG_OUT);
            return;
        }
        if (outMode.equals(OutputMode.LOG_ERR.label)) {
            this.controller.changeOutputMode(OutputMode.LOG_ERR);
            return;
        }
        if (outMode.equals(OutputMode.LOG_FULL.label)) {
            this.controller.changeOutputMode(OutputMode.LOG_FULL);
            return;
        }
    }

    /**
     * Called when the live option has been toggled.
     */
    @Override
    public void liveToggled(boolean newValue) {
        if (newValue) {
            this.outSelect.hide();
            this.refreshButton.hide();
        } else {
            this.outSelect.show();
            this.refreshButton.show();
        }
        this.liveCheck.setValue(newValue);
    }

    /**
     * Called when the live option has been enabled or disabled.
     */
    @Override
    public void liveEnabled(boolean newValue) {
        this.liveCheck.setDisabled(!newValue);
        if (!newValue) {
            liveToggled(false);
        }
    }

}
