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

import org.ow2.proactive_grid_cloud_portal.common.client.JSUtil;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.JobSelectedListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.JobsUpdatedListener;

import java.util.Map;

import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.viewer.DetailViewer;
import com.smartgwt.client.widgets.viewer.DetailViewerField;
import com.smartgwt.client.widgets.viewer.DetailViewerRecord;


/**
 * Displays detailed info about the currently selected job
 *
 *
 * @author mschnoor
 *
 */
public class JobInfoView implements JobSelectedListener, JobsUpdatedListener {

    private static final String ID_ATTR = "id";
    private static final String STATE_ATTR = "state";
    private static final String NAME_ATTR = "name";
    private static final String PRIORITY_ATTR = "priority";
    private static final String USER_ATTR = "user";
    private static final String PENDING_TASKS_ATTR = "pendingTasks";
    private static final String RUNNING_TASKS_ATTR = "runningTasks";
    private static final String FINISHED_TASKS_ATTR = "finishedTasks";
    private static final String TOTAL_TASKS_ATTR = "totalTasks";
    private static final String SUBMITTED_TIME_ATTR = "submittedTime";
    private static final String STARTED_TIME_ATTR = "startedTime";
    private static final String FINISHED_TIME_ATTR = "finishedTime";
    private static final String PENDING_DURATION_ATTR = "pendingDuration";
    private static final String EXEC_DURATION_ATTR = "execDuration";
    private static final String TOTAL_DURATION_ATTR = "totalDuration";

    /** label when no job is selected */
    private Label label = null;
    /** widget shown when a job is selected */
    private DetailViewer details = null;
    /** currently displayed job */
    private Job displayedJob = null;

    /**
     * @param controller the Controller that created this View
     */
    public JobInfoView(SchedulerController controller) {
        controller.getEventDispatcher().addJobSelectedListener(this);
        controller.getEventDispatcher().addJobsUpdatedListener(this);
    }

    /**
     * @return the Widget to display, ready to be added in a container
     */
    public Layout build() {
        /* widget that has been returned as the root layout */
        Layout root = new Layout();
        root.setWidth100();
        root.setHeight100();

        this.label = new Label("No job selected.");
        this.label.setWidth100();
        this.label.setAlign(Alignment.CENTER);

        this.details = new DetailViewer();
        this.details.setWidth100();
        this.details.setHeight100();
        this.details.setCanSelectText(true);
        this.details.hide();

        DetailViewerField df1 = new DetailViewerField(ID_ATTR, "Job Id");
        DetailViewerField df2 = new DetailViewerField(STATE_ATTR, "State");
        DetailViewerField df3 = new DetailViewerField(NAME_ATTR, "Name");
        DetailViewerField df4 = new DetailViewerField(PRIORITY_ATTR, "Priority");
        DetailViewerField df9 = new DetailViewerField(USER_ATTR, "User");
        DetailViewerField df5 = new DetailViewerField(PENDING_TASKS_ATTR, "Pending tasks");
        DetailViewerField df6 = new DetailViewerField(RUNNING_TASKS_ATTR, "Running tasks");
        DetailViewerField df7 = new DetailViewerField(FINISHED_TASKS_ATTR, "Finished tasks");
        DetailViewerField df8 = new DetailViewerField(TOTAL_TASKS_ATTR, "Total tasks");
        DetailViewerField df10 = new DetailViewerField(SUBMITTED_TIME_ATTR, "Submitted time");
        DetailViewerField df11 = new DetailViewerField(FINISHED_TIME_ATTR, "Finished time");
        DetailViewerField df12 = new DetailViewerField(PENDING_DURATION_ATTR, "Pending duration");
        DetailViewerField df13 = new DetailViewerField(EXEC_DURATION_ATTR, "Execution duration");
        DetailViewerField df14 = new DetailViewerField(TOTAL_DURATION_ATTR, "Total duration");

        this.details.setFields(df1, df2, df3, df4, df9, df5, df6, df7, df8, df10, df11, df12, df13, df14);

        root.addMember(label);
        root.addMember(details);

        return root;
    }

    public void jobSelected(Job job) {
        long submitTime = job.getSubmitTime();
        long startTime = job.getStartTime();
        long finishTime = job.getFinishTime();

        String pendingDuration = "";
        if (startTime > submitTime)
            pendingDuration = Job.formatDuration(startTime - submitTime);
        String execDuration = "";
        String totalDuration = "";
        if (finishTime > startTime) {
            if (startTime > 0)
                execDuration = Job.formatDuration(finishTime - startTime);
            totalDuration = Job.formatDuration(finishTime - submitTime);
        }

        /* currently displayed details */
        DetailViewerRecord curDetails = new DetailViewerRecord();
        curDetails.setAttribute(ID_ATTR, job.getId());
        curDetails.setAttribute(STATE_ATTR, job.getStatus().toString());
        curDetails.setAttribute(NAME_ATTR, job.getName());
        curDetails.setAttribute(PRIORITY_ATTR, job.getPriority().toString());
        curDetails.setAttribute(USER_ATTR, job.getUser());
        curDetails.setAttribute(PENDING_TASKS_ATTR, job.getPendingTasks());
        curDetails.setAttribute(RUNNING_TASKS_ATTR, job.getRunningTasks());
        curDetails.setAttribute(FINISHED_TASKS_ATTR, job.getFinishedTasks());
        curDetails.setAttribute(TOTAL_TASKS_ATTR, job.getTotalTasks());
        curDetails.setAttribute(SUBMITTED_TIME_ATTR, JSUtil.getTime(submitTime));
        curDetails.setAttribute(STARTED_TIME_ATTR, (startTime > submitTime) ? JSUtil.getTime(startTime) : "");
        curDetails.setAttribute(FINISHED_TIME_ATTR, (finishTime > startTime) ? JSUtil.getTime(finishTime)
                : "");
        curDetails.setAttribute(PENDING_DURATION_ATTR, pendingDuration);
        curDetails.setAttribute(EXEC_DURATION_ATTR, execDuration);
        curDetails.setAttribute(TOTAL_DURATION_ATTR, totalDuration);

        this.details.setData(new DetailViewerRecord[]{curDetails});

        this.label.hide();
        this.details.show();
        this.displayedJob = job;
    }

    public void jobsUpdating() {
    }

    public void jobSubmitted(Job j) {
    }

    public void jobsUpdated(Map<Integer, Job> jobs) {
        if (this.displayedJob == null)
            return;

        for (Job j : jobs.values()) {
            if (j.getId().equals(this.displayedJob.getId())) {
                jobSelected(j);
            }
        }
    }

    public void jobUnselected() {
        this.details.hide();
        this.label.show();
        this.displayedJob = null;
    }

}
