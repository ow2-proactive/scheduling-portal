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
package org.ow2.proactive_grid_cloud_portal.scheduler.client;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.StatisticsListener;

import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.viewer.DetailViewer;
import com.smartgwt.client.widgets.viewer.DetailViewerField;
import com.smartgwt.client.widgets.viewer.DetailViewerRecord;


/**
 * Displays stats about the current logged account, and the scheduler
 * 
 * 
 * @author mschnoor
 *
 */
public class StatisticsView implements StatisticsListener {

    private DetailViewer statsDetail, accountDetail, taskDetails;

    private Label statsLabel, accountLabel, placeHolder;

    /**
     * Default constructor
     */
    public StatisticsView(SchedulerController controller) {
        controller.getEventDispatcher().addStatisticsListener(this);
    }

    /**
     * Build and return this view's graphical components
     */
    public Layout build() {
        Layout root = new HLayout();
        root.setWidth100();
        root.setHeight100();

        Layout leftPart = new VLayout();

        statsDetail = new DetailViewer();
        statsDetail.setCanSelectText(true);

        DetailViewerField jobSubmittingPeriod = new DetailViewerField("JobSubmittingPeriod", "Job Submitting Period");
        DetailViewerField formattedJobSubmittingPeriod = new DetailViewerField("FormattedJobSubmittingPeriod",
                                                                               "Formatted Job Submitting Period");

        DetailViewerField meanJobPendingTime = new DetailViewerField("MeanJobPendingTime", "Mean Job Pending Time");
        DetailViewerField formattedMeanJobPendingTime = new DetailViewerField("FormattedMeanJobPendingTime",
                                                                              "Formatted Mean Job Pending Time");

        DetailViewerField meanJobExecutionTime = new DetailViewerField("MeanJobExecutionTime",
                                                                       "Mean Job Execution Time");
        DetailViewerField formattedMeanJobExecutionTime = new DetailViewerField("FormattedMeanJobExecutionTime",
                                                                                "Formatted Mean Job Execution Time");

        DetailViewerField connectedUsersCount = new DetailViewerField("ConnectedUsersCount", "Users Sessions Count");

        DetailViewerField pendingJobsCount = new DetailViewerField("PendingJobsCount", "Pending Jobs Count");
        DetailViewerField runningJobsCount = new DetailViewerField("RunningJobsCount", "Running Jobs Count");
        DetailViewerField stalledJobsCount = new DetailViewerField("StalledJobsCount", "Stalled Jobs Count");
        DetailViewerField pausedJobsCount = new DetailViewerField("PausedJobsCount", "Paused Jobs Count");
        DetailViewerField finishedJobsCount = new DetailViewerField("FinishedJobsCount", "Finished Jobs Count");
        DetailViewerField inErrorJobsCount = new DetailViewerField("InErrorJobsCount", "In-Error Jobs Count");
        DetailViewerField killedJobsCount = new DetailViewerField("KilledJobsCount", "Killed Jobs Count");
        DetailViewerField cancelledJobsCount = new DetailViewerField("CancelledJobsCount", "Cancelled Jobs Count");
        DetailViewerField failedJobsCount = new DetailViewerField("FailedJobsCount", "Failed Jobs Count");
        DetailViewerField totalJobsCount = new DetailViewerField("TotalJobsCount", "Total Jobs Count");

        statsDetail.setFields(connectedUsersCount,
                              jobSubmittingPeriod,
                              formattedJobSubmittingPeriod,
                              meanJobPendingTime,
                              formattedMeanJobPendingTime,
                              meanJobExecutionTime,
                              formattedMeanJobExecutionTime,
                              pendingJobsCount,
                              runningJobsCount,
                              stalledJobsCount,
                              pausedJobsCount,
                              finishedJobsCount,
                              inErrorJobsCount,
                              killedJobsCount,
                              cancelledJobsCount,
                              failedJobsCount,
                              totalJobsCount);

        accountDetail = new DetailViewer();
        accountDetail.setCanSelectText(true);

        DetailViewerField totalJobCount = new DetailViewerField("TotalJobCount", "Total Jobs Count");
        DetailViewerField totalJobDuration = new DetailViewerField("TotalJobDuration", "Total Jobs Duration");
        DetailViewerField formattedTotalJobDuration = new DetailViewerField("FormattedTotalJobDuration",
                                                                            "Formatted Total Jobs Duration");

        DetailViewerField totalTaskCount = new DetailViewerField("TotalTaskCount", "Total Tasks Count");
        DetailViewerField totalTaskDuration = new DetailViewerField("TotalTaskDuration", "Total Tasks Duration");
        DetailViewerField formattedTotalTaskDuration = new DetailViewerField("FormattedTotalTaskDuration",
                                                                             "Formatted Total Tasks Duration");

        accountDetail.setFields(totalJobCount,
                                totalJobDuration,
                                formattedTotalJobDuration,
                                totalTaskCount,
                                totalTaskDuration,
                                formattedTotalTaskDuration);

        statsLabel = new Label("<h3>Scheduler statistics</h3>");
        statsLabel.setHeight(25);
        statsLabel.hide();

        accountLabel = new Label("<h3>My account</h3>");
        accountLabel.setHeight(25);
        accountLabel.hide();

        leftPart.addMember(statsLabel);
        leftPart.addMember(statsDetail);
        leftPart.addMember(accountLabel);
        leftPart.addMember(accountDetail);

        root.addMember(leftPart);

        Layout rightPart = new VLayout();

        root.addMember(rightPart);

        placeHolder = new Label("");
        placeHolder.setHeight(25);
        placeHolder.hide();
        rightPart.addMember(placeHolder);

        taskDetails = new DetailViewer();
        taskDetails.setCanSelectText(true);

        DetailViewerField submittedTasksCount = new DetailViewerField("SubmittedTasksCount", "Submitted Tasks Count");
        DetailViewerField pendingTasksCount = new DetailViewerField("PendingTasksCount", "Pending Tasks Count");
        DetailViewerField pausedTasksCount = new DetailViewerField("PausedTasksCount", "Paused Tasks Count");
        DetailViewerField runningTasksCount = new DetailViewerField("RunningTasksCount", "Running Tasks Count");
        DetailViewerField finishedTasksCount = new DetailViewerField("FinishedTasksCount", "Finished Tasks Count");
        DetailViewerField waitingOnErrorTasksCount = new DetailViewerField("WaitingOnErrorTasksCount",
                                                                           "Faulty... Tasks Count");
        DetailViewerField waitingOnFailureTasksCount = new DetailViewerField("WaitingOnFailureTasksCount",
                                                                             "Failed... Tasks Count");
        DetailViewerField failedTasksCount = new DetailViewerField("FailedTasksCount", "Resource down Tasks Count");
        DetailViewerField notStartedTasksCount = new DetailViewerField("NotStartedTasksCount",
                                                                       "Could not start Tasks Count");
        DetailViewerField notRestartedTasksCount = new DetailViewerField("NotRestartedTasksCount",
                                                                         "Could not restart Tasks Count");
        DetailViewerField abortedTasksCount = new DetailViewerField("AbortedTasksCount", "Aborted Tasks Count");
        DetailViewerField faultyTasksCount = new DetailViewerField("FaultyTasksCount", "Faulty Tasks Count");
        DetailViewerField skippedTasksCount = new DetailViewerField("SkippedTasksCount", "Skipped Tasks Count");
        DetailViewerField inErrorTasksCount = new DetailViewerField("InErrorTasksCount", "In-Error Tasks Count");
        DetailViewerField totalTasksCount = new DetailViewerField("TotalTasksCount", "Total Tasks Count");

        taskDetails.setFields(submittedTasksCount,
                              pendingTasksCount,
                              pausedTasksCount,
                              runningTasksCount,
                              finishedTasksCount,
                              waitingOnErrorTasksCount,
                              waitingOnFailureTasksCount,
                              failedTasksCount,
                              notStartedTasksCount,
                              notRestartedTasksCount,
                              abortedTasksCount,
                              faultyTasksCount,
                              skippedTasksCount,
                              inErrorTasksCount,
                              totalTasksCount);

        rightPart.addMember(taskDetails);

        return root;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.ow2.proactive_grid_cloud_portal.client.Listeners.StatisticsListener#schedulerStatsUpdated
     * (org.ow2.proactive_grid_cloud_portal.shared.SchedulerStatistics)
     */
    public void schedulerStatsUpdated(HashMap<String, String> stats) {
        DetailViewerRecord r = new DetailViewerRecord();
        List<String> aList = Arrays.asList("JobSubmittingPeriod",
                                           "FormattedJobSubmittingPeriod",
                                           "MeanJobPendingTime",
                                           "ConnectedUsersCount",
                                           "FinishedTasksCount",
                                           "RunningJobsCount",
                                           "RunningTasksCount",
                                           "FormattedMeanJobPendingTime",
                                           "MeanJobExecutionTime",
                                           "PendingTasksCount",
                                           "FinishedJobsCount",
                                           "TotalTasksCount",
                                           "FormattedMeanJobExecutionTime",
                                           "TotalJobsCount",
                                           "PendingJobsCount",
                                           "StalledJobsCount",
                                           "PausedJobsCount",
                                           "InErrorJobsCount",
                                           "KilledJobsCount",
                                           "CancelledJobsCount",
                                           "FailedJobsCount");

        for (String propName : aList) {
            r.setAttribute(propName, stats.get(propName));
        }

        statsDetail.setData(new DetailViewerRecord[] { r });
        statsLabel.show();

        r = new DetailViewerRecord();
        aList = Arrays.asList("SubmittedTasksCount",
                              "PendingTasksCount",
                              "PausedTasksCount",
                              "RunningTasksCount",
                              "FinishedTasksCount",
                              "WaitingOnErrorTasksCount",
                              "WaitingOnFailureTasksCount",
                              "FailedTasksCount",
                              "NotStartedTasksCount",
                              "NotRestartedTasksCount",
                              "AbortedTasksCount",
                              "FaultyTasksCount",
                              "SkippedTasksCount",
                              "InErrorTasksCount",
                              "TotalTasksCount");

        for (String propName : aList) {
            r.setAttribute(propName, stats.get(propName));
        }

        taskDetails.setData(new DetailViewerRecord[] { r });
        placeHolder.show();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.ow2.proactive_grid_cloud_portal.client.Listeners.StatisticsListener#accountStatsUpdated(
     * org.ow2.proactive_grid_cloud_portal.shared.AccountStatistics)
     */
    public void accountStatsUpdated(HashMap<String, String> stats) {
        DetailViewerRecord r = new DetailViewerRecord();
        r.setAttribute("TotalTaskCount", stats.get("TotalTaskCount"));
        r.setAttribute("TotalJobDuration", stats.get("TotalJobDuration"));
        r.setAttribute("FormattedTotalJobDuration", Job.formatDuration(stats.get("TotalJobDuration")));
        r.setAttribute("TotalJobCount", stats.get("TotalJobCount"));
        r.setAttribute("TotalTaskDuration", stats.get("TotalTaskDuration"));
        r.setAttribute("FormattedTotalTaskDuration", Job.formatDuration(stats.get("TotalTaskDuration")));
        accountDetail.setData(new DetailViewerRecord[] { r });
        accountLabel.show();
    }

}
