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

import java.util.HashMap;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.StatisticsListener;

import com.smartgwt.client.widgets.Label;
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

    private Layout root = null;

    private DetailViewer statsDetail, accountDetail;

    private Label l1, l2;

    /**
     * Default constructor
     * 
     * @param controller
     */
    public StatisticsView(SchedulerController controller) {
        controller.getEventDispatcher().addStatisticsListener(this);
    }

    /**
     * Build and return this view's graphical components
     * 
     * @return
     */
    public Layout build() {
        this.root = new VLayout();
        this.root.setWidth100();
        this.root.setHeight100();

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

        DetailViewerField pendingTasksCount = new DetailViewerField("PendingTasksCount", "Pending Tasks Count");
        DetailViewerField runningTasksCount = new DetailViewerField("RunningTasksCount", "Running Tasks Count");
        DetailViewerField finishedTasksCount = new DetailViewerField("FinishedTasksCount", "Finished Tasks Count");
        DetailViewerField totalTasksCount = new DetailViewerField("TotalTasksCount", "Total Tasks Count");

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
                              totalJobsCount,
                              pendingTasksCount,
                              runningTasksCount,
                              finishedTasksCount,
                              totalTasksCount);

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

        l1 = new Label("<h3>Scheduler statistics</h3>");
        l1.setHeight(25);
        l1.hide();

        l2 = new Label("<h3>My account</h3>");
        l2.setHeight(25);
        l2.hide();

        this.root.addMember(l1);
        this.root.addMember(statsDetail);
        this.root.addMember(l2);
        this.root.addMember(accountDetail);

        return this.root;
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
        r.setAttribute("JobSubmittingPeriod", stats.get("JobSubmittingPeriod"));
        r.setAttribute("FormattedJobSubmittingPeriod", stats.get("FormattedJobSubmittingPeriod"));
        r.setAttribute("MeanJobPendingTime", stats.get("MeanJobPendingTime"));
        r.setAttribute("ConnectedUsersCount", stats.get("ConnectedUsersCount"));
        r.setAttribute("FinishedTasksCount", stats.get("FinishedTasksCount"));
        r.setAttribute("RunningJobsCount", stats.get("RunningJobsCount"));
        r.setAttribute("RunningTasksCount", stats.get("RunningTasksCount"));
        r.setAttribute("FormattedMeanJobPendingTime", stats.get("FormattedMeanJobPendingTime"));
        r.setAttribute("MeanJobExecutionTime", stats.get("MeanJobExecutionTime"));
        r.setAttribute("PendingTasksCount", stats.get("PendingTasksCount"));
        r.setAttribute("FinishedJobsCount", stats.get("FinishedJobsCount"));
        r.setAttribute("TotalTasksCount", stats.get("TotalTasksCount"));
        r.setAttribute("FormattedMeanJobExecutionTime", stats.get("FormattedMeanJobExecutionTime"));
        r.setAttribute("TotalJobsCount", stats.get("TotalJobsCount"));
        r.setAttribute("PendingJobsCount", stats.get("PendingJobsCount"));

        r.setAttribute("StalledJobsCount", stats.get("StalledJobsCount"));
        r.setAttribute("PausedJobsCount", stats.get("PausedJobsCount"));
        r.setAttribute("InErrorJobsCount", stats.get("InErrorJobsCount"));
        r.setAttribute("KilledJobsCount", stats.get("KilledJobsCount"));
        r.setAttribute("CancelledJobsCount", stats.get("CancelledJobsCount"));
        r.setAttribute("FailedJobsCount", stats.get("FailedJobsCount"));

        this.statsDetail.setData(new DetailViewerRecord[] { r });
        l1.show();
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
        this.accountDetail.setData(new DetailViewerRecord[] { r });
        l2.show();
    }

}
