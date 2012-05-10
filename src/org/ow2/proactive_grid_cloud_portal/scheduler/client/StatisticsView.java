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
        DetailViewerField sd1 = new DetailViewerField("JobSubmittingPeriod", "Job Submitting Period");
        DetailViewerField sd2 = new DetailViewerField("FormattedJobSubmittingPeriod",
            "Formatted Job Submitting Period");
        DetailViewerField sd3 = new DetailViewerField("MeanJobPendingTime", "Mean Job Pending Time");
        DetailViewerField sd4 = new DetailViewerField("ConnectedUsersCount", "Connected Users Count");
        DetailViewerField sd5 = new DetailViewerField("FinishedTasksCount", "Finished Tasks Count");
        DetailViewerField sd6 = new DetailViewerField("RunningJobsCount", "Running Jobs Count");
        DetailViewerField sd7 = new DetailViewerField("RunningTasksCount", "Running Tasks Count");
        DetailViewerField sd8 = new DetailViewerField("FormattedMeanJobPendingTime",
            "Formatted Mean Job Pending Time");
        DetailViewerField sd9 = new DetailViewerField("MeanJobExecutionTime", "Mean Job Execution Time");
        DetailViewerField sd10 = new DetailViewerField("PendingTasksCount", "Pending Tasks Count");
        DetailViewerField sd11 = new DetailViewerField("FinishedJobsCount", "Finished Jobs Count");
        DetailViewerField sd12 = new DetailViewerField("TotalTasksCount", "Total Tasks Count");
        DetailViewerField sd13 = new DetailViewerField("FormattedMeanJobExecutionTime",
            "Formatted Mean Job Execution Time");
        DetailViewerField sd14 = new DetailViewerField("TotalJobsCount", "Total Jobs Count");
        DetailViewerField sd15 = new DetailViewerField("PendingJobsCount", "Pending Jobs Count");
        statsDetail
                .setFields(sd1, sd2, sd3, sd4, sd5, sd6, sd7, sd8, sd9, sd10, sd11, sd12, sd13, sd14, sd15);

        accountDetail = new DetailViewer();
        accountDetail.setCanSelectText(true);
        DetailViewerField ad1 = new DetailViewerField("TotalTaskCount", "Total Task Count");
        DetailViewerField ad2 = new DetailViewerField("TotalJobDuration", "Total Job Duration");
        DetailViewerField ad3 = new DetailViewerField("TotalJobCount", "Total Job Count");
        DetailViewerField ad4 = new DetailViewerField("TotalTaskDuration", "Total Task Duration");
        accountDetail.setFields(ad1, ad2, ad3, ad4);

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
     * @see org.ow2.proactive_grid_cloud_portal.client.Listeners.StatisticsListener#schedulerStatsUpdated(org.ow2.proactive_grid_cloud_portal.shared.SchedulerStatistics)
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

        this.statsDetail.setData(new DetailViewerRecord[] { r });
        l1.show();
    }

    /*
     * (non-Javadoc)
     * @see org.ow2.proactive_grid_cloud_portal.client.Listeners.StatisticsListener#accountStatsUpdated(org.ow2.proactive_grid_cloud_portal.shared.AccountStatistics)
     */
    public void accountStatsUpdated(HashMap<String, String> stats) {
        DetailViewerRecord r = new DetailViewerRecord();
        r.setAttribute("TotalTaskCount", stats.get("TotalTaskCount"));
        r.setAttribute("TotalJobDuration", stats.get("TotalJobDuration"));
        r.setAttribute("TotalJobCount", stats.get("TotalJobCount"));
        r.setAttribute("TotalTaskDuration", stats.get("TotalTaskDuration"));
        this.accountDetail.setData(new DetailViewerRecord[] { r });
        l2.show();
    }

}
