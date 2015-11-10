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

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.ow2.proactive_grid_cloud_portal.common.client.Listeners.StatsListener;
import org.ow2.proactive_grid_cloud_portal.common.client.Model.StatHistory.Range;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.SchedulerStatusListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.StatisticsListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.UsersListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.VisualizationListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.ExecutionsModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.OutputModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.TasksModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.shared.JobVisuMap;

import com.google.gwt.i18n.client.DateTimeFormat;
import com.google.gwt.i18n.client.DateTimeFormat.PredefinedFormat;


/**
 * Writable Model, should only be used by the Controller
 *
 *
 * @author mschnoor
 *
 */
public class SchedulerModelImpl extends SchedulerModel implements SchedulerEventDispatcher {

    
    private SchedulerStatus schedulerStatus = SchedulerStatus.STARTED;
    
    private List<SchedulerUser> users = null;
    private List<SchedulerUser> usersWithJobs = null;
    private HashMap<String, String> schedulerStats = null;
    private HashMap<String, String> accountStats = null;
    private Map<String, String> imagePath = null;
    private Map<String, JobVisuMap> visuMap = null;
    private Map<String, String> htmlMap = null;
    private Map<String, StatHistory> statistics = null;
    private Map<String, Range> requestedStatRange = null;
    private List<JobUsage> usage = null;

    private ArrayList<SchedulerStatusListener> schedulerStateListeners = null;
    
    private ArrayList<UsersListener> usersListeners = null;
    private ArrayList<UsersListener> usersWithJobsListeners = null;
    private ArrayList<StatisticsListener> statisticsListeners = null;
    private ArrayList<VisualizationListener> visuListeners = null;
    private ArrayList<StatsListener> statsListeners = null;
    private ArrayList<SchedulerListeners.UsageListener> usageListeners = null;
    private SchedulerListeners.ThirdPartyCredentialsListener thirdPartyCredentialsListener;


    private ExecutionsModel executionsModel;

    private TasksModel tasksModel;
    
    private OutputModel outputModel;

    SchedulerModelImpl() {
        super();
        this.schedulerStateListeners = new ArrayList<SchedulerStatusListener>();
        this.usersListeners = new ArrayList<UsersListener>();
        this.usersWithJobsListeners = new ArrayList<UsersListener>();
        this.statisticsListeners = new ArrayList<StatisticsListener>();
        this.visuListeners = new ArrayList<VisualizationListener>();
        this.statsListeners = new ArrayList<StatsListener>();
        this.usageListeners = new ArrayList<SchedulerListeners.UsageListener>();
        this.imagePath = new HashMap<String, String>();
        this.visuMap = new HashMap<String, JobVisuMap>();
        this.htmlMap = new HashMap<String, String>();
        this.requestedStatRange = new HashMap<String, Range>();
    }


    @Override
    public SchedulerStatus getSchedulerStatus() {
        return this.schedulerStatus;
    }

    /**
     * Set a new scheduler status,
     * notify listeners
     * 
     * @param status the new scheduler status
     */
    void setSchedulerStatus(SchedulerStatus status) {
        this.schedulerStatus = status;

        for (SchedulerStatusListener listener : this.schedulerStateListeners) {
            listener.statusChanged(this.schedulerStatus);
        }
    }


    @Override
    public String getJobImagePath(String jobId) {
        return this.imagePath.get(jobId);
    }

    void setJobImagePath(String jobId, String path) {
        this.imagePath.put(jobId, path);

        for (VisualizationListener list : this.visuListeners) {
            list.imageUpdated(jobId, path);
        }
    }

    void visuUnavailable(String jobId) {
        for (VisualizationListener list : visuListeners) {
            list.visualizationUnavailable(jobId);
        }
    }

    @Override
    public String getJobHtml(String jobId) {
        return this.htmlMap.get(jobId);
    }

    @Override
    public void setJobHtml(String jobId, String curHtml) {
        this.htmlMap.put(jobId, curHtml);

        for (VisualizationListener list : this.visuListeners) {
            list.htmlUpdated(jobId, curHtml);
        }
    }

    @Override
    public JobVisuMap getJobVisuMap(String jobId) {
        return this.visuMap.get(jobId);
    }

    void setJobVisuMap(String jobId, JobVisuMap map) {
        this.visuMap.put(jobId, map);

        for (VisualizationListener list : this.visuListeners) {
            list.mapUpdated(jobId, map);
        }
    }

    @Override
    public List<SchedulerUser> getSchedulerUsers() {
        return this.users;
    }

    /**
     * Change the local users list, notify listeners
     * 
     * @param users new users
     */
    void setSchedulerUsers(List<SchedulerUser> users) {
        this.users = users;

        for (UsersListener list : this.usersListeners) {
            list.usersUpdated(this.users);
        }
    }

    @Override
    public List<SchedulerUser> getSchedulerUsersWithJobs() {
        return this.usersWithJobs;
    }

    /**
     * Change the local users list, notify listeners
     * 
     * @param usersWithJobs new users
     */
    void setSchedulerUsersWithJobs(List<SchedulerUser> usersWithJobs) {
        this.usersWithJobs = usersWithJobs;
        for (UsersListener list : this.usersWithJobsListeners) {
            list.usersUpdated(this.usersWithJobs);
        }
    }

    /**
     * Set local model, notify listeners
     * 
     */
    void setAccountStatistics(HashMap<String, String> stats) {
        this.accountStats = stats;
        for (StatisticsListener list : this.statisticsListeners) {
            list.accountStatsUpdated(stats);
        }
    }

    @Override
    public HashMap<String, String> getAccountStatistics() {
        return this.accountStats;
    }

    void setSchedulerStatistics(HashMap<String, String> stats) {
        this.schedulerStats = stats;
        for (StatisticsListener list : this.statisticsListeners) {
            list.schedulerStatsUpdated(stats);
        }
    }

    @Override
    public HashMap<String, String> getSchedulerStatistics() {
        return this.schedulerStats;
    }

    @Override
    public List<JobUsage> getUsage() {
        return this.usage;
    }

    void setUsage(List<JobUsage> usage) {
        this.usage = usage;
        for (SchedulerListeners.UsageListener list : this.usageListeners) {
            list.usageUpdated(usage);
        }
    }




    public void setThirdPartyCredentialsKeys(Set<String> thirdPartyCredentialsKeys) {
        thirdPartyCredentialsListener.keysUpdated(thirdPartyCredentialsKeys);
    }

    @Override
    public StatHistory getStatHistory(String source) {
        return this.statistics.get(source);
    }

    @Override
    public Map<String, StatHistory> getStatHistory() {
        return this.statistics;
    }

    @Override
    public Range getRequestedStatHistoryRange(String source) {
        Range r = this.requestedStatRange.get(source);
        if (r == null)
            return Range.MINUTE_1;
        return r;
    }


    private String getLogStamp() {
        String date = DateTimeFormat.getFormat(PredefinedFormat.TIME_LONG).format(new Date());
        return "<span style='color:gray'>" + date + "</span> ";
    }

    

    /*
     * (non-Javadoc)
     * @see org.ow2.proactive_grid_cloud_portal.client.EventDispatcher#addSchedulerStateListener(org.ow2.proactive_grid_cloud_portal.client.Listeners.SchedulerStateListener)
     */
    public void addSchedulerStatusListener(SchedulerStatusListener listener) {
        this.schedulerStateListeners.add(listener);
    }
    

    /*
     * (non-Javadoc)
     * @see org.ow2.proactive_grid_cloud_portal.client.EventDispatcher#addUsersListener(org.ow2.proactive_grid_cloud_portal.client.Listeners.UsersListener)
     */
    public void addUsersListener(UsersListener listener) {
        this.usersListeners.add(listener);
    }

    /*
     * (non-Javadoc)
     * @see org.ow2.proactive_grid_cloud_portal.client.EventDispatcher#addUsersWithJobsListener(org.ow2.proactive_grid_cloud_portal.client.Listeners.UsersListener)
     */
    public void addUsersWithJobsListener(UsersListener listener) {
        this.usersWithJobsListeners.add(listener);
    }

    /*
     * (non-Javadoc)
     * @see org.ow2.proactive_grid_cloud_portal.client.EventDispatcher#addStatisticsListener(org.ow2.proactive_grid_cloud_portal.client.Listeners.StatisticsListener)
     */
    public void addStatisticsListener(StatisticsListener listener) {
        this.statisticsListeners.add(listener);
    }


    /*
     * (non-Javadoc)
     * @see org.ow2.proactive_grid_cloud_portal.client.EventDispatcher#addVisualizationListener(org.ow2.proactive_grid_cloud_portal.client.Listeners.VisualizationListener)
     */
    public void addVisualizationListener(VisualizationListener listener) {
        this.visuListeners.add(listener);
    }

    @Override
    public void addStatsListener(StatsListener listener) {
        this.statsListeners.add(listener);
    }

    @Override
    public void addUsageListener(SchedulerListeners.UsageListener listener) {
        this.usageListeners.add(listener);
    }

    @Override
    public void setThirdPartyCredentialsListener(
            SchedulerListeners.ThirdPartyCredentialsListener thirdPartyCredentialsListener) {
        this.thirdPartyCredentialsListener = thirdPartyCredentialsListener;
    }


    public TasksModel getTasksModel() {
        return tasksModel;
    }


    public void setTasksModel(TasksModel tasksModel) {
        this.tasksModel = tasksModel;
    }


    public ExecutionsModel getExecutionsModel() {
        return executionsModel;
    }


    public void setExecutionsModel(ExecutionsModel executionsModel) {
        this.executionsModel = executionsModel;
    }


    public OutputModel getOutputModel() {
        return outputModel;
    }


    public void setOutputModel(OutputModel outputModel) {
        this.outputModel = outputModel;
    }
    
}
