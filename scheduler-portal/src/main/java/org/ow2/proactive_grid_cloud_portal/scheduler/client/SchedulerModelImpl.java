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

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.ow2.proactive_grid_cloud_portal.common.client.Listeners.StatsListener;
import org.ow2.proactive_grid_cloud_portal.common.client.Model.StatHistory.Range;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.JobOutputListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.JobSelectedListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.JobsUpdatedListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.RemoteHintListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.SchedulerStatusListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.StatisticsListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.TasksUpdatedListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.UsersListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.VisualizationListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.PaginationModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.TasksModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.TasksNavigationModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.shared.JobVisuMap;

import com.google.gwt.i18n.client.DateTimeFormat;
import com.google.gwt.i18n.client.DateTimeFormat.PredefinedFormat;
import com.google.gwt.safehtml.shared.SafeHtmlUtils;


/**
 * Writable Model, should only be used by the Controller
 *
 *
 * @author mschnoor
 *
 */
public class SchedulerModelImpl extends SchedulerModel implements SchedulerEventDispatcher {

    private static final String PLATFORM_INDEPENDENT_LINE_BREAK = "\r\n?|\n";
    

    
    private SchedulerStatus schedulerStatus = SchedulerStatus.STARTED;
    private LinkedHashMap<Integer, Job> jobs = null;
    private long jobsRev = -1;
    private Job selectedJob = null;
    
    private HashMap<Integer, JobOutput> output = null;
    private HashSet<String> isLiveOutput = null;
    
    private HashMap<String, StringBuffer> liveOutput = null;
    private boolean fetchMyJobsOnly = false;
    private boolean fetchPending = true;
    private boolean fetchRunning = true;
    private boolean fetchFinished = true;
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


    private ArrayList<JobsUpdatedListener> jobsUpdatedListeners = null;
    private ArrayList<JobSelectedListener> jobSelectedListeners = null;
    private ArrayList<SchedulerStatusListener> schedulerStateListeners = null;
    private ArrayList<JobOutputListener> jobOutputListeners = null;
    private ArrayList<UsersListener> usersListeners = null;
    private ArrayList<UsersListener> usersWithJobsListeners = null;
    private ArrayList<StatisticsListener> statisticsListeners = null;
    private ArrayList<VisualizationListener> visuListeners = null;
    private ArrayList<StatsListener> statsListeners = null;
    private ArrayList<SchedulerListeners.UsageListener> usageListeners = null;
    private SchedulerListeners.ThirdPartyCredentialsListener thirdPartyCredentialsListener;


    private PaginationModel jobsPaginationModel;

    private TasksModel tasksModel;

    SchedulerModelImpl() {
        super();

        this.output = new HashMap<Integer, JobOutput>();
        this.isLiveOutput = new HashSet<String>();
        this.liveOutput = new HashMap<String, StringBuffer>();
        this.jobsUpdatedListeners = new ArrayList<JobsUpdatedListener>();
        this.jobSelectedListeners = new ArrayList<JobSelectedListener>();
        this.schedulerStateListeners = new ArrayList<SchedulerStatusListener>();
        this.jobOutputListeners = new ArrayList<JobOutputListener>();
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
    public LinkedHashMap<Integer, Job> getJobs() {
        return this.jobs;
    }

    @Override
    public void emptyJobs() {
        setJobs(null, -1);
    }

    /**
     * Modifies the local joblist
     * triggers {@link JobsUpdatedListener#jobsUpdated(java.util.Map)}},
     * or {@link JobsUpdatedListener#jobsUpdating()} if <code>jobs</code> was null
     * 
     * @param jobs a jobset, or null
     * @param rev the revision of this jobset
     */
    void setJobs(LinkedHashMap<Integer, Job> jobs, long rev) {
        this.jobs = jobs;
        this.jobsRev = rev;
        boolean empty = false;

        if (jobs == null) {
            empty = true;
            this.jobs = new LinkedHashMap<Integer, Job>();
        }

        for (JobsUpdatedListener listener : this.jobsUpdatedListeners) {
            listener.jobsUpdated(this.jobs);
            if (empty)
                listener.jobsUpdating();
        }
        
        if(this.selectedJob != null){
            Job oldSel = this.selectedJob;
            this.selectedJob = jobs.get(oldSel.getId());
            if(this.selectedJob == null){
                for(JobSelectedListener listener: this.jobSelectedListeners){
                    listener.jobUnselected();
                }
            }
            else{
                if (this.selectedJob != null && !this.selectedJob.isEqual(oldSel)) {
                    for(JobSelectedListener listener: this.jobSelectedListeners){
                        listener.selectedJobUpdated();
                    }
                }
            }
        }
    }

    void jobSubmitted(Job j) {
        for (JobsUpdatedListener listener : this.jobsUpdatedListeners) {
            listener.jobSubmitted(j);
        }
    }

    void jobsUpdating() {
        for (JobsUpdatedListener listener : this.jobsUpdatedListeners) {
            listener.jobsUpdating();
        }
    }




    public PaginationModel getJobsPaginationModel() {
        return jobsPaginationModel;
    }





    /**
     * Modifies the Job selection,
     * triggers a JobSelected event
     *
     */
    void selectJob(int jobId) {
        Job j = null;
        // find the job
        for (Job it : this.jobs.values()) {
            if (it.getId() == jobId) {
                j = it;
            }
        }
        boolean selChanged = this.selectedJob == null || !this.selectedJob.equals(j);
        this.selectedJob = j;

        // notify job selection listeners
        for (JobSelectedListener listener : this.jobSelectedListeners) {
            if (j == null)
                listener.jobUnselected();
            else
                listener.jobSelected(j);
        }

        // tasks list will change, notify tasks listeners
        this.tasksModel.notifyTasksChanging(j, selChanged);
    }



    @Override
    public Job getSelectedJob() {
        return this.selectedJob;
    }

    @Override
    public Job getJob(int jobId) {
        for (Job j : this.jobs.values()) {
            if (j.getId() == jobId) {
                return j;
            }
        }
        return null;
    }

    @Override
    public long getJobsRevision() {
        return this.jobsRev;
    }







    


    

    @Override
    public JobOutput getJobOutput(int jobId) {
        return this.output.get(jobId);
    }

    /**
     * Set the output for a given task in a given job
     * 
     * notify listeners
     * 
     */
    void setTaskOutput(int jobId, Task task, String output) {
        JobStatus stat = null;
        for (Job j : this.jobs.values()) {
            if (jobId == j.getId())
                stat = j.getStatus();
        }
        if (stat == null) {
            throw new IllegalStateException("Trying to set output for a task in job " + jobId +
                    " for which there is no local representation");
        }

        List<String> lines = new ArrayList<String>();

        for (String line : lineByLine(output)) {
            addRemoteHintIfNecessary(line);
            line = formatLine(line);

            if (!line.trim().isEmpty()) {
                lines.add(line);
            }
        }

        if (this.output.get(jobId) == null) {
            JobOutput jo = new JobOutput(jobId);
            jo.update(task, lines);
            this.output.put(jobId, jo);
        } else {
            this.output.get(jobId).update(task, lines);
        }

        this.updateOutput(jobId);
    }

    private String[] lineByLine(String lines) {
        return lines.split(PLATFORM_INDEPENDENT_LINE_BREAK);
    }

    private void addRemoteHintIfNecessary(String line) {
        if (line.contains(TasksModel.PA_REMOTE_CONNECTION)) {
            tasksModel.addRemoteHint(line);
        }
    }

    

    /**
     * Notify listeners that the output of a given job has changed
     * 
     * @param jobId the job for which the output changed
     */
    void updateOutput(int jobId) {
        if (this.output.get(jobId) == null) {
            JobOutput jo = new JobOutput(jobId);
            this.output.put(jobId, jo);
        }

        for (JobOutputListener listener : this.jobOutputListeners) {
            listener.jobOutputUpdated(this.output.get(jobId));
        }
    }

    /**
     * Append a job output fragment to the stored live output
     * @param jobId id of the job to which this fragment belongs
     * @param out job output fragment
     */
    void appendLiveOutput(String jobId, String out) {
        String[] expl = lineByLine(out);
        out = "";
        for (String str : expl) {
            addRemoteHintIfNecessary(str);
            out += formatLine(str);
        }

        StringBuffer buf = this.liveOutput.get(jobId);
        if (buf == null) {
            buf = new StringBuffer();
            this.liveOutput.put(jobId, buf);
        }
        buf.append(out);

        for (JobOutputListener list : this.jobOutputListeners) {
            list.liveOutputUpdated(jobId, buf.toString());
        }
    }

    private String formatLine(String str) {
        if (str.matches("\\[.*\\].*")) {
            str = SafeHtmlUtils.htmlEscape(str).replaceFirst("]", "]</span>");
            return "<nobr><span style='color:gray;'>" + str +"</nobr><br>";
        }
        return "";
    }

    @Override
    public String getLiveOutput(String jobId) {
        StringBuffer buf = this.liveOutput.get(jobId);
        if (buf == null) {
            return "";
        } else {
            return buf.toString();
        }
    }

    @Override
    public boolean isLiveOutput(String jobId) {
        return this.isLiveOutput.contains(jobId);
    }

    /**
     * The output for this job should be fetched live
     * @param jobId id of the job
     * @param isLiveOutput true to live fetch
     */
    void setLiveOutput(String jobId, boolean isLiveOutput) {
        if (isLiveOutput) {
            this.isLiveOutput.add(jobId);
            if(!liveOutput.containsKey(jobId)){
                this.liveOutput.put(jobId, new StringBuffer());
            }
        } else {
            this.isLiveOutput.remove(jobId);
        }
    }

    

    @Override
    public boolean isFetchMyJobsOnly() {
        return fetchMyJobsOnly;
    }

    void fetchMyJobsOnly(boolean b) {
        this.fetchMyJobsOnly = b;
    }

    @Override
    public boolean isFetchPendingJobs() {
        return this.fetchPending;
    }

    void fetchPending(boolean f) {
        this.fetchPending = f;
    }

    @Override
    public boolean isFetchRunningJobs() {
        return this.fetchRunning;
    }

    void fetchRunning(boolean f) {
        this.fetchRunning = f;
    }

    @Override
    public boolean isFetchFinishedJobs() {
        return this.fetchFinished;
    }

    void fetchFinished(boolean f) {
        this.fetchFinished = f;
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
     * @see org.ow2.proactive_grid_cloud_portal.client.EventDispatcher#addJobsUpdatedListener(org.ow2.proactive_grid_cloud_portal.client.Listeners.JobsUpdatedListener)
     */
    public void addJobsUpdatedListener(JobsUpdatedListener listener) {
        this.jobsUpdatedListeners.add(listener);
    }

    /*
     * (non-Javadoc)
     * @see org.ow2.proactive_grid_cloud_portal.client.EventDispatcher#addJobSelectedListener(org.ow2.proactive_grid_cloud_portal.client.Listeners.JobSelectedListener)
     */
    public void addJobSelectedListener(JobSelectedListener listener) {
        this.jobSelectedListeners.add(listener);
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
     * @see org.ow2.proactive_grid_cloud_portal.client.EventDispatcher#addJobOutputListener(org.ow2.proactive_grid_cloud_portal.client.Listeners.JobOutputListener)
     */
    public void addJobOutputListener(JobOutputListener listener) {
        this.jobOutputListeners.add(listener);
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


    public void setJobsPaginationModel(PaginationModel jobsPaginationModel) {
        this.jobsPaginationModel = jobsPaginationModel;
    }


    public TasksModel getTasksModel() {
        return tasksModel;
    }


    public void setTasksModel(TasksModel tasksModel) {
        this.tasksModel = tasksModel;
    }
    
    
    
}
