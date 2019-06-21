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
package org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.jobs;

import static org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.jobs.JobsColumnsFactory.COLUMNS_TO_ALIGN;
import static org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.jobs.JobsColumnsFactory.ID_ATTR;
import static org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.jobs.JobsColumnsFactory.ISSUES_ATTR;
import static org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.jobs.JobsColumnsFactory.PROGRESS_ATTR;
import static org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.jobs.JobsColumnsFactory.STATE_ATTR;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.ow2.proactive_grid_cloud_portal.common.client.model.LogModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Job;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.JobPriority;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.JobStatus;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerImages;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.JobsUpdatedListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.JobsController;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.GridColumns;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.ItemsListGrid;

import com.google.gwt.user.client.Window;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.RecordList;
import com.smartgwt.client.data.SortSpecifier;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.types.SortDirection;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.SortNormalizer;
import com.smartgwt.client.widgets.grid.events.SelectionEvent;
import com.smartgwt.client.widgets.grid.events.SelectionUpdatedEvent;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;


/**
 * A list grid that shows jobs.
 *
 * @author The activeeon team.
 */
public class JobsListGrid extends ItemsListGrid<Job> implements JobsUpdatedListener {

    private static final SortSpecifier[] DEFAULT_SORT = new SortSpecifier[] { new SortSpecifier(STATE_ATTR.getName(),
                                                                                                SortDirection.ASCENDING),
                                                                              new SortSpecifier(ID_ATTR.getName(),
                                                                                                SortDirection.DESCENDING) };

    //Specifies if the job id given in the URL has already been automatically selected when loading the page where it is
    private boolean isJobFromUrlAutoSelected = false;

    /**
     * The controller for the jobs grid.
     */
    protected JobsController controller;

    public JobsListGrid(final JobsController controller) {
        super(new JobsColumnsFactory(), "jobsDS_");
        this.emptyMessage = "No jobs to show. You can find workflows to submit in the samples/workflows folder where the Scheduler is installed.";
        this.controller = controller;
        this.controller.getModel().addJobsUpdatedListener(this);
    }

    @Override
    public void build() {
        super.build();
        this.setSelectionProperty("isSelected");
        this.setSort(DEFAULT_SORT);
    }

    @Override
    protected void selectionChangedHandler(SelectionEvent event) {
        if (event.getState() && !fetchingData) {
            ListGridRecord record = event.getRecord();
            Job job = JobRecord.getJob(record);
            controller.selectJob(job);
        }
    }

    @Override
    protected void selectionUpdatedHandler(SelectionUpdatedEvent event) {
        ListGridRecord[] selectedRecords = this.getSelectedRecords();
        List<Integer> selectedJobsIds = new ArrayList<>(selectedRecords.length);
        for (ListGridRecord selectedRecord : selectedRecords) {
            selectedJobsIds.add(JobRecord.getJob(selectedRecord).getId());
        }
        controller.getModel().setSelectedJobsIds(selectedJobsIds);
    }

    @Override
    public void jobsUpdated(Map<Integer, Job> jobs) {
        List<Integer> selectedJobsIds = this.controller.getModel().getSelectedJobsIds();

        this.ds.invalidateCache();
        RecordList data = new RecordList();
        for (Job j : jobs.values()) {
            JobRecord jobRecord = new JobRecord(j);
            this.columnsFactory.buildRecord(j, jobRecord);
            data.add(jobRecord);

            if (selectedJobsIds != null && selectedJobsIds.contains(j.getId())) {
                jobRecord.setAttribute("isSelected", true);
            }
        }

        this.ds.setCacheData(data.toArray());
        data.destroy();
        applyCurrentLocalFilter();

        //select the job given in the URL if it has not been automatically selected before
        if (!isJobFromUrlAutoSelected && selectJobIdFromUrl()) {
            isJobFromUrlAutoSelected = true;
        }
    }

    @Override
    public void jobsUpdating() {
        // Default constructor
    }

    @Override
    public void jobSubmitted(Job j) {
        JobRecord jr = new JobRecord(j);
        DSRequest customErrorHandling = new DSRequest();
        customErrorHandling.setWillHandleError(true);
        this.ds.addData(jr, (dsResponse, o, dsRequest) -> {
            if (dsResponse.getStatus() < 0) {
                // it could fail because results from the server with the new job are already displayed
                // failed silently since the new job is already displayed or will be anyway with next call
                SC.logWarn(dsResponse.getDataAsString());
            }
        }, customErrorHandling);
        applyCurrentLocalFilter();
    }

    @Override
    protected String getCellCSSText(ListGridRecord record, int rowNum, int colNum) {
        String base = super.getCellCSSText(record, rowNum, colNum);
        return getJobStatusFieldColor(record, rowNum, colNum, base);
    }

    protected String getJobStatusFieldColor(ListGridRecord record, int rowNum, int colNum, String base) {
        String fieldName = this.getFieldName(colNum);

        base = highlightRowHavingIssues(rowNum, base);

        /* change the color of the job status field */
        if (fieldName.equals(STATE_ATTR.getName())) {
            try {
                switch (getJobStatus(record)) {
                    case KILLED:
                        return "color:#BE2D1C;font-weight:bold;" + base; // red color
                    case CANCELED:
                        return "color:#C50000;font-weight:bold;" + base; // red color
                    case FAILED:
                        return "color:#850003;font-weight:bold;" + base; // dark Red color
                    case IN_ERROR:
                        return "color:#E94504;font-weight:bold;" + base; // light orange color
                    case RUNNING:
                        return "color:#2051F4;font-weight:bold;" + base; // Blue color
                    case PENDING:
                        return "color:#F0E130;" + base; // Yellow color: Dandelion color name
                    case STALLED:
                        return "color:#1B89B8;font-weight:bold;" + base; // Light Blue color
                    case PAUSED:
                        return "color:#ff9933;font-weight:bold;" + base; // Light Orange color
                    case FINISHED:
                        return base;
                    default:
                        return base;
                }
            } catch (NullPointerException npe) {
                return base;
            }
        }
        return base;
    }

    private boolean selectJobIdFromUrl() {
        String urlJobId = Window.Location.getParameter("job");
        if (urlJobId == null) {
            return true;
        }
        //the job to select should always be the first but we keep this check because sometimes the RecordList is not updated yet
        int recordIndex = this.getRecordList().findIndex("id", Integer.valueOf(urlJobId));
        if (recordIndex >= 0) {
            scrollToRow(recordIndex);
            selectRecord(recordIndex);
            return true;
        } else {
            return false;
        }
    }

    private String highlightRowHavingIssues(int rowNum, String base) {
        Object issues = getEditedCell(rowNum, ISSUES_ATTR.getName());

        if (issues instanceof Integer && !isRowSelected(rowNum)) {
            base = "background-color: #FFE8E8; border-bottom:1px solid #FFD7D7; border-top:1px solid #FFD7D7;";
        }

        return base;
    }

    private boolean isRowSelected(int rowNum) {
        for (ListGridRecord listGridRecord : getSelectedRecords()) {
            int recordIndex = getRecordIndex(listGridRecord);

            if (rowNum == recordIndex) {
                return true;
            }
        }

        return false;
    }

    @Override
    protected Map<GridColumns, ListGridField> buildListGridField() {
        Map<GridColumns, ListGridField> fields = super.buildListGridField();

        alignCells(fields);

        ListGridField idField = fields.get(ID_ATTR);
        idField.setType(ListGridFieldType.INTEGER);

        ListGridField stateField = fields.get(STATE_ATTR);
        stateField.setSortNormalizer(sortStatusAndGroup());

        ListGridField progressField = fields.get(PROGRESS_ATTR);
        progressField.setType(ListGridFieldType.FLOAT);
        progressField.setAlign(Alignment.CENTER);
        progressField.setCellFormatter((value, record, rowNum, colNum) -> {
            int pw = getFieldWidth(PROGRESS_ATTR.getName());
            float progress = 0;
            if (value != null) {
                progress = Float.parseFloat(value.toString());
            }
            int bx = Double.valueOf(Math.ceil(pw * progress)).intValue() - 601;
            String progressUrl = SchedulerImages.instance.progressbar().getSafeUri().asString();

            String style = "display:block; " + //
                           "border: 1px solid #acbac7; " + //
                           "background-image:url(" + progressUrl + ");" + //
                           "background-position:" + bx + "px 0px;" + //
                           "background-repeat: no-repeat;" + //
                           "background-color:#a7cef6";

            Job job = JobRecord.getJob(record);
            String progressCounters = job.getFinishedTasks() + " / " + job.getTotalTasks();
            return "<div style='" + style + "'>" + progressCounters + "</div>";
        });

        ListGridField duration = fields.get(JobsColumnsFactory.DURATION_ATTR);
        duration.setCellFormatter((value, record, rowNum, colNum) -> {
            if (value != null) {
                return Job.formatDuration(value.toString());
            } else {
                return "";
            }
        });

        return fields;
    }

    private void alignCells(Map<GridColumns, ListGridField> fields) {
        for (GridColumns column : COLUMNS_TO_ALIGN) {
            ListGridField listGridField = fields.get(column);
            listGridField.setAlign(Alignment.CENTER);
            listGridField.setCellAlign(Alignment.CENTER);
        }
    }

    /**
     * A custom sort for status:
     * - pending first
     * - running, stalled, paused then
     * - all other status (finished, killed,...)
     */
    private SortNormalizer sortStatusAndGroup() {
        return (record, fieldName) -> {
            String status = record.getAttribute(fieldName);
            if (status.equals(JobStatus.PENDING.toString())) {
                return 0;
            } else if (status.equals(JobStatus.RUNNING.toString()) || status.equals(JobStatus.STALLED.toString()) ||
                       status.equals(JobStatus.PAUSED.toString())) {
                return 1;
            } else {
                return 2;
            }
        };
    }

    protected void buildCellContextualMenu(Menu menu) {
        boolean selPause = true; // ALL selected jobs are paused
        boolean selRunning = true; // ALL selected jobs are running/stalled/pending
        boolean selFinished = true; // ALL selected jobs are finished
        boolean selPauseOrRunning = true; // ALL selected jobs are running/pending/paused/stalled
        boolean selInError = false;
        boolean selSingleSelected = this.getSelectedRecords().length == 1;

        final ArrayList<String> ids = new ArrayList<>(this.getSelectedRecords().length);
        for (ListGridRecord rec : this.getSelectedRecords()) {
            JobStatus status = getJobStatus(rec);

            switch (status) {
                case PENDING:
                case STALLED:
                    selPause = false;
                    selFinished = false;
                    break;
                case RUNNING:
                    selPause = false;
                    selFinished = false;
                    selInError = true;
                    break;
                case PAUSED:
                    selFinished = false;
                    selInError = true;
                    selRunning = false;
                    break;
                case IN_ERROR:
                    selFinished = false;
                    selInError = true;
                    selRunning = true;
                    break;
                default:
                    selPauseOrRunning = false;
                    selRunning = false;
                    selPause = false;
            }

            ids.add(rec.getAttribute(ID_ATTR.getName()));
        }

        MenuItem pauseItem = new MenuItem("Pause",
                                          SchedulerImages.instance.scheduler_pause_16().getSafeUri().asString());
        pauseItem.addClickHandler(event -> controller.pauseJobs(ids));
        pauseItem.setEnabled(selRunning);

        MenuItem restartInErrorTaskItem = new MenuItem("Restart All In-Error Tasks",
                                                       SchedulerImages.instance.scheduler_resume_16()
                                                                               .getSafeUri()
                                                                               .asString());
        restartInErrorTaskItem.addClickHandler(event -> controller.restartAllInErrorTasks(ids));
        restartInErrorTaskItem.setEnabled(selInError);

        MenuItem resumeItem = new MenuItem("Resume All Paused Tasks",
                                           SchedulerImages.instance.scheduler_resume_16().getSafeUri().asString());
        resumeItem.addClickHandler(event -> controller.resumeJobs(ids));
        resumeItem.setEnabled(selPause);

        MenuItem resumeAndRestartItemTask = new MenuItem("Resume All Paused Tasks  & Restart All In-Error Tasks",
                                                         SchedulerImages.instance.scheduler_resume_16()
                                                                                 .getSafeUri()
                                                                                 .asString());
        resumeAndRestartItemTask.addClickHandler(event -> {
            controller.resumeJobs(ids);
            controller.restartAllInErrorTasks(ids);
        });
        resumeAndRestartItemTask.setEnabled(selInError || selPause);

        MenuItem priorityItem = new MenuItem("Priority");
        Menu priorityMenu = new Menu();
        for (final JobPriority p : JobPriority.values()) {
            MenuItem item = new MenuItem(p.toString());
            if (!selPauseOrRunning) {
                item.setEnabled(false);
            } else {
                item.addClickHandler(event -> controller.setJobPriority(ids, p));
            }
            priorityMenu.addItem(item);
        }
        priorityItem.setSubmenu(priorityMenu);

        MenuItem killItem = new MenuItem("Kill", SchedulerImages.instance.scheduler_kill_16().getSafeUri().asString());
        killItem.addClickHandler(event -> controller.killJobs(ids));
        killItem.setEnabled(selPauseOrRunning);

        MenuItem killAndResubmitItem = new MenuItem("Kill & Re-Submit",
                                                    SchedulerImages.instance.job_kill_resubmit_22()
                                                                            .getSafeUri()
                                                                            .asString());
        // Allow killing & re-submitting a job only & only if a single job is selected.
        killAndResubmitItem.addClickHandler(event -> controller.killAndResubmit(ids.get(0)));
        killAndResubmitItem.setEnabled(selSingleSelected && selPauseOrRunning);

        MenuItem resubmitItem = new MenuItem("Re-Submit",
                                             SchedulerImages.instance.job_resubmit_22().getSafeUri().asString());
        // Allow re-submitting a job only & only if a single job is selected.
        resubmitItem.addClickHandler(event -> controller.resubmitJob(ids.get(0)));
        resubmitItem.setEnabled(selSingleSelected);

        MenuItem exportXmlItem = new MenuItem("Export XML",
                                              SchedulerImages.instance.job_export_32().getSafeUri().asString());
        // Allow exporting job's XML only & only if a single job is selected.
        exportXmlItem.addClickHandler(event -> controller.exportJobXML(ids.get(0)));
        exportXmlItem.setEnabled(selSingleSelected);

        MenuItem removeItem = new MenuItem("Remove", SchedulerImages.instance.job_kill_16().getSafeUri().asString());
        removeItem.addClickHandler(event -> controller.removeJob(ids));
        removeItem.setEnabled(selFinished);

        menu.setItems(pauseItem,
                      restartInErrorTaskItem,
                      resumeItem,
                      resumeAndRestartItemTask,
                      priorityItem,
                      killItem,
                      killAndResubmitItem,
                      resubmitItem,
                      exportXmlItem,
                      removeItem);
    }

    private JobStatus getJobStatus(ListGridRecord rec) {
        String jobStatusName = rec.getAttribute(STATE_ATTR.getName());
        return JobStatus.from(jobStatusName);
    }
}
