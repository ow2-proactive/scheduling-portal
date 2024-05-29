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

import static org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.JobsController.PREFIX_SIGNAL_READY;
import static org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.jobs.JobsColumnsFactory.*;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.ow2.proactive_grid_cloud_portal.common.client.Images;
import org.ow2.proactive_grid_cloud_portal.common.client.Settings;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LogModel;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LoginModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.*;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.JobsUpdatedListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.ActionsController;
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
import com.smartgwt.client.widgets.events.DrawEvent;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.SortNormalizer;
import com.smartgwt.client.widgets.grid.events.FieldStateChangedEvent;
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

    //specifies the variable name of the job grid view state in the local storage
    private static final String JOBS_GRID_VIEW_STATE = "jobsGridViewState";

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

    private ActionsWindow actionsWindow;

    private boolean selPause; // ALL selected jobs are paused

    private boolean selRunning; // ALL selected jobs are running/stalled/pending

    private boolean selFinished; // ALL selected jobs are finished

    private boolean selPauseOrRunning; // ALL selected jobs are running/pending/paused/stalled

    private boolean selInError;

    private boolean selSingleSelected;

    private Menu menu;

    private MenuItem actionsItem;

    private MenuItem killItem;

    private MenuItem pauseItem;

    private MenuItem resumeItem;

    private MenuItem resumeAndRestartItemTask;

    private MenuItem priorityItem;

    private Menu priorityMenu;

    private MenuItem exportXmlItem;

    private MenuItem removeItem;

    private MenuItem killAndResubmitItem;

    private MenuItem resubmitItem;

    private MenuItem restartInErrorTaskItem;

    private MenuItem openItem;

    private MenuItem editLabels;

    private MenuItem removeLabels;

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
    protected void fieldStateChangedHandler(FieldStateChangedEvent event) {
        //save the view state in the local storage
        Settings.get().setSetting(JOBS_GRID_VIEW_STATE, this.getViewState());
    }

    @Override
    protected void drawHandler(DrawEvent event) {
        try {
            final String viewState = Settings.get().getSetting(JOBS_GRID_VIEW_STATE);
            if (viewState != null) {
                // restore any previously saved view state for this grid
                this.setViewState(viewState);
            }
        } catch (Exception e) {
            LogModel.getInstance().logImportantMessage("Failed to restore jobs grid view state " + e);
        }
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
        controller.checkJobsPermissionMethods(selectedJobsIds.stream()
                                                             .map(String::valueOf)
                                                             .collect(Collectors.toList()),
                                              this);
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

        ListGridField startTime = fields.get(JobsColumnsFactory.START_TIME_ATTR);
        startTime.setSortNormalizer(customDateSorting("startTime"));

        ListGridField finishTime = fields.get(JobsColumnsFactory.FINISHED_TIME_ATTR);
        finishTime.setSortNormalizer(customDateSorting("finishTime"));

        ListGridField submitTime = fields.get(JobsColumnsFactory.SUBMIT_TIME_ATTR);
        submitTime.setSortNormalizer(customDateSorting("submitTime"));

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
                       status.equals(JobStatus.PAUSED.toString()) || status.equals(JobStatus.IN_ERROR.toString())) {
                return 1;
            } else {
                return 2;
            }
        };
    }

    /**
     * A custom sort for job submit, start, and finish times:
     * dates are sorted according to the job epoch time and not
     * according to the String representation given by JSUtil.getTime(long Time)
     */
    private SortNormalizer customDateSorting(String date) {
        return (record, fieldName) -> {
            Job job = JobRecord.getJob(record);
            if (date.equals("startTime")) {
                return job.getStartTime();
            }
            if (date.equals("finishTime")) {
                return job.getFinishTime();
            }
            return job.getSubmitTime();
        };
    }

    protected void buildCellContextualMenu(Menu menu) {

        selPause = true;
        selRunning = true;
        selFinished = true;
        selPauseOrRunning = true;
        selInError = false;
        selSingleSelected = this.getSelectedRecords().length == 1;
        this.menu = menu;
        final ArrayList<String> ids = new ArrayList<>(this.getSelectedRecords().length);
        final ArrayList<String> jobLabels = new ArrayList<>(this.getSelectedRecords().length);
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
            jobLabels.add(rec.getAttribute(LABEL_ATTRIBUTE.getName()));
        }

        pauseItem = new MenuItem("Pause", SchedulerImages.instance.scheduler_pause_16().getSafeUri().asString());
        pauseItem.addClickHandler(event -> controller.pauseJobs(ids));
        pauseItem.setEnabled(selRunning);

        restartInErrorTaskItem = new MenuItem("Restart All In-Error Tasks",
                                              SchedulerImages.instance.scheduler_resume_16().getSafeUri().asString());
        restartInErrorTaskItem.addClickHandler(event -> controller.restartAllInErrorTasks(ids));
        restartInErrorTaskItem.setEnabled(selInError);

        resumeItem = new MenuItem("Resume All Paused Tasks",
                                  SchedulerImages.instance.scheduler_resume_16().getSafeUri().asString());
        resumeItem.addClickHandler(event -> controller.resumeJobs(ids));
        resumeItem.setEnabled(selPause);

        resumeAndRestartItemTask = new MenuItem("Resume All Paused Tasks  & Restart All In-Error Tasks",
                                                SchedulerImages.instance.scheduler_resume_16().getSafeUri().asString());
        resumeAndRestartItemTask.addClickHandler(event -> {
            controller.resumeJobs(ids);
            controller.restartAllInErrorTasks(ids);
        });
        resumeAndRestartItemTask.setEnabled(selInError || selPause);

        priorityItem = new MenuItem("Priority");
        priorityMenu = new Menu();
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

        killItem = new MenuItem("Kill", SchedulerImages.instance.scheduler_kill_16().getSafeUri().asString());
        killItem.addClickHandler(event -> controller.killJobs(ids));
        killItem.setEnabled(selPauseOrRunning);

        killAndResubmitItem = new MenuItem("Kill & Re-Submit",
                                           SchedulerImages.instance.job_kill_resubmit_22().getSafeUri().asString());
        // Allow killing & re-submitting a job only & only if a single job is selected.
        killAndResubmitItem.addClickHandler(event -> controller.killAndResubmit(ids.get(0)));
        killAndResubmitItem.setEnabled(selSingleSelected && selPauseOrRunning);

        resubmitItem = new MenuItem("Re-Submit", SchedulerImages.instance.job_resubmit_22().getSafeUri().asString());
        if (ids.size() > 1) {
            // Allow re-submitting many jobs without showing the submit window, when more than one job is selected
            resubmitItem.addClickHandler(event -> controller.resubmitAllJobs(ids));
        } else {
            // Allow re-submitting a job only with a submit window, only if a single job is selected.
            resubmitItem.addClickHandler(event -> controller.resubmitJob(ids.get(0)));
        }

        openItem = new MenuItem("Open in Studio", Images.instance.studio_30().getSafeUri().asString());
        openItem.addClickHandler(event -> controller.openStudio(ids.get(0)));
        openItem.setEnabled(selSingleSelected);

        exportXmlItem = new MenuItem("Export XML", SchedulerImages.instance.job_export_32().getSafeUri().asString());
        // Allow exporting job's XML only & only if a single job is selected.
        exportXmlItem.addClickHandler(event -> controller.exportJobXML(ids.get(0)));
        exportXmlItem.setEnabled(selSingleSelected);

        removeItem = new MenuItem("Remove", SchedulerImages.instance.job_kill_16().getSafeUri().asString());
        removeItem.addClickHandler(event -> controller.removeJob(ids));
        removeItem.setEnabled(selFinished);

        actionsItem = new MenuItem("Signals");
        actionsItem.setEnabled(false);

        editLabels = new MenuItem("Edit label", SchedulerImages.instance.label().getSafeUri().asString());
        editLabels.setEnabled(true);

        removeLabels = new MenuItem("Remove label", SchedulerImages.instance.remove_label().getSafeUri().asString());
        removeLabels.addClickHandler(event -> controller.removeJobLabel(ids));
        removeLabels.setEnabled(true);

        this.menu.setItems(actionsItem,
                           pauseItem,
                           restartInErrorTaskItem,
                           resumeItem,
                           resumeAndRestartItemTask,
                           priorityItem,
                           killItem,
                           killAndResubmitItem,
                           resubmitItem,
                           openItem,
                           exportXmlItem,
                           removeItem,
                           editLabels,
                           removeLabels);

        controller.getJobSignals(ids.get(0), this);
        controller.checkJobsPermissionMethods(ids, this);
        controller.getJobLabels(this, ids);
        checkIfLabelsAreEmpty(menu, jobLabels);
    }

    private void checkIfLabelsAreEmpty(Menu menu, List<String> jobLabels) {
        if (jobLabels.stream().allMatch(String::isEmpty)) {
            removeLabels.setEnabled(false);
            menu.redraw();
        }
    }

    public void addActionsMenu(String jobId, Map<String, Map<String, Map<String, String>>> detailedSignals) {
        Menu signalsMenu = new Menu();
        for (String signal : detailedSignals.keySet()) {
            MenuItem item = new MenuItem(signal.substring(PREFIX_SIGNAL_READY.length()));
            boolean signalDoesNotHaveVariables = detailedSignals.get(signal)
                                                                .values()
                                                                .stream()
                                                                .allMatch(Objects::isNull);
            boolean allVariablesAreHidden = detailedSignals.get(signal)
                                                           .values()
                                                           .stream()
                                                           .allMatch(entry -> Boolean.parseBoolean(entry.get("hidden")));
            item.addClickHandler(event -> {
                if (signalDoesNotHaveVariables || allVariablesAreHidden) {
                    ActionsController actionsController = new ActionsController(signal.substring(PREFIX_SIGNAL_READY.length()),
                                                                                jobId);
                    actionsController.addJobSignal();
                } else {
                    actionsWindow = new ActionsWindow(signal.substring(PREFIX_SIGNAL_READY.length()),
                                                      detailedSignals.get(signal),
                                                      jobId);
                    actionsWindow.show();
                }
            });

            signalsMenu.addItem(item);
        }
        actionsItem.setSubmenu(signalsMenu);
        actionsItem.setEnabled(selSingleSelected && selPauseOrRunning && !detailedSignals.keySet().isEmpty());
        menu.redraw();
    }

    public void addLabelsMenu(Map<String, String> labels, ArrayList<String> ids) {
        Menu labelsMenu = new Menu();
        for (String labelKey : labels.keySet()) {
            MenuItem item = new MenuItem(labels.get(labelKey));
            item.addClickHandler(event -> controller.setLabelOnJobs(labelKey, ids));
            labelsMenu.addItem(item);
        }
        editLabels.setSubmenu(labelsMenu);
        editLabels.setEnabled(!labels.isEmpty());
        menu.redraw();
    }

    private JobStatus getJobStatus(ListGridRecord rec) {
        String jobStatusName = rec.getAttribute(STATE_ATTR.getName());
        return JobStatus.from(jobStatusName);
    }

    private JobPriority getJobPriority(ListGridRecord rec) {
        return JobPriority.findPriority(rec.getAttribute(PRIORITY_ATTR.getName()));
    }

    public void setMenuItemsStatus(List<String> jobIds) {
        LoginModel loginModel = LoginModel.getInstance();
        if (loginModel.userDoesNotHavePermissionToPauseJobs(jobIds)) {
            pauseItem.setEnabled(false);
        }
        if (loginModel.userDoesNotHavePermissionToRestartAllInErrorTasks(jobIds)) {
            restartInErrorTaskItem.setEnabled(false);
            resumeAndRestartItemTask.setEnabled(false);
        }
        if (loginModel.userDoesNotHavePermissionToResumeJobs(jobIds)) {
            resumeItem.setEnabled(false);
            resumeAndRestartItemTask.setEnabled(false);
        }
        if (loginModel.userDoesNotHavePermissionToChangeJobsPriority(jobIds)) {
            priorityItem.setEnabled(false);
        } else {
            /*
             * If the change priority action is authorized for a user,
             * we need to check the specific priorities the user is allowed to set
             */
            boolean isSingleSelection = this.getSelectedRecords().length == 1;
            boolean allSelectedRecordsHaveSamePriority = Arrays.stream(this.getSelectedRecords())
                                                               .map(this::getJobPriority)
                                                               .distinct()
                                                               .count() == 1;
            List<String> userPrioritiesPermission = loginModel.getUserPrioritiesPermission();
            Stream.of(priorityMenu.getItems()).forEach(priorityMenuItem -> {
                if (!userPrioritiesPermission.contains(priorityMenuItem.getTitle())) {
                    priorityMenuItem.setEnabled(false);
                }
                if ((isSingleSelection || allSelectedRecordsHaveSamePriority) &&
                    priorityMenuItem.getTitle().equals(getJobPriority(this.getSelectedRecord()).toString())) {
                    priorityMenuItem.setIcon(Images.instance.ok_16().getSafeUri().asString());
                }
            });
        }
        if (loginModel.userDoesNotHavePermissionToKillJobs(jobIds)) {
            killItem.setEnabled(false);
            killAndResubmitItem.setEnabled(false);
        }
        if (loginModel.userDoesNotHavePermissionToGetJobsContent(jobIds)) {
            resubmitItem.setEnabled(false);
            exportXmlItem.setEnabled(false);
            openItem.setEnabled(false);
            killAndResubmitItem.setEnabled(false);
        }
        if (loginModel.userDoesNotHavePermissionToRemoveJobs(jobIds)) {
            removeItem.setEnabled(false);
            menu.redraw();
        }
        if (loginModel.userDoesNotHavePermissionToSetLabelOnJobs(jobIds)) {
            editLabels.setEnabled(false);
            removeLabels.setEnabled(false);
        }
        menu.redraw();
    }
}
