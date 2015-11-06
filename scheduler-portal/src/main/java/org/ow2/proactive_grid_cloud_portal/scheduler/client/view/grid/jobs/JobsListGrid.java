package org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.jobs;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.Job;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.JobPriority;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.JobStatus;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerImages;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.JobsUpdatedListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.JobsController;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.GridColumns;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.ItemsListGrid;

import com.smartgwt.client.data.DSCallback;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.RecordList;
import com.smartgwt.client.data.SortSpecifier;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.types.SortDirection;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.grid.CellFormatter;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.SortNormalizer;
import com.smartgwt.client.widgets.grid.events.SelectionEvent;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.ClickHandler;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;

/**
 * A list grid that shows jobs.
 * @author The activeeon team.
 *
 */
public class JobsListGrid extends ItemsListGrid<Job> implements JobsUpdatedListener{

    private static final SortSpecifier[] DEFAULT_SORT = new SortSpecifier[] {
        new SortSpecifier(JobsColumnsFactory.STATE_ATTR.getName(), SortDirection.ASCENDING),
        new SortSpecifier(JobsColumnsFactory.ID_ATTR.getName(), SortDirection.DESCENDING) };
    
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
    
    
    protected void selectionChangedHandler(SelectionEvent event){
        if (event.getState() && !fetchingData) {
            ListGridRecord record = event.getRecord();
            Job job = JobRecord.getJob(record);
            controller.selectJob(job);
        }
    }
    
    
    
    @Override
    public void jobsUpdated(Map<Integer, Job> jobs) {
        boolean selectedJobRemoved = this.getSelectedRecord() != null;
        List<Integer> selectedIds = listSelectedJobs();

        RecordList data = new RecordList();
        for (Job j : jobs.values()) {
            JobRecord jobRecord = new JobRecord(j);
            this.columnsFactory.buildRecord(j, jobRecord);
            data.add(jobRecord);
            boolean isSelectedJob = selectedIds.contains(jobRecord.getAttributeAsInt(JobsColumnsFactory.ID_ATTR.getName()));
            jobRecord.setAttribute("isSelected", isSelectedJob);
            if (isSelectedJob) {
                selectedJobRemoved = false;
            }
        }

        if (selectedJobRemoved) {
            this.controller.selectJob(null);
        }

        this.ds.setTestData(data.toArray());
        applyCurrentLocalFilter();
        
    }

    @Override
    public void jobsUpdating() {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void jobSubmitted(Job j) {
        JobRecord jr = new JobRecord(j);
        DSRequest customErrorHandling = new DSRequest();
        customErrorHandling.setWillHandleError(true);
        this.ds.addData(jr, new DSCallback() {
            @Override
            public void execute(DSResponse dsResponse, Object o, DSRequest dsRequest) {
                if (dsResponse.getStatus() < 0) {
                    // it could fail because results from the server with the new job are already displayed
                    // failed silently since the new job is already displayed or will be anyway with next call
                    SC.logWarn(dsResponse.getDataAsString());
                }
            }
        }, customErrorHandling);
        applyCurrentLocalFilter();
    }

    
    private List<Integer> listSelectedJobs() {
        List<Integer> selectedIds = new ArrayList<Integer>();
        for (ListGridRecord listGridRecord : this.getSelectedRecords()) {
            selectedIds.add(listGridRecord.getAttributeAsInt(JobsColumnsFactory.ID_ATTR.getName()));
        }
        return selectedIds;
    }
    
    
    @Override
    protected String getCellCSSText(ListGridRecord record, int rowNum, int colNum) {
        String base = super.getCellCSSText(record, rowNum, colNum);
        return getJobStatusFieldColor(record, rowNum, colNum, base);
    }
    
    protected String getJobStatusFieldColor(ListGridRecord record, int rowNum, int colNum, String base){
        String fieldName = this.getFieldName(colNum);

        /* change the color of the job status field  */
        if (fieldName.equals(JobsColumnsFactory.STATE_ATTR.getName())) {
            try {
                switch (JobStatus.valueOf(record.getAttribute(JobsColumnsFactory.STATE_ATTR.getName()).toUpperCase())) {
                    case KILLED:
                        return "color:#d37a11;font-weight:bold;" + base;
                    case CANCELED:
                    case FAILED:
                        return "color:#c50000;font-weight:bold;" + base;
                    case RUNNING:
                        return "color:#176925;font-weight:bold;" + base;
                    case PENDING:
                        return "color:#1a8bba;" + base;
                    case STALLED:
                    case PAUSED:
                        return "font-weight:bold;" + base;
                    case FINISHED:
                        return base;
                }
            } catch (NullPointerException npe) {
                return base;
            }
        }
        return base;
    }
    
    
    protected Map<GridColumns, ListGridField> buildListGridField(){
        Map<GridColumns, ListGridField> fields = super.buildListGridField();
        
        ListGridField idField = fields.get(JobsColumnsFactory.ID_ATTR);
        idField.setType(ListGridFieldType.INTEGER);
        idField.setAlign(Alignment.LEFT);
        idField.setCellAlign(Alignment.LEFT);

        ListGridField stateField = fields.get(JobsColumnsFactory.STATE_ATTR);
        stateField.setSortNormalizer(sortStatusAndGroup());

        ListGridField progressField = fields.get(JobsColumnsFactory.PROGRESS_ATTR);
        progressField.setType(ListGridFieldType.FLOAT);
        progressField.setAlign(Alignment.CENTER);
        progressField.setCellFormatter(new CellFormatter() {
            public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
                int pw = getFieldWidth(JobsColumnsFactory.PROGRESS_ATTR.getName());
                float progress = Float.parseFloat(value.toString());
                int bx = new Double(Math.ceil(pw * progress)).intValue() - 601;
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
            }
        });

        
        ListGridField duration = fields.get(JobsColumnsFactory.DURATION_ATTR);
        duration.setCellFormatter(new CellFormatter() {
            public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
                long l = Long.parseLong(value.toString());
                return Job.formatDuration(l);
            }
        });
        
        return fields;
    }
    
    
    /**
     * A custom sort for status:
     *  - pending first
     *  - running, stalled, paused then
     *  - all other status (finished, killed,...)
     */
    private SortNormalizer sortStatusAndGroup() {
        return new SortNormalizer() {
            @Override
            public Object normalize(ListGridRecord record, String fieldName) {
                String status = record.getAttribute(fieldName);
                if (status.equals(JobStatus.PENDING.toString())) {
                    return 0;
                } else if (status.equals(JobStatus.RUNNING.toString())
                  || status.equals(JobStatus.STALLED.toString())
                  || status.equals(JobStatus.PAUSED.toString())) {
                    return 1;
                } else {
                    return 2;
                }
            }
        };
    }
    
    
    protected void buildCellContextualMenu(Menu menu){
        boolean selPause = true; // ALL selected jobs are paused
        boolean selRunning = true; // ALL selected jobs are running/stalled/pending
        boolean selFinished = true; // ALL selected jobs are finished
        boolean selPauseOrRunning = true; // ALL selected jobs are running/pending/paused/stalled

        final ArrayList<String> ids = new ArrayList<String>(this.getSelectedRecords().length);
        for (ListGridRecord rec : this.getSelectedRecords()) {
            switch (JobStatus.valueOf(rec.getAttribute(JobsColumnsFactory.STATE_ATTR.getName()).toUpperCase())) {
                case PENDING:
                case RUNNING:
                case STALLED:
                    selPause = false;
                    selFinished = false;
                    break;
                case PAUSED:
                    selFinished = false;
                    selRunning = false;
                    break;
                default:
                    selPauseOrRunning = false;
                    selRunning = false;
                    selPause = false;
            }

            ids.add(rec.getAttribute(JobsColumnsFactory.ID_ATTR.getName()));
        }
        
        MenuItem pauseItem = new MenuItem("Pause", SchedulerImages.instance.scheduler_pause_16()
                .getSafeUri().asString());
        pauseItem.addClickHandler(new ClickHandler() {
            public void onClick(MenuItemClickEvent event) {
                controller.pauseJobs(ids);
            }
        });
        pauseItem.setEnabled(selRunning);

        MenuItem resumeItem = new MenuItem("Resume", SchedulerImages.instance.scheduler_resume_16()
                .getSafeUri().asString());
        resumeItem.addClickHandler(new ClickHandler() {
            public void onClick(MenuItemClickEvent event) {
                controller.resumeJobs(ids);
            }
        });
        resumeItem.setEnabled(selPause);

        MenuItem priorityItem = new MenuItem("Priority");
        Menu priorityMenu = new Menu();
        for (final JobPriority p : JobPriority.values()) {
            MenuItem item = new MenuItem(p.toString());
            if (!selPauseOrRunning) {
                item.setEnabled(false);
            } else {
                item.addClickHandler(new ClickHandler() {
                    public void onClick(MenuItemClickEvent event) {
                        controller.setJobPriority(ids, p);
                    }
                });
            }
            priorityMenu.addItem(item);
        }
        priorityItem.setSubmenu(priorityMenu);

        MenuItem removeItem = new MenuItem("Remove", SchedulerImages.instance.job_kill_16()
                .getSafeUri().asString());
        removeItem.addClickHandler(new ClickHandler() {
            public void onClick(MenuItemClickEvent event) {
                controller.removeJob(ids);
            }
        });

        MenuItem killItem = new MenuItem("Kill", SchedulerImages.instance.scheduler_kill_16()
                .getSafeUri().asString());
        killItem.addClickHandler(new ClickHandler() {
            public void onClick(MenuItemClickEvent event) {
                controller.killJob(ids);
            }
        });

        killItem.setEnabled(selPauseOrRunning);
        removeItem.setEnabled(selFinished);

        menu.setItems(pauseItem, resumeItem, priorityItem, removeItem, killItem);
    }
}
