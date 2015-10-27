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
package org.ow2.proactive_grid_cloud_portal.scheduler.client.view;

import java.util.ArrayList;
import java.util.Collection;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;

import org.ow2.proactive_grid_cloud_portal.common.client.model.LoginModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Job;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.JobPriority;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.JobStatus;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerImages;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.JobsUpdatedListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.JobsController;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.JobsColumns;

import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.data.DSCallback;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.DataSourceField;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.RecordList;
import com.smartgwt.client.data.SortSpecifier;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.types.SortDirection;
import com.smartgwt.client.types.TopOperatorAppearance;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Img;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.MouseOutEvent;
import com.smartgwt.client.widgets.events.MouseOutHandler;
import com.smartgwt.client.widgets.events.MouseOverEvent;
import com.smartgwt.client.widgets.events.MouseOverHandler;
import com.smartgwt.client.widgets.form.FilterBuilder;
import com.smartgwt.client.widgets.grid.CellFormatter;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.SortNormalizer;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
import com.smartgwt.client.widgets.grid.events.CellOutEvent;
import com.smartgwt.client.widgets.grid.events.CellOutHandler;
import com.smartgwt.client.widgets.grid.events.SelectionChangedHandler;
import com.smartgwt.client.widgets.grid.events.SelectionEvent;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.ClickHandler;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;
import com.smartgwt.client.widgets.toolbar.ToolStrip;


/**
 * Contains the ListGrid that displays jobs
 */
public class JobsView extends AbstractGridItemsView implements JobsUpdatedListener {

    
    private static final String JOB_ATTR = "job";
    
    /** jobs filtering */
    protected Layout filterPane = null;
    
    protected Layout contentPane = null;
    
    protected Img filterButtonLabel = null;

    private static final SortSpecifier[] DEFAULT_SORT = new SortSpecifier[] {
      new SortSpecifier(JobsColumns.STATE_ATTR.getName(), SortDirection.ASCENDING),
      new SortSpecifier(JobsColumns.ID_ATTR.getName(), SortDirection.DESCENDING) };

    private class JobRecord extends ListGridRecord {

        public JobRecord(Job j) {
            setAttribute(JobsColumns.ID_ATTR.getName(), j.getId());
            update(j);
        }

        /**
         * Updates the attributes of this record to reflect the fields of the job <code>j</code>
         *
         * @param j a Job
         */
        public void update(Job j) {
            float progress = (float) j.getFinishedTasks() / (float) j.getTotalTasks();
            long duration = -1;
            if (j.getFinishTime() > 0 && j.getStartTime() > 0) {
                duration = j.getFinishTime() - j.getStartTime();
            }

            setAttribute(JobsColumns.STATE_ATTR.getName(), j.getStatus().toString());
            setAttribute(JobsColumns.USER_ATTR.getName(), j.getUser());
            setAttribute(JobsColumns.PRIORITY_ATTR.getName(), j.getPriority().toString());
            setAttribute(JobsColumns.NAME_ATTR.getName(), j.getName());
            setAttribute(JobsColumns.DURATION_ATTR.getName(), duration);
            setAttribute(JobsColumns.PROGRESS_ATTR.getName(), progress);
            setAttribute(JOB_ATTR, j);
        }
    }

    
    
    
    private class JobDS extends DataSource {

        public JobDS(String id) {
            setID(id);
            DataSourceField [] fields = buildDatasourceFields();
            setFields(fields);
            setClientOnly(true);
        }
    }

    /**
     * the Grid widget displayed in the view
     */
    private ListGrid jobsGrid = null;
    /**
     * shown in undeterminate state
     */
    private Label jobsLoading = null;
    /**
     * data-source: contains the actual data
     */
    private JobDS ds = null;
    /**
     * ui panel used to edit filters
     */
    private FilterBuilder filterBuilder = null;
    /**
     * current job filtering criteria, or null
     */
    private AdvancedCriteria filter = null;

    private JobsController controller = null;

    /** To disable selection listener while fetching data */
    private boolean fetchingData;

    /**
     * @param controller Controller used to create this view
     */
    public JobsView(JobsController controller) {
        this.controller = controller;
        this.controller.getModel().addJobsUpdatedListener(this);
    }

    public void jobsUpdating() {
        this.jobsGrid.hide();
        this.jobsLoading.show();
    }

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
        transparentUpdate(JobsView.this.ds.getTestData().length + 1);
    }

    public void jobsUpdated(Map<Integer, Job> jobs) {
        boolean selectedJobRemoved = jobsGrid.getSelectedRecord() != null;
        List<Integer> selectedIds = listSelectedJobs();

        RecordList data = new RecordList();
        for (Job j : jobs.values()) {
            JobRecord jobRecord = new JobRecord(j);
            data.add(jobRecord);
            boolean isSelectedJob = selectedIds.contains(jobRecord.getAttributeAsInt(JobsColumns.ID_ATTR.getName()));
            jobRecord.setAttribute("isSelected", isSelectedJob);
            if (isSelectedJob) {
                selectedJobRemoved = false;
            }
        }

        if (selectedJobRemoved) {
            JobsView.this.controller.selectJob(null);
        }

        this.ds.setTestData(data.toArray());
        transparentUpdate(jobs.size() + 1);

        this.jobsLoading.hide();
        this.jobsGrid.show();
    }

    private List<Integer> listSelectedJobs() {
        List<Integer> selectedIds = new ArrayList<Integer>();
        for (ListGridRecord listGridRecord : jobsGrid.getSelectedRecords()) {
            selectedIds.add(listGridRecord.getAttributeAsInt(JobsColumns.ID_ATTR.getName()));
        }
        return selectedIds;
    }

    // as found in https://isomorphic.atlassian.net/wiki/display/Main/Refresh+ListGrid+Periodically+(Smart+GWT)#RefreshListGridPeriodically(SmartGWT)-Transparentupdate
    private void transparentUpdate(int nbOfJobs) {
        DataSource dataSource = jobsGrid.getDataSource();
        Integer[] visibleRows = jobsGrid.getVisibleRows();

        DSRequest request = new DSRequest();
        request.setStartRow(0);
        request.setEndRow(nbOfJobs + visibleRows[1]);
        request.setSortBy(jobsGrid.getSort());

        dataSource.fetchData(this.filter, new DSCallback() {
            @Override
            public void execute(DSResponse response, Object rawData, DSRequest request) {
                ListGridRecord[] keepSelectedRecords = jobsGrid.getSelectedRecords();
                jobsGrid.setData(new RecordList(response.getData()));
                // manual reset of selection (otherwise it is lost)
                if (keepSelectedRecords != null) {
                    fetchingData = true;
                    jobsGrid.selectRecords(keepSelectedRecords);
                    fetchingData = false;
                }
            }

        }, request);
    }

    private void applyFilter() {
        transparentUpdate(ds.getTestData().length);
    }

    /**
     * Construct and return the pane used to filter the job's datasource
     *
     * @return a widget for filtering the grid
     */
    public Layout buildFilterPane() {
        Label label = new Label("Use filters to restrict the number of jobs currently displayed.<br><br>"
            + "Filters apply only to the current page.<br>"
            + "Use The <strong>&lt;Previous</strong> and <strong>Next&gt;</strong> "
            + "controls to view more results.");
        label.setHeight(55);

        this.filterPane = new VLayout();
        this.filterPane.setBackgroundColor("#fafafa");
        this.filterPane.addMember(label);
        
        Layout gridFilterPane = new VLayout();
        gridFilterPane.setWidth100();
        gridFilterPane.setHeight100();

        filterBuilder = new FilterBuilder();
        filterBuilder.setDataSource(this.ds);
        filterBuilder.setTopOperatorAppearance(TopOperatorAppearance.RADIO);

        IButton ok = new IButton("Apply");
        ok.setHeight(20);
        ok.addClickHandler(new com.smartgwt.client.widgets.events.ClickHandler() {
            public void onClick(ClickEvent event) {
                filter = filterBuilder.getCriteria();
                applyFilter();
            }
        });
        IButton clear = new IButton("Clear");
        clear.setHeight(20);
        clear.addClickHandler(new com.smartgwt.client.widgets.events.ClickHandler() {
            public void onClick(ClickEvent event) {
                filterBuilder.clearCriteria();
                filter = filterBuilder.getCriteria();
                applyFilter();
            }
        });

        HLayout buttons = new HLayout();
        buttons.setWidth100();
        buttons.setAlign(Alignment.RIGHT);
        buttons.setHeight(20);
        buttons.setMargin(10);
        buttons.setMembersMargin(5);
        buttons.setMembers(clear, ok);

        gridFilterPane.addMember(filterBuilder);
        gridFilterPane.addMember(buttons);
        
        this.filterPane.setPadding(5);
        this.filterPane.setMembersMargin(10);
        this.filterPane.setOverflow(Overflow.AUTO);
        this.filterPane.addMember(gridFilterPane);
        this.filterPane.setBorder("1px solid #bfbfbf");
        this.filterPane.hide();

        return this.filterPane;
    }

    
    protected String getJobStatusFieldColor(ListGridRecord record, int rowNum, int colNum, String base){
        String fieldName = this.jobsGrid.getFieldName(colNum);

        /* change the color of the job status field  */
        if (fieldName.equals(JobsColumns.STATE_ATTR.getName())) {
            try {
                switch (JobStatus.valueOf(record.getAttribute(JobsColumns.STATE_ATTR.getName()).toUpperCase())) {
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
    
    
    protected void buildCellContextualMenu(Menu menu){
        boolean selPause = true; // ALL selected jobs are paused
        boolean selRunning = true; // ALL selected jobs are running/stalled/pending
        boolean selFinished = true; // ALL selected jobs are finished
        boolean selPauseOrRunning = true; // ALL selected jobs are running/pending/paused/stalled

        final ArrayList<String> ids = new ArrayList<String>(jobsGrid.getSelectedRecords().length);
        for (ListGridRecord rec : jobsGrid.getSelectedRecords()) {
            switch (JobStatus.valueOf(rec.getAttribute(JobsColumns.STATE_ATTR.getName()).toUpperCase())) {
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

            ids.add(rec.getAttribute(JobsColumns.ID_ATTR.getName()));
        }
        
        MenuItem pauseItem = new MenuItem("Pause", SchedulerImages.instance.scheduler_pause_16()
                .getSafeUri().asString());
        pauseItem.addClickHandler(new ClickHandler() {
            public void onClick(MenuItemClickEvent event) {
                JobsView.this.controller.pauseJobs(ids);
            }
        });
        pauseItem.setEnabled(selRunning);

        MenuItem resumeItem = new MenuItem("Resume", SchedulerImages.instance.scheduler_resume_16()
                .getSafeUri().asString());
        resumeItem.addClickHandler(new ClickHandler() {
            public void onClick(MenuItemClickEvent event) {
                JobsView.this.controller.resumeJobs(ids);
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
                        JobsView.this.controller.setJobPriority(ids, p);
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
                JobsView.this.controller.removeJob(ids);
            }
        });

        MenuItem killItem = new MenuItem("Kill", SchedulerImages.instance.scheduler_kill_16()
                .getSafeUri().asString());
        killItem.addClickHandler(new ClickHandler() {
            public void onClick(MenuItemClickEvent event) {
                JobsView.this.controller.killJob(ids);
            }
        });

        killItem.setEnabled(selPauseOrRunning);
        removeItem.setEnabled(selFinished);

        menu.setItems(pauseItem, resumeItem, priorityItem, removeItem, killItem);
    }
    
    
    protected EnumMap<JobsColumns, ListGridField> getColumnsForListGridField(){
        EnumMap<JobsColumns, ListGridField> columns = new EnumMap<JobsColumns, ListGridField>(JobsColumns.class);
        for(JobsColumns col: JobsColumns.values()){
           columns.put(col, null); 
        }
        return columns;
    }
    
    protected EnumMap<JobsColumns, ListGridField> buildListGridField(){
        EnumMap<JobsColumns, ListGridField> fields = super.<JobsColumns>buildListGridField();
        
        ListGridField idField = fields.get(JobsColumns.ID_ATTR);
        idField.setType(ListGridFieldType.INTEGER);
        idField.setAlign(Alignment.LEFT);
        idField.setCellAlign(Alignment.LEFT);

        ListGridField stateField = fields.get(JobsColumns.STATE_ATTR);
        stateField.setSortNormalizer(sortStatusAndGroup());

        ListGridField progressField = fields.get(JobsColumns.PROGRESS_ATTR);
        progressField.setType(ListGridFieldType.FLOAT);
        progressField.setAlign(Alignment.CENTER);
        progressField.setCellFormatter(new CellFormatter() {
            public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
                int pw = jobsGrid.getFieldWidth(JobsColumns.PROGRESS_ATTR.getName());
                float progress = Float.parseFloat(value.toString());
                int bx = new Double(Math.ceil(pw * progress)).intValue() - 601;
                String progressUrl = SchedulerImages.instance.progressbar().getSafeUri().asString();

                String style = "display:block; " + //
                    "border: 1px solid #acbac7; " + //
                    "background-image:url(" + progressUrl + ");" + //
                    "background-position:" + bx + "px 0px;" + //
                    "background-repeat: no-repeat;" + //
                    "background-color:#a7cef6";

                Job job = (Job) record.getAttributeAsObject(JOB_ATTR);
                String progressCounters = job.getFinishedTasks() + " / " + job.getTotalTasks();
                return "<div style='" + style + "'>" + progressCounters + "</div>";
            }
        });

        
        ListGridField duration = fields.get(JobsColumns.DURATION_ATTR);
        duration.setCellFormatter(new CellFormatter() {
            public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
                long l = Long.parseLong(value.toString());
                return Job.formatDuration(l);
            }
        });
        
        return fields;
    }
    
    
    /**
     * Builds and returns the jobs list grid
     *
     * <pre>
     * +- layout:HLayout -------+
     * |+ jobsGrid:ListGrid ---+|
     * ||    jobs details      ||
     * |+----------------------+|
     * +------------------------+
     * </pre>
     */
    public ListGrid buildGridPane() {
        this.jobsLoading = new Label("Fetching jobs...");
        this.jobsLoading.setWidth100();
        this.jobsLoading.setAlign(Alignment.CENTER);
        this.jobsLoading.setIcon("loading.gif");
        this.jobsLoading.hide();

        this.jobsGrid = new ListGrid() {
            @Override
            protected String getCellCSSText(ListGridRecord record, int rowNum, int colNum) {
                String base = super.getCellCSSText(record, rowNum, colNum);
                return getJobStatusFieldColor(record, rowNum, colNum, base);
            }
        };
        this.ds = new JobDS("jobds_" + LoginModel.getInstance().getSessionId());
        this.jobsGrid.setDataSource(this.ds);
        this.jobsGrid.setCanGroupBy(false);
        this.jobsGrid.setCanReorderFields(true);
        this.jobsGrid.setCanPickFields(true);
        this.jobsGrid.setCanFreezeFields(false);
        this.jobsGrid.setSelectionProperty("isSelected");
        this.jobsGrid.setEmptyMessage("No jobs to show. You can find workflows to submit in the samples/workflows folder where the Scheduler is installed.");
        this.jobsGrid.setAutoFetchData(true);
        this.jobsGrid.setShowSortNumerals(false);
        this.jobsGrid.setSort(DEFAULT_SORT);

        Collection<ListGridField> fieldsCollection = this.buildListGridField().values();
        ListGridField [] fields = new ListGridField[fieldsCollection.size()];
        this.jobsGrid.setFields(fieldsCollection.toArray(fields));

        this.jobsGrid.setWidth100();
        this.jobsGrid.setHeight100();


        // right click on an entry : popup a menu for job-contextual operations
        this.jobsGrid.addCellContextClickHandler(new CellContextClickHandler() {
            public void onCellContextClick(CellContextClickEvent event) {
                Menu menu = new Menu();
                menu.setShowShadow(true);
                menu.setShadowDepth(10);
                buildCellContextualMenu(menu);

                jobsGrid.setContextMenu(menu);
            }
        });

        this.jobsGrid.addCellOutHandler(new CellOutHandler() {
            public void onCellOut(CellOutEvent event) {
                // cancel contextual menu when moving the mouse away to
                // avoid keeping a menu on items we lost track of.
                // Not perfect but works in most cases
                jobsGrid.setContextMenu(null);
            }
        });

        jobsGrid.addSelectionChangedHandler(new SelectionChangedHandler() {
            public void onSelectionChanged(SelectionEvent event) {
                if (event.getState() && !fetchingData) {
                    Record record = event.getRecord();
                    String attName = JobsColumns.ID_ATTR.getName();
                    JobsView.this.controller.selectJob(Integer.toString(record.getAttributeAsInt(attName)));
                }
            }
        });
        
        return this.jobsGrid;
    }
    
    
    protected Layout buildToolBar(){
        ToolStrip toolbar = new ToolStrip();
        toolbar.addStyleName("itemViewNav");
        toolbar.setHeight(34);
        toolbar.setWidth100();
        toolbar.setBackgroundImage("");
        toolbar.setBackgroundColor("#fafafa");
        toolbar.setBorder("0px");
        
        return toolbar;
    }
    
    public Layout build(){
        VLayout layout = new VLayout();
        
        Layout toolBarLayout = this.buildToolBar();
        Layout contentLayout = this.buildContent();
        Layout paginationLayout = this.controller.getPaginationController().buildView();
        
        layout.addMember(toolBarLayout);
        layout.addMember(contentLayout);
        layout.addMember(paginationLayout);
        return layout;
    }
    
    
    protected void toggleFilterPane(){
        if (!filterPane.isVisible()) {
            filterPane.setWidth(490);
            filterButtonLabel.setSrc(SchedulerImages.instance.section_right_10().getSafeUri()
                    .asString());
            contentPane.showMember(filterPane);
        } else {
            filterButtonLabel.setSrc(SchedulerImages.instance.section_left_10().getSafeUri()
                    .asString());
            contentPane.hideMember(filterPane);
        }
    }
    
    
    protected Layout buildFilterButton(){
        final VLayout filterButton = new VLayout();
        filterButton.setBackgroundColor("#bfbfbf");
        filterButton.setAlign(VerticalAlignment.CENTER);
        filterButton.setWidth(12);
        filterButton.setHeight100();
        filterButton.setBorder("1px solid #bfbfbf");
        filterButtonLabel = new Img(SchedulerImages.instance.section_left_10().getSafeUri()
                .asString(), 10, 13);
        filterButton.addMember(filterButtonLabel);
        filterButton.addClickHandler(new com.smartgwt.client.widgets.events.ClickHandler() {
            @Override
            public void onClick(ClickEvent event) {
                toggleFilterPane();
            }
        });
        filterButton.addMouseOverHandler(new MouseOverHandler() {
            public void onMouseOver(MouseOverEvent event) {
                filterButton.setBackgroundColor("#eee");
            }
        });
        filterButton.addMouseOutHandler(new MouseOutHandler() {
            public void onMouseOut(MouseOutEvent event) {
                filterButton.setBackgroundColor("#bfbfbf");
            }
        });
        return filterButton;
    }
    
    /**
     * Builds and return the top pane: the jobs list and filtering options
     *
     * <pre>
     * +- HLayout -----------------------------------------------+
     * |+- ListGrid -----------++--++- VLayout -----------------+|
     * || JobsView#build()     ||  || JobsView#buildFilterPane()||
     * ||                      ||>>|| hidden/shown upon click   ||
     * ||                      ||  || on the '>>' canvas        ||
     * |+----------------------++--++---------------------------+|
     * +---------------------------------------------------------+
     * </pre>
     */
    public Layout buildContent() {
        this.buildGridPane();

        HLayout contentGridLayout = new HLayout();
        contentGridLayout.addMember(jobsGrid);
        contentGridLayout.addMember(jobsLoading);
        
        Layout filterButton = this.buildFilterButton();
        Layout filterPane = this.buildFilterPane();
        
        contentPane = new HLayout();
        contentPane.setMembers(contentGridLayout, filterButton, filterPane);

        return contentPane;
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
}
