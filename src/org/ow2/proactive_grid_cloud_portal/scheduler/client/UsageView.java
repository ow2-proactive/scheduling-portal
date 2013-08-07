/*
 *  *
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
 *  * $$PROACTIVE_INITIAL_DEV$$
 */
package org.ow2.proactive_grid_cloud_portal.scheduler.client;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.ow2.proactive_grid_cloud_portal.common.client.JSUtil;
import com.google.gwt.core.client.GWT;
import com.google.gwt.http.client.URL;
import com.google.gwt.i18n.client.DateTimeFormat;
import com.google.gwt.user.client.Window;
import com.google.gwt.visualization.client.AbstractDataTable;
import com.google.gwt.visualization.client.DataTable;
import com.google.gwt.visualization.client.LegendPosition;
import com.google.gwt.visualization.client.VisualizationUtils;
import com.google.gwt.visualization.client.visualizations.corechart.AxisOptions;
import com.google.gwt.visualization.client.visualizations.corechart.ColumnChart;
import com.google.gwt.visualization.client.visualizations.corechart.CoreChart;
import com.google.gwt.visualization.client.visualizations.corechart.HorizontalAxisOptions;
import com.google.gwt.visualization.client.visualizations.corechart.Options;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.RelativeDate;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.AutoFitWidthApproach;
import com.smartgwt.client.types.FormLayoutType;
import com.smartgwt.client.types.GroupStartOpen;
import com.smartgwt.client.types.SummaryFunctionType;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.RelativeDateItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.grid.CellFormatter;
import com.smartgwt.client.widgets.grid.GroupNode;
import com.smartgwt.client.widgets.grid.GroupTitleRenderer;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.SummaryFunction;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.LayoutSpacer;
import com.smartgwt.client.widgets.layout.VLayout;

public class UsageView implements SchedulerListeners.UsageListener {

    private static final String ISO_8601_FORMAT = "yyyy-MM-dd'T'HH:mm:ss.SSSZ";
    private static final DateTimeFormat DATE_FORMAT = DateTimeFormat.getFormat(ISO_8601_FORMAT);
    // The RelativeDate#START_OF_MONTH has not the behavior expected
    private static final RelativeDate START_OF_MONTH = new RelativeDate("+1m[-1m]");

    private static final RelativeDate DEFAULT_START_DATE = START_OF_MONTH;
    private static final RelativeDate DEFAULT_END_DATE = RelativeDate.NOW;

    private static final String BLUE = "#3a668d";
    private static final String GREEN = "#35a849";

    private static final int CHART_HEIGHT = 100;

    private SchedulerController controller;

    private ListGrid detailsGrid;
    private DataTable counterData;
    private ColumnChart counterChart;

    private Options counterChartOptions;
    private DataTable durationData;
    private ColumnChart durationChart;
    private Options durationChartOptions;
    private DynamicForm datesForm;
    private SelectItem userSelect;
    /** In case data are received before charts are displayed, save them */
    private ChartData currentChartData;

    public UsageView(SchedulerController controller) {
        this.controller = controller;
        controller.getEventDispatcher().addUsageListener(this);
    }

    public Layout build() {
        final VLayout root = new VLayout();
        root.setWidth100();
        root.setHeight100();
        // for details grid to be shown even if empty
        root.setMinMemberSize(200);

        DynamicForm dateInputs = createDateInputs();
        HLayout detailsLabelAndExportButton = createDetailsLabelAndExportButton();
        ListGrid details = createDetailsGrid();

        root.addMember(dateInputs);
        root.addMember(detailsLabelAndExportButton);
        root.addMember(details);

        VisualizationUtils.loadVisualizationApi(new Runnable() {
            @Override
            public void run() {
                Label summaryLabel = new Label("<h3>Summary</h3>");
                summaryLabel.setHeight(20);
                VLayout charts = createCharts(root);
                root.addMember(summaryLabel, 1);
                root.addMember(charts, 2);
                updateCharts();
            }
        }, CoreChart.PACKAGE);

        Date from = RelativeDateItem.getAbsoluteDate(DEFAULT_START_DATE);
        Date to = RelativeDateItem.getAbsoluteDate(DEFAULT_END_DATE);
        controller.getUsage(null, from, to);
        controller.getUsersWithJobs();

        return root;
    }

    private DynamicForm createDateInputs() {
        datesForm = new DynamicForm();
        datesForm.setWidth100();
        datesForm.setTitleOrientation(TitleOrientation.LEFT);
        datesForm.setItemLayout(FormLayoutType.TABLE);
        datesForm.setLayoutAlign(Alignment.LEFT);
        datesForm.setNumCols(3);
        datesForm.setColWidths("80", "200", "*");
        datesForm.setWrapItemTitles(false);

        userSelect = new SelectItem("User");
        userSelect.disable();
        userSelect.setValue(controller.getModel().getLogin());
        userSelect.setAlign(Alignment.LEFT);
        userSelect.addChangedHandler(new ChangedHandler() {
            public void onChanged(ChangedEvent event) {
                Date from = readDateFromFormItem(event.getForm().getItem("From"));
                Date to = readDateFromFormItem(event.getForm().getItem("To"));
                refreshUsage(from, to);
            }
        });

        controller.getEventDispatcher().addUsersWithJobsListener(new SchedulerListeners.UsersListener() {
            @Override
            public void usersUpdated(List<SchedulerUser> users) {
                ArrayList<String> formatted = new ArrayList<String>(users.size());
                for (SchedulerUser user : users) {
                    formatted.add(user.getUsername());
                }
                if (formatted.size() == 1 && formatted.get(0).equals(controller.getModel().getLogin())) {
                    // only one user available and it is the current user, disable combo
                    userSelect.disable();
                } else {
                    userSelect.enable();
                }

                if (userSelect.getValue().equals(controller.getModel().getLogin())
                        && !formatted.contains(controller.getModel().getLogin())) {
                    // remove default value (user login) as he has not yet submitted jobs
                    userSelect.clearValue();
                }

                userSelect.setValueMap(formatted.toArray(new String[] { }));
            }
        });

        RelativeDateItem fromDate = new RelativeDateItem("From", "Usage From");
        fromDate.setValue(DEFAULT_START_DATE);
        fromDate.setAlign(Alignment.LEFT);
        fromDate.setShowFutureOptions(false);

        RelativeDateItem toDate = new RelativeDateItem("To");
        toDate.setValue(DEFAULT_END_DATE);
        toDate.setAlign(Alignment.LEFT);
        toDate.setShowFutureOptions(false);

        ButtonItem button = new ButtonItem("Refresh");
        button.setAutoFit(true);
        button.setStartRow(false);
        button.setAlign(Alignment.RIGHT);
        button.addClickHandler(refreshAfterDateSelection());

        datesForm.setItems(userSelect, button, fromDate, toDate);

        return datesForm;
    }

    private void refreshUsage(Date from, Date to) {
        clearDetailsGrid();
        clearCharts();
        displayDetailsGridLoadingMessage();
        String userName = userSelect.isDisabled() ? null : userSelect.getValue().toString();
        controller.getUsage(userName, from, to);
        controller.getUsersWithJobs();
    }

    private com.smartgwt.client.widgets.form.fields.events.ClickHandler refreshAfterDateSelection() {
        return new com.smartgwt.client.widgets.form.fields.events.ClickHandler() {
            @Override
            public void onClick(com.smartgwt.client.widgets.form.fields.events.ClickEvent event) {
                Date from = readDateFromFormItem(event.getForm().getItem("From"));
                Date to = readDateFromFormItem(event.getForm().getItem("To"));
                refreshUsage(from, to);
            }
        };
    }

    private HLayout createDetailsLabelAndExportButton() {
        HLayout layout = new HLayout();
        layout.setDefaultLayoutAlign(VerticalAlignment.CENTER);
        layout.setHeight(30);

        Label detailsLabel = new Label("<h3>Details</h3>");
        detailsLabel.setHeight(20);
        layout.addMember(detailsLabel);

        layout.addMember(new LayoutSpacer());

        IButton export = new IButton("Export");
        export.setAutoFit(true);
        export.addClickHandler(downloadUsageData());
        layout.addMember(export);

        LayoutSpacer toAlignWithRefresh = new LayoutSpacer();
        toAlignWithRefresh.setWidth(2);
        layout.addMember(toAlignWithRefresh);
        return layout;
    }

    private ClickHandler downloadUsageData() {
        return new ClickHandler() {
            @Override
            public void onClick(ClickEvent event) {
                String from = DATE_FORMAT.format(readDateFromFormItem(datesForm.getItem("From")));
                String to = DATE_FORMAT.format(readDateFromFormItem(datesForm.getItem("To")));

                String url = GWT.getModuleBaseURL() + "usageexporter";
                url += "?sessionId=" + controller.getModel().getSessionId();
                url += "&user=" + userSelect.getValue().toString();
                url += "&startDate=" + URL.encodeQueryString(from);
                url += "&endDate=" + URL.encodeQueryString(to);
                Window.open(url, "_blank", "");
            }
        };
    }

    private ListGrid createDetailsGrid() {
        detailsGrid = new ListGrid();
        detailsGrid.setWidth100();

        ListGridField jobField = new ListGridField("jobId", "Job");
        jobField.setHidden(true);
        jobField.setCellFormatter(new CellFormatter() {
            @Override
            public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
                return record.getAttribute("jobName");
            }
        });
        jobField.setGroupTitleRenderer(new GroupTitleRenderer() {
            @Override
            public String getGroupTitle(Object groupValue, GroupNode groupNode, ListGridField field, String fieldName, ListGrid grid) {
                return groupValue + " - " + groupNode.getGroupMembers()[0].getAttribute("jobName");
            }
        });
        jobField.setShowGridSummary(true);
        jobField.setShowGroupSummary(false);

        ListGridField taskField = new ListGridField("taskId", "Task");
        taskField.setCellFormatter(new CellFormatter() {
            @Override
            public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
                if (record.getIsGridSummary() || record.getIsGroupSummary()) {
                    return value + " tasks";
                }
                return record.getAttribute("taskName");
            }
        });
        taskField.setWidth(150);
        taskField.setShowGridSummary(true);
        taskField.setShowGroupSummary(true);
        taskField.setSummaryFunction(new SummaryFunction() {
            @Override
            public Object getSummaryValue(Record[] records, ListGridField field) {
                return records.length;
            }
        });

        ListGridField nbNodesField = new ListGridField("nbNodes", "# Nodes");
        nbNodesField.setWidth(50);
        nbNodesField.setCanGroupBy(false);
        nbNodesField.setPrompt("The number of nodes used to execute this task");
        nbNodesField.setShowGridSummary(false);
        nbNodesField.setShowGroupSummary(false);

        ListGridField startTimeField = new ListGridField("startTime", "Started at");
        startTimeField.setCanGroupBy(false);
        startTimeField.setCellFormatter(new CellFormatter() {
            @Override
            public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
                return JSUtil.getTime(record.getAttributeAsLong("startTime"));
            }
        });
        startTimeField.setShowGridSummary(false);
        startTimeField.setShowGroupSummary(false);

        ListGridField finishedTimeField = new ListGridField("finishedTime", "Finished at");
        finishedTimeField.setCanGroupBy(false);
        finishedTimeField.setCellFormatter(new CellFormatter() {
            @Override
            public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
                return JSUtil.getTime(record.getAttributeAsLong("finishedTime"));
            }
        });
        finishedTimeField.setShowGridSummary(false);
        finishedTimeField.setShowGroupSummary(false);

        ListGridField durationField = new ListGridField("duration", "Duration");
        durationField.setCanGroupBy(false);
        durationField.setCellFormatter(new CellFormatter() {
            @Override
            public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
                return Job.formatDuration(record.getAttributeAsLong("duration"));
            }
        });
        durationField.setWidth(80);
        durationField.setPrompt("Execution duration");
        durationField.setShowGridSummary(true);
        durationField.setShowGroupSummary(true);
        durationField.setSummaryFunction(SummaryFunctionType.SUM);

        detailsGrid.setGroupByField("jobId");
        detailsGrid.setGroupStartOpen(GroupStartOpen.ALL);
        detailsGrid.setShowGridSummary(true);
        detailsGrid.setShowGroupSummary(true);
        detailsGrid.setAutoFitWidthApproach(AutoFitWidthApproach.BOTH);
        detailsGrid.setFields(jobField, taskField, nbNodesField, startTimeField, finishedTimeField, durationField);
        displayDetailsGridLoadingMessage();
        return detailsGrid;
    }

    private VLayout createCharts(final VLayout root) {
        VLayout charts = new VLayout();
        charts.setWidth(root.getWidth() - 30);
        charts.setHeight(CHART_HEIGHT);

        ColumnChart counter = createCounterChart();
        charts.addMember(counter);

        ColumnChart timer = createDurationChart();
        charts.addMember(timer);

        return charts;
    }

    private ColumnChart createDurationChart() {
        durationChartOptions = createChartOptions();
        durationChartOptions.setTitle("Total Duration in ms");

        durationData = DataTable.create();
        durationData.addColumn(AbstractDataTable.ColumnType.STRING, "Label");
        durationData.addColumn(AbstractDataTable.ColumnType.NUMBER, "Duration");
        durationData.addColumn(AbstractDataTable.ColumnType.NUMBER, "Duration");

        durationChart = new ColumnChart(durationData, durationChartOptions);
        durationChart.setTitle("Duration");
        return durationChart;
    }

    private ColumnChart createCounterChart() {
        counterChartOptions = createChartOptions();
        counterChartOptions.setTitle("Execution count");

        counterData = DataTable.create();
        counterData.addColumn(AbstractDataTable.ColumnType.STRING, "Label");
        counterData.addColumn(AbstractDataTable.ColumnType.NUMBER, "Counter");
        counterData.addColumn(AbstractDataTable.ColumnType.NUMBER, "Counter");

        counterChart = new ColumnChart(counterData, counterChartOptions);
        counterChart.setTitle("Count");
        return counterChart;
    }

    private Options createChartOptions() {
        Options nodeLineOpts = Options.create();
        HorizontalAxisOptions axisOpts = HorizontalAxisOptions.create();
        nodeLineOpts.setLegend(LegendPosition.NONE);
        nodeLineOpts.setHAxisOptions(axisOpts);
        AxisOptions options = AxisOptions.create();
        nodeLineOpts.setVAxisOptions(options);
        nodeLineOpts.setHeight(CHART_HEIGHT);
        nodeLineOpts.setLineWidth(0);
        nodeLineOpts.setColors(BLUE, GREEN);
        nodeLineOpts.setIsStacked(true);
        return nodeLineOpts;
    }

    @Override
    public void usageUpdated(List<JobUsage> jobUsages) {
        int taskCounter = 0;
        int jobCounter = 0;
        long jobTotalDuration = 0;
        long taskTotalDuration = 0;

        List<ListGridRecord> records = new ArrayList<ListGridRecord>();
        for (JobUsage jobUsage : jobUsages) {
            jobCounter++;
            jobTotalDuration += jobUsage.getJobDuration();
            for (TaskUsage taskUsage : jobUsage.getTaskUsages()) {
                taskCounter++;
                taskTotalDuration += taskUsage.getTaskExecutionDuration();
                ListGridRecord listGridRecord = createGridRecord(jobUsage, taskUsage);
                records.add(listGridRecord);
            }
        }

        detailsGrid.setData(records.toArray(new ListGridRecord[records.size()]));
        detailsGrid.setEmptyMessage("No data for this period.");

        currentChartData = new ChartData(taskCounter, jobCounter, jobTotalDuration, taskTotalDuration);
        updateCharts();

        detailsGrid.recalculateSummaries();
    }

    private void updateCharts() {
        if (currentChartData != null) {
            updateCounterChart(currentChartData.taskCounter, currentChartData.jobCounter);
            updateDurationChart(currentChartData.jobTotalDuration, currentChartData.taskTotalDuration);
        }
    }

    private ListGridRecord createGridRecord(JobUsage jobUsage, TaskUsage taskUsage) {
        ListGridRecord listGridRecord = new ListGridRecord();
        listGridRecord.setAttribute("taskId", taskUsage.getTaskId());
        listGridRecord.setAttribute("taskName", taskUsage.getTaskName());
        listGridRecord.setAttribute("jobName", jobUsage.getJobName());
        listGridRecord.setAttribute("jobId", jobUsage.getJobId());
        listGridRecord.setAttribute("nbNodes", taskUsage.getTaskNodeNumber());
        listGridRecord.setAttribute("duration", taskUsage.getTaskExecutionDuration());
        listGridRecord.setAttribute("startTime", taskUsage.getTaskStartTime());
        listGridRecord.setAttribute("finishedTime", taskUsage.getTaskFinishedTime());
        return listGridRecord;
    }

    private void updateDurationChart(long jobTotalDuration, long taskTotalDuration) {
        if (durationChart != null && durationData != null) { // offline mode, charts not displayed
            durationData.removeRows(0, durationData.getNumberOfRows());
            durationData.addRows(2);
            durationData.setValue(0, 0, "Jobs");
            durationData.setValue(0, 1, scale(jobTotalDuration, taskTotalDuration));
            durationData.setFormattedValue(0, 1, Job.formatDuration(jobTotalDuration));
            durationData.setValue(1, 0, "Tasks");
            durationData.setValue(1, 2, scale(taskTotalDuration, jobTotalDuration));
            durationData.setFormattedValue(1, 2, Job.formatDuration(taskTotalDuration));
            durationChart.draw(durationData, durationChartOptions);
        }
    }

    // divide the value in the chart to be seconds, or minutes, or ... for better y-axis display
    private double scale(long valueToDisplay, long otherValueInChart) {
        long scaleMax = Math.max(valueToDisplay, otherValueInChart);
        if (scaleMax < 1000) {
            durationChartOptions.setTitle("Total Duration in milliseconds");
            return valueToDisplay;
        } else if (scaleMax < 1000 * 60) {
            durationChartOptions.setTitle("Total Duration in seconds");
            return valueToDisplay / 1000.0;
        } else if (scaleMax < 1000 * 60 * 60) {
            durationChartOptions.setTitle("Total Duration in minutes");
            return valueToDisplay / (1000.0 * 60);
        } else {
            durationChartOptions.setTitle("Total Duration in hours");
            return valueToDisplay / (1000.0 * 60 * 60);
        }
    }

    private void updateCounterChart(int taskCounter, int jobCounter) {
        if (counterChart != null && counterData != null) { // offline mode, charts not displayed
            counterData.removeRows(0, counterData.getNumberOfRows());
            counterData.addRows(2);
            counterData.setValue(0, 0, "Jobs");
            counterData.setValue(0, 1, jobCounter);
            counterData.setValue(1, 0, "Tasks");
            counterData.setValue(1, 2, taskCounter);
            counterChart.draw(counterData, counterChartOptions);
        }
    }

    private Date readDateFromFormItem(FormItem formItem) {
        Object formItemValue = formItem.getValue();
        if (formItemValue instanceof Date) {
            return (Date) formItemValue;
        }
        return RelativeDateItem.getAbsoluteDate(new RelativeDate(formItem.getValue().toString()));
    }

    private void displayDetailsGridLoadingMessage() {
        detailsGrid.setEmptyMessage("Loading usage data...");
    }

    private void clearDetailsGrid() {
        detailsGrid.setData(new ListGridRecord[]{});
    }

    private void clearCharts() {
        if (counterData != null && counterChart != null
                && durationChart != null && durationData != null) { // offline mode, charts not displayed
            counterData.removeRows(0, counterData.getNumberOfRows());
            counterChart.draw(counterData, counterChartOptions);

            durationData.removeRows(0, durationData.getNumberOfRows());
            durationChart.draw(durationData, durationChartOptions);
        }
    }

    private class ChartData {
        private final int taskCounter;
        private final int jobCounter;
        private final long jobTotalDuration;
        private final long taskTotalDuration;

        public ChartData(int taskCounter, int jobCounter, long jobTotalDuration, long taskTotalDuration) {
            this.taskCounter = taskCounter;
            this.jobCounter = jobCounter;
            this.jobTotalDuration = jobTotalDuration;
            this.taskTotalDuration = taskTotalDuration;
        }
    }
}
