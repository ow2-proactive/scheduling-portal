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

import org.ow2.proactive_grid_cloud_portal.common.client.JSUtil;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import com.google.gwt.core.client.GWT;
import com.google.gwt.http.client.URL;
import com.google.gwt.i18n.client.DateTimeFormat;
import com.google.gwt.user.client.Window;
import com.google.gwt.user.client.ui.HorizontalPanel;
import com.google.gwt.visualization.client.AbstractDataTable;
import com.google.gwt.visualization.client.DataTable;
import com.google.gwt.visualization.client.LegendPosition;
import com.google.gwt.visualization.client.visualizations.corechart.AxisOptions;
import com.google.gwt.visualization.client.visualizations.corechart.ColumnChart;
import com.google.gwt.visualization.client.visualizations.corechart.HorizontalAxisOptions;
import com.google.gwt.visualization.client.visualizations.corechart.Options;
import com.smartgwt.client.data.RelativeDate;
import com.smartgwt.client.types.AutoFitWidthApproach;
import com.smartgwt.client.types.FormLayoutType;
import com.smartgwt.client.types.GroupStartOpen;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.RelativeDateItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.grid.CellFormatter;
import com.smartgwt.client.widgets.grid.GroupNode;
import com.smartgwt.client.widgets.grid.GroupTitleRenderer;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.LayoutSpacer;
import com.smartgwt.client.widgets.layout.VLayout;

public class UsageView implements SchedulerListeners.UsageListener {

    private static final String ISO_8601_FORMAT = "yyyy-MM-dd'T'HH:mm:ss.SSSZ";
    private static final DateTimeFormat DATE_FORMAT = DateTimeFormat.getFormat(ISO_8601_FORMAT);

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

    public UsageView(SchedulerController controller) {
        this.controller = controller;
        controller.getEventDispatcher().addUsageListener(this);
    }

    public Layout build() {
        VLayout root = new VLayout();
        root.setWidth100();
        root.setHeight100();

        DynamicForm dateInputs = createDateInputs();
        Label summaryLabel = new Label("<h3>Summary</h3>");
        summaryLabel.setHeight(20);
        HorizontalPanel charts = createCharts();
        Label detailsLabel = new Label("<h3>Details</h3>");
        detailsLabel.setHeight(20);
        ListGrid details = createDetailsGrid();

        root.addMember(dateInputs);
        root.addMember(summaryLabel);
        root.addMember(charts);
        HLayout layout = new HLayout();
        layout.setDefaultLayoutAlign(VerticalAlignment.CENTER);
        layout.setHeight(30);
        layout.addMember(detailsLabel);
        IButton export = new IButton("Export");
        export.setAutoFit(true);
        export.addClickHandler(downloadUsageData());
        layout.addMember(new LayoutSpacer());
        layout.addMember(export);
        root.addMember(layout);
        root.addMember(details);

        Date from = RelativeDateItem.getAbsoluteDate(RelativeDate.START_OF_MONTH);
        Date to = RelativeDateItem.getAbsoluteDate(RelativeDate.NOW);
        controller.getUsage(from, to);

        return root;
    }

    private ClickHandler downloadUsageData() {
        return new ClickHandler() {
            @Override
            public void onClick(ClickEvent event) {
                String url = GWT.getModuleBaseURL() + "usageexporter";
                url += "?sessionId=" + controller.getModel().getSessionId();
                String date = DATE_FORMAT.format(new Date()); // TODO get proper dates
                url += "&startDate=" + URL.encodeQueryString(date);
                url += "&endDate=" + URL.encodeQueryString(date);
                Window.open(url, "_blank", "");
            }
        };
    }

    private DynamicForm createDateInputs() {
        DynamicForm form = new DynamicForm();
        form.setWidth100();
        form.setTitleOrientation(TitleOrientation.LEFT);
        form.setItemLayout(FormLayoutType.TABLE);
        form.setNumCols(8);
        form.setWrapItemTitles(false);

        RelativeDateItem fromDate = new RelativeDateItem("From", "Usage From");
        fromDate.setOperator(OperatorId.GREATER_OR_EQUAL);
        fromDate.setValue(RelativeDate.START_OF_MONTH);

        RelativeDateItem toDate = new RelativeDateItem("To");
        toDate.setOperator(OperatorId.LESS_OR_EQUAL);
        toDate.setValue(RelativeDate.NOW);

        form.setItems(fromDate, toDate);

        fromDate.addChangedHandler(getHandler());
        toDate.addChangedHandler(getHandler());

        return form;
    }

    private ChangedHandler getHandler() {
        return new ChangedHandler() {
            @Override
            public void onChanged(ChangedEvent event) {
                Date from = RelativeDateItem.getAbsoluteDate(((RelativeDateItem) event.getForm().getItem("From")).getRelativeDate());
                Date to = RelativeDateItem.getAbsoluteDate(((RelativeDateItem) event.getForm().getItem("To")).getRelativeDate());
                controller.getUsage(from, to);
                clearDetailsGrid();
                displayDetailsGridLoadingMessage();
            }
        };
    }

    private void clearDetailsGrid() {
        detailsGrid.setData(new ListGridRecord[]{});
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

        ListGridField taskField = new ListGridField("taskId", "Task");
        taskField.setCellFormatter(new CellFormatter() {
            @Override
            public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
                return record.getAttribute("taskName");
            }
        });
        taskField.setWidth(150);

        ListGridField nbNodesField = new ListGridField("nbNodes", "# Nodes");
        nbNodesField.setWidth(50);
        nbNodesField.setCanGroupBy(false);
        nbNodesField.setPrompt("The number of nodes used to execute this task");

        ListGridField startTimeField = new ListGridField("startTime", "Started at");
        startTimeField.setCanGroupBy(false);
        startTimeField.setCellFormatter(new CellFormatter() {
            @Override
            public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
                return JSUtil.getTime(record.getAttributeAsLong("startTime"));
            }
        });

        ListGridField finishedTimeField = new ListGridField("finishedTime", "Finished at");
        finishedTimeField.setCanGroupBy(false);
        finishedTimeField.setCellFormatter(new CellFormatter() {
            @Override
            public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
                return JSUtil.getTime(record.getAttributeAsLong("finishedTime"));
            }
        });

        ListGridField durationField = new ListGridField("duration", "Duration");
        durationField.setCanGroupBy(false);
        durationField.setCellFormatter(new CellFormatter() {
            @Override
            public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
                return Job.formatDuration(record.getAttributeAsLong("duration"));
            }
        });
        durationField.setWidth(80);

        detailsGrid.setGroupByField("jobId");
        detailsGrid.setGroupStartOpen(GroupStartOpen.ALL);
        detailsGrid.setAutoFitWidthApproach(AutoFitWidthApproach.BOTH);
        detailsGrid.setFields(jobField, taskField, nbNodesField, startTimeField, finishedTimeField, durationField);
        displayDetailsGridLoadingMessage();
        return detailsGrid;
    }

    private void displayDetailsGridLoadingMessage() {
        detailsGrid.setEmptyMessage("Loading usage data...");
    }

    private HorizontalPanel createCharts() {
        HorizontalPanel charts = new HorizontalPanel();
        charts.setWidth("100%");
        charts.setHeight(CHART_HEIGHT + "px");

        ColumnChart counter = createCounterChart();
        charts.add(counter);
        charts.setCellWidth(counter, "50%");

        ColumnChart timer = createDurationChart();
        charts.add(timer);
        charts.setCellWidth(timer, "50%");
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
        axisOpts.setMaxAlternation(1);
        nodeLineOpts.setHAxisOptions(axisOpts);
        AxisOptions options = AxisOptions.create();
        options.set("format", "");
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

        updateCounterChart(taskCounter, jobCounter);
        updateDurationChart(jobTotalDuration, taskTotalDuration);
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
        durationData.removeRows(0, durationData.getNumberOfRows());
        durationData.addRows(2);
        durationData.setValue(0, 0, "Jobs");
        durationData.setValue(0, 1, jobTotalDuration);
        durationData.setFormattedValue(0, 1, Job.formatDuration(jobTotalDuration));
        durationData.setValue(1, 0, "Tasks");
        durationData.setValue(1, 2, taskTotalDuration);
        durationData.setFormattedValue(1, 2, Job.formatDuration(taskTotalDuration));
        durationChart.draw(durationData, durationChartOptions);
    }

    private void updateCounterChart(int taskCounter, int jobCounter) {
        counterData.removeRows(0, counterData.getNumberOfRows());
        counterData.addRows(2);
        counterData.setValue(0, 0, "Jobs");
        counterData.setValue(0, 1, jobCounter);
        counterData.setValue(1, 0, "Tasks");
        counterData.setValue(1, 2, taskCounter);
        counterChart.draw(counterData, counterChartOptions);
    }
}
