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
package org.ow2.proactive_grid_cloud_portal.rm.client;

import java.util.Date;
import java.util.LinkedHashMap;
import java.util.Map;

import org.ow2.proactive_grid_cloud_portal.common.client.Listeners.StatsListener;
import org.ow2.proactive_grid_cloud_portal.common.client.Model.StatHistory;
import org.ow2.proactive_grid_cloud_portal.common.client.Model.StatHistory.Range;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMListeners.NodesListener;

import com.google.gwt.i18n.client.DateTimeFormat;
import com.google.gwt.i18n.client.DateTimeFormat.PredefinedFormat;
import com.google.gwt.user.client.ui.AbsolutePanel;
import com.google.gwt.visualization.client.AbstractDataTable.ColumnType;
import com.google.gwt.visualization.client.DataTable;
import com.google.gwt.visualization.client.LegendPosition;
import com.google.gwt.visualization.client.visualizations.corechart.AreaChart;
import com.google.gwt.visualization.client.visualizations.corechart.ColumnChart;
import com.google.gwt.visualization.client.visualizations.corechart.HorizontalAxisOptions;
import com.google.gwt.visualization.client.visualizations.corechart.Options;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;


/**
 * Displays monitoring graphs using Google Visualization
 * 
 * 
 * @author mschnoor
 *
 */
public class RMStatsView implements StatsListener, NodesListener {

    private RMController controller;

    private AreaChart nodeLineChart;

    private DataTable nodeLineTable;

    private Options nodeLineOpts;

    private int nodeLineTimeId, nodeLineFreeId, nodeLineBusyId, nodeLineDownId, nodeLineTotalId, linePendingTasksId,
            nodeLineDeployingId, nodeLineConfiguringId, nodeLineLostId;

    private DynamicForm nodeLineForm;

    private Label nodeLineHeaderLabel;

    private DynamicForm nodeLineSeriesForm;

    private ColumnChart nodeColChart;

    private DataTable nodeColTable;

    private Options nodeColOpts;

    private Label nodeColHeaderLabel;

    private AreaChart loadChart;

    private DataTable loadTable;

    private Options loadOpts;

    private int loadTimeId, loadValId;

    private DynamicForm loadForm;

    private Label loadHeaderLabel;

    RMStatsView(RMController controller) {
        this.controller = controller;
        this.controller.getEventDispatcher().addStatsListener(this);
        this.controller.getEventDispatcher().addNodesListener(this);
    }

    Canvas build() {
        VLayout root = new VLayout();
        root.setBackgroundColor("white");
        root.setPadding(5);
        root.setWidth100();
        root.setHeight100();
        root.setOverflow(Overflow.AUTO);

        /*
         * Node history graph
         */
        final AbsolutePanel nodeLinePane = new AbsolutePanel();
        nodeLinePane.setWidth("100%");
        nodeLinePane.setHeight("150px");

        nodeLineOpts = Options.create();
        // no specified width : will fill available space on chart.draw()
        //nodeLineOpts.setWidth(400);
        HorizontalAxisOptions axisOpts = HorizontalAxisOptions.create();
        axisOpts.setMaxAlternation(1);
        axisOpts.setSlantedText(false);
        //axisOpts.setTitle("Time");
        nodeLineOpts.setHAxisOptions(axisOpts);
        nodeLineOpts.setHeight(150);
        nodeLineOpts.setLegend(LegendPosition.NONE);
        nodeLineOpts.setColors("#3a668d", "#35a849", "#ffff00", "#fcaf3e", "#24c1ff", "#1e4ed7", "#ef2929", "#000000");

        nodeLineTable = DataTable.create();
        nodeLineTimeId = nodeLineTable.addColumn(ColumnType.STRING, "Time");

        nodeLineTotalId = nodeLineTable.addColumn(ColumnType.NUMBER, "Total");
        nodeLineFreeId = nodeLineTable.addColumn(ColumnType.NUMBER, "Free");
        linePendingTasksId = nodeLineTable.addColumn(ColumnType.NUMBER, "Needed");
        nodeLineBusyId = nodeLineTable.addColumn(ColumnType.NUMBER, "Busy");
        nodeLineDeployingId = nodeLineTable.addColumn(ColumnType.NUMBER, "Deploying");
        nodeLineConfiguringId = nodeLineTable.addColumn(ColumnType.NUMBER, "Configuting");
        nodeLineDownId = nodeLineTable.addColumn(ColumnType.NUMBER, "Down");
        nodeLineLostId = nodeLineTable.addColumn(ColumnType.NUMBER, "Lost");

        nodeLineChart = new AreaChart(nodeLineTable, nodeLineOpts);
        nodeLinePane.add(nodeLineChart);

        nodeLineForm = new DynamicForm();
        final SelectItem nodeLineSelect = new SelectItem("nodeLineSelect", "");
        LinkedHashMap<String, String> nodeLineValues = new LinkedHashMap<>();
        for (Range r : StatHistory.Range.values()) {
            nodeLineValues.put("" + r.getChar(), r.getString());
        }
        nodeLineSelect.setDefaultValue("" + Range.MINUTE_10.getChar());
        nodeLineSelect.setValueMap(nodeLineValues);
        nodeLineForm.setItems(nodeLineSelect);
        nodeLineForm.setHeight(24);
        nodeLineForm.setWidth(40);

        nodeLineSelect.addChangedHandler(event -> {
            nodeLineForm.setDisabled(true);
            nodeLineHeaderLabel.setIcon("loading.gif");
            nodeLineSeriesForm.setDisabled(true);

            Range r = Range.create(nodeLineSelect.getValueAsString().charAt(0));
            controller.setRuntimeRRDRange(r,
                                          "FreeNodesCount",
                                          "BusyNodesCount",
                                          "DeployingNodesCount",
                                          "DownNodesCount",
                                          "LostNodesCount",
                                          "ConfigNodesCount",
                                          "PendingTasksCount",
                                          "AvailableNodesCount");
        });

        nodeLineHeaderLabel = new Label("<nobr style='font-size:1.4em;font-weight:bold;'>Nodes History<nobr>");
        nodeLineHeaderLabel.setHeight(24);

        Canvas filler = new Canvas();
        filler.setWidth100();

        HLayout nodeLineHeader = new HLayout();
        nodeLineHeader.setHeight(24);
        nodeLineHeader.setMembers(nodeLineHeaderLabel, filler, nodeLineForm);

        ChangedHandler seriesChanged = event -> {
            nodeLineSeriesForm.setDisabled(true);
            loadForm.setDisabled(true);
            loadHeaderLabel.setIcon("loading.gif");
            RMStatsView.this.statsUpdated(controller.getModel().getStatHistory());
        };

        this.nodeLineSeriesForm = new DynamicForm();
        nodeLineSeriesForm.setHeight(24);
        nodeLineSeriesForm.setNumCols(16);
        nodeLineSeriesForm.setWidth(300);
        CheckboxItem freeIt = new CheckboxItem("free",
                                               "<span style='background:#35a849;'>&nbsp;&nbsp;&nbsp;</span> Free");
        freeIt.setValue(true);
        freeIt.setWidth(90);
        freeIt.addChangedHandler(seriesChanged);

        CheckboxItem busyIt = new CheckboxItem("busy",
                                               "<span style='background:#fcaf3e;'>&nbsp;&nbsp;&nbsp;</span> Busy");
        busyIt.setValue(true);
        busyIt.setWidth(90);
        busyIt.addChangedHandler(seriesChanged);

        CheckboxItem deployingIt = new CheckboxItem("deploying",
                                                    "<span style='background:#24c1ff;'>&nbsp;&nbsp;&nbsp;</span> Deploying");
        deployingIt.setValue(true);
        deployingIt.setWidth(90);
        deployingIt.addChangedHandler(seriesChanged);

        CheckboxItem downIt = new CheckboxItem("down",
                                               "<span style='background:#ef2929;'>&nbsp;&nbsp;&nbsp;</span> Down");
        downIt.setValue(false);
        downIt.setWidth(90);
        downIt.addChangedHandler(seriesChanged);

        CheckboxItem pendingIt = new CheckboxItem("pending",
                                                  "<span style='background:#ffff00;'>&nbsp;&nbsp;&nbsp;</span> Needed");
        pendingIt.setValue(true);
        pendingIt.setWidth(90);
        pendingIt.setTooltip("Number of total Nodes needed for pending tasks ready to execute and that does not have an appropriate Node(s) to execute.");
        pendingIt.addChangedHandler(seriesChanged);

        CheckboxItem totalIt = new CheckboxItem("total",
                                                "<span style='background:#3a668d;'>&nbsp;&nbsp;&nbsp;</span> Total");
        totalIt.setValue(true);
        totalIt.setWidth(90);
        totalIt.addChangedHandler(seriesChanged);

        CheckboxItem configuringIt = new CheckboxItem("configuring",
                                                      "<span style='background:#1e4ed7;'>&nbsp;&nbsp;&nbsp;</span> Configuring");
        configuringIt.setValue(false);
        configuringIt.setWidth(90);
        configuringIt.addChangedHandler(seriesChanged);

        CheckboxItem lostIt = new CheckboxItem("lost",
                                               "<span style='background:#000000;'>&nbsp;&nbsp;&nbsp;</span> Lost");
        lostIt.setValue(false);
        lostIt.setWidth(90);
        lostIt.addChangedHandler(seriesChanged);

        nodeLineSeriesForm.setItems(totalIt, freeIt, pendingIt, busyIt, deployingIt, configuringIt, downIt, lostIt);

        /*
         * Instantaneous node state - Node State histogram
         */
        final AbsolutePanel nodeColPane = new AbsolutePanel();
        nodeColPane.setWidth("100%");
        nodeColPane.setHeight("150px");

        this.nodeColOpts = Options.create();
        nodeColOpts.setHeight(150);
        nodeColOpts.setLegend(LegendPosition.NONE);
        HorizontalAxisOptions nodeColHaxis = HorizontalAxisOptions.create();
        //nodeColHaxis.setSlantedText(false);
        nodeColHaxis.setMaxAlternation(1);
        nodeColOpts.setHAxisOptions(nodeColHaxis);
        nodeColOpts.setIsStacked(true);
        nodeColOpts.setColors("#3a668d",
                              "#35a849",
                              "#ffff00",
                              "#fcaf3e",
                              "#24c1ff",
                              "#24c1ff",
                              "#1e4ed7",
                              "#ef2929",
                              "#000000");
        //nodeColOpts.set("enableInteractivity", "false");

        nodeColTable = DataTable.create();
        nodeColTable.addColumn(ColumnType.STRING, "State");
        nodeColTable.addColumn(ColumnType.NUMBER, "Total");
        nodeColTable.addColumn(ColumnType.NUMBER, "Free");
        nodeColTable.addColumn(ColumnType.NUMBER, "Needed");
        nodeColTable.addColumn(ColumnType.NUMBER, "Busy");
        nodeColTable.addColumn(ColumnType.NUMBER, "Deploying");
        nodeColTable.addColumn(ColumnType.NUMBER, "Configuring");
        nodeColTable.addColumn(ColumnType.NUMBER, "Down");
        nodeColTable.addColumn(ColumnType.NUMBER, "Lost");

        nodeColChart = new ColumnChart(nodeColTable, nodeColOpts);
        nodeColPane.add(nodeColChart);

        nodeColHeaderLabel = new Label("<nobr style='font-size:1.4em;font-weight:bold;'>Nodes State</nobr>");
        nodeColHeaderLabel.setHeight(24);

        /*
         * Activity graph - Load history graph
         */
        final AbsolutePanel loadPane = new AbsolutePanel();
        loadPane.setWidth("100%");
        loadPane.setHeight("150px");

        loadOpts = Options.create();
        loadOpts.setHeight(150);
        HorizontalAxisOptions loadAxis = HorizontalAxisOptions.create();
        loadAxis.setMaxAlternation(1);
        loadAxis.setSlantedText(false);
        loadOpts.setLegend(LegendPosition.NONE);
        loadOpts.setHAxisOptions(loadAxis);
        loadOpts.setColors("#fcaf3e");

        loadTable = DataTable.create();
        loadTimeId = loadTable.addColumn(ColumnType.STRING, "Time");
        loadValId = loadTable.addColumn(ColumnType.NUMBER, "Load");

        loadChart = new AreaChart(loadTable, loadOpts);
        loadPane.add(loadChart);

        loadForm = new DynamicForm();
        final SelectItem loadSelect = new SelectItem("loadSelect", "");
        LinkedHashMap<String, String> loadValues = new LinkedHashMap<String, String>();
        for (Range r : StatHistory.Range.values()) {
            loadValues.put("" + r.getChar(), r.getString());
        }
        loadSelect.setDefaultValue("" + Range.MINUTE_10.getChar());
        loadSelect.setValueMap(loadValues);
        loadForm.setItems(loadSelect);
        loadForm.setHeight(24);
        loadForm.setWidth(40);

        loadSelect.addChangedHandler(event -> {
            loadForm.setDisabled(true);
            loadHeaderLabel.setIcon("loading.gif");
            nodeLineSeriesForm.setDisabled(true);

            Range r = Range.create(loadSelect.getValueAsString().charAt(0));
            controller.setRuntimeRRDRange(r, "AverageActivity");
        });

        loadHeaderLabel = new Label("<nobr style='font-size:1.4em;font-weight:bold;'>Load History<nobr>");
        loadHeaderLabel.setTooltip("Percentage of cumulative node utilization since server start-up.");
        loadHeaderLabel.setHeight(24);

        filler = new Canvas();
        filler.setWidth100();

        HLayout loadHeader = new HLayout();
        loadHeader.setHeight(24);
        loadHeader.setMembersMargin(20);
        loadHeader.setMembers(loadHeaderLabel, filler, loadForm);

        Canvas cc1 = new Canvas();
        cc1.setHeight(4);
        cc1.setBackgroundColor("#eee");
        Canvas cc2 = new Canvas();
        cc2.setHeight(4);
        cc2.setBackgroundColor("#eee");

        root.addMember(nodeColHeaderLabel);
        root.addMember(nodeColPane);
        root.addMember(cc1);
        root.addMember(nodeLineHeader);
        root.addMember(nodeLinePane);
        root.addMember(nodeLineSeriesForm);
        root.addMember(cc2);
        root.addMember(loadHeader);
        root.addMember(loadPane);

        return root;
    }

    @Override
    public void statsUpdated(Map<String, StatHistory> values) {

        StatHistory freeNodes = values.get("FreeNodesCount");
        StatHistory busyNodes = values.get("BusyNodesCount");
        StatHistory deployingNodes = values.get("DeployingNodesCount");
        StatHistory downNodes = values.get("DownNodesCount");
        StatHistory pendingTasks = values.get("PendingTasksCount");
        StatHistory totalNodes = values.get("AvailableNodesCount");
        StatHistory configuringNodes = values.get("ConfigNodesCount");
        StatHistory lostNodes = values.get("LostNodesCount");

        nodeLineTable.removeRows(0, nodeLineTable.getNumberOfRows());
        nodeLineTable.addRows(freeNodes.values.size());

        long now = new Date().getTime() / 1000;
        long dur = freeNodes.range.getDuration();
        long step = dur / freeNodes.values.size();
        for (int i = 0; i < freeNodes.values.size(); i++) {
            long t = now - dur + step * i;
            PredefinedFormat format = freeNodes.range.getFormat();
            String timeStamp = DateTimeFormat.getFormat(format).format(new Date(t * 1000));

            nodeLineTable.setValue(i, nodeLineTimeId, timeStamp);

            if (Boolean.parseBoolean(nodeLineSeriesForm.getValueAsString("free"))) {
                nodeLineTable.setValue(i, nodeLineFreeId, Math.round(freeNodes.values.get(i)));
            }
            if (Boolean.parseBoolean(nodeLineSeriesForm.getValueAsString("busy"))) {
                nodeLineTable.setValue(i, nodeLineBusyId, Math.round(busyNodes.values.get(i)));
            }
            if (Boolean.parseBoolean(nodeLineSeriesForm.getValueAsString("deploying"))) {
                nodeLineTable.setValue(i, nodeLineDeployingId, Math.round(deployingNodes.values.get(i)));
            }
            if (Boolean.parseBoolean(nodeLineSeriesForm.getValueAsString("down"))) {
                nodeLineTable.setValue(i, nodeLineDownId, Math.round(downNodes.values.get(i)));
            }
            if (Boolean.parseBoolean(nodeLineSeriesForm.getValueAsString("pending"))) {
                nodeLineTable.setValue(i, linePendingTasksId, Math.round(pendingTasks.values.get(i)));
            }
            if (Boolean.parseBoolean(nodeLineSeriesForm.getValueAsString("total"))) {
                nodeLineTable.setValue(i, nodeLineTotalId, Math.round(totalNodes.values.get(i)));
            }
            if (Boolean.parseBoolean(nodeLineSeriesForm.getValueAsString("configuring"))) {
                nodeLineTable.setValue(i, nodeLineConfiguringId, Math.round(configuringNodes.values.get(i)));
            }
            if (Boolean.parseBoolean(nodeLineSeriesForm.getValueAsString("lost"))) {
                nodeLineTable.setValue(i, nodeLineLostId, Math.round(lostNodes.values.get(i)));
            }

        }
        nodeLineChart.draw(nodeLineTable, nodeLineOpts);
        nodeLineForm.setDisabled(false);
        nodeLineSeriesForm.setDisabled(false);
        nodeLineHeaderLabel.setIcon(null);

        StatHistory loadHist = values.get("AverageActivity");

        loadTable.removeRows(0, loadTable.getNumberOfRows());
        loadTable.addRows(loadHist.values.size());
        dur = loadHist.range.getDuration();
        step = dur / loadHist.values.size();
        for (int i = 0; i < loadHist.values.size(); i++) {
            long t = now - dur + step * i;
            PredefinedFormat format = loadHist.range.getFormat();
            String timeStamp = DateTimeFormat.getFormat(format).format(new Date(t * 1000));

            loadTable.setValue(i, loadTimeId, timeStamp);
            loadTable.setValue(i, loadValId, loadHist.values.get(i));
        }
        loadChart.draw(loadTable, loadOpts);
        loadForm.setDisabled(false);
        loadHeaderLabel.setIcon(null);
    }

    @Override
    public void nodesUpdated(Map<String, NodeSource> nodes) {
        int depl = controller.getModel().getNumDeploying();
        int conf = controller.getModel().getNumConfiguring();
        int free = controller.getModel().getNumFree();
        int busy = controller.getModel().getNumBusy();
        int down = controller.getModel().getNumDown();
        int lost = controller.getModel().getNumLost();
        int total = controller.getModel().getNumNodes();
        int needed = controller.getModel().getNumNeeded();

        nodeColTable.removeRows(0, nodeColTable.getNumberOfRows());
        nodeColTable.addRows(8);

        nodeColTable.setValue(0, 0, "Total");
        nodeColTable.setValue(0, 1, total);

        nodeColTable.setValue(1, 0, "Free");
        nodeColTable.setValue(1, 2, free);

        nodeColTable.setValue(2, 0, "Needed");
        nodeColTable.setValue(2, 3, needed);

        nodeColTable.setValue(3, 0, "Busy");
        nodeColTable.setValue(3, 4, busy);

        nodeColTable.setValue(4, 0, "Deploying");
        nodeColTable.setValue(4, 5, depl);

        nodeColTable.setValue(5, 0, "Configuring");
        nodeColTable.setValue(5, 6, conf);

        nodeColTable.setValue(6, 0, "Down");
        nodeColTable.setValue(6, 7, down);

        nodeColTable.setValue(7, 0, "Lost");
        nodeColTable.setValue(7, 8, lost);

        nodeColChart.draw(nodeColTable, nodeColOpts);

    }
}
