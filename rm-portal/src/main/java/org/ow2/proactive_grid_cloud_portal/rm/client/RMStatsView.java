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

import java.util.AbstractMap;
import java.util.ArrayList;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.ow2.proactive_grid_cloud_portal.common.client.Listeners.StatsListener;
import org.ow2.proactive_grid_cloud_portal.common.client.Model.StatHistory;
import org.ow2.proactive_grid_cloud_portal.common.client.Model.StatHistory.Range;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMListeners.NodesListener;
import org.pepstock.charba.client.BarChart;
import org.pepstock.charba.client.LineChart;
import org.pepstock.charba.client.configuration.CartesianLinearAxis;
import org.pepstock.charba.client.configuration.CartesianTimeAxis;
import org.pepstock.charba.client.data.BarBorderWidth;
import org.pepstock.charba.client.data.BarDataset;
import org.pepstock.charba.client.data.DataPoint;
import org.pepstock.charba.client.data.Dataset;
import org.pepstock.charba.client.data.LineDataset;
import org.pepstock.charba.client.enums.Fill;
import org.pepstock.charba.client.enums.Position;
import org.pepstock.charba.client.enums.ScaleDistribution;
import org.pepstock.charba.client.enums.SteppedLine;
import org.pepstock.charba.client.enums.TickSource;
import org.pepstock.charba.client.enums.TimeUnit;
import org.pepstock.charba.client.resources.EmbeddedResources;
import org.pepstock.charba.client.resources.ResourcesType;

import com.google.gwt.i18n.client.DateTimeFormat;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.SelectItem;
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

    public static final String FREE_NODES_COUNT = "FreeNodesCount";

    public static final String NEEDED_NODES_COUNT = "NeededNodesCount";

    public static final String BUSY_NODES_COUNT = "BusyNodesCount";

    public static final String AVAILABLE_NODES_COUNT = "AvailableNodesCount";

    public static final String DEPLOYING_NODES_COUNT = "DeployingNodesCount";

    public static final String CONFIG_NODES_COUNT = "ConfigNodesCount";

    public static final String DOWN_NODES_COUNT = "DownNodesCount";

    public static final String LOST_NODES_COUNT = "LostNodesCount";

    public static final String AVERAGE_ACTIVITY = "AverageActivity";

    private RMController controller;

    private DynamicForm nodeLineForm;

    private Label nodeLineHeaderLabel;

    private Label nodeColHeaderLabel;

    private DynamicForm loadForm;

    private Label loadHeaderLabel;

    private BarChart barChart;

    private LineChart nodeHistoryChart;

    private LineChart activityChart;

    private List<Map.Entry<String, String>> datasetsAndColor = new ArrayList<>();

    {
        datasetsAndColor.add(new AbstractMap.SimpleEntry<>("Total", "#3a668d"));
        datasetsAndColor.add(new AbstractMap.SimpleEntry<>("Free", "#35a849"));
        datasetsAndColor.add(new AbstractMap.SimpleEntry<>("Needed", "#ffff00"));
        datasetsAndColor.add(new AbstractMap.SimpleEntry<>("Busy", "#fcaf3e"));
        datasetsAndColor.add(new AbstractMap.SimpleEntry<>("Deploying", "#24c1ff"));
        datasetsAndColor.add(new AbstractMap.SimpleEntry<>("Configuring", "#1e4ed7"));
        datasetsAndColor.add(new AbstractMap.SimpleEntry<>("Down", "#ef2929"));
        datasetsAndColor.add(new AbstractMap.SimpleEntry<>("Lost", "#000000"));
    }

    RMStatsView(RMController controller) {
        this.controller = controller;
        this.controller.getEventDispatcher().addStatsListener(this);
        this.controller.getEventDispatcher().addNodesListener(this);
    }

    Canvas build() {

        ResourcesType.setClientBundle(EmbeddedResources.INSTANCE);

        VLayout root = new VLayout();
        root.setBackgroundColor("white");
        root.setPadding(5);
        root.setWidth100();
        root.setHeight100();
        root.setAlign(Alignment.CENTER);
        root.setOverflow(Overflow.AUTO);

        /*
         * Node history graph
         */
        nodeHistoryChart = new LineChart();
        nodeHistoryChart.getOptions().setResponsive(true);
        nodeHistoryChart.getOptions().getLegend().setDisplay(true);
        nodeHistoryChart.getOptions().getLegend().setPosition(Position.BOTTOM);
        nodeHistoryChart.getOptions().getTitle().setDisplay(false);
        nodeHistoryChart.getOptions().setMaintainAspectRatio(false);
        nodeHistoryChart.setHeight("250px");
        nodeHistoryChart.setWidth("100%");
        //        CartesianTimeAxis xAxis = new CartesianTimeAxis(nodeHistoryChart);
        //        xAxis.setDistribution(ScaleDistribution.SERIES);
        //        xAxis.getTicks().setSource(TickSource.DATA);
        //        xAxis.getTicks().setDisplay(true);
        //        xAxis.getTicks().setAutoSkip(true);
        //        xAxis.getTime().setUnit(TimeUnit.SECOND);
        //        CartesianLinearAxis yAxis = new CartesianLinearAxis(nodeHistoryChart);
        //        yAxis.setDisplay(true);
        //        yAxis.getTicks().setBeginAtZero(true);
        //        nodeHistoryChart.getOptions().getScales().setXAxes(xAxis);
        //        nodeHistoryChart.getOptions().getScales().setYAxes(yAxis);

        List<Dataset> datasets = new ArrayList<>();
        for (Map.Entry<String, String> datasetAndColor : datasetsAndColor) {
            LineDataset dataset = nodeHistoryChart.newDataset();
            dataset.setFill(Fill.FALSE);
            dataset.setLabel(datasetAndColor.getKey());
            dataset.setBackgroundColor(datasetAndColor.getValue());
            dataset.setBorderColor(datasetAndColor.getValue());
            dataset.setPointBackgroundColor(datasetAndColor.getValue());
            dataset.setPointBorderColor(datasetAndColor.getValue());
            dataset.setSteppedLine(SteppedLine.AFTER);

            datasets.add(dataset);
        }

        nodeHistoryChart.getData().setDatasets(datasets.toArray(new Dataset[0]));

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
            nodeLineHeaderLabel.setIcon("loading.gif");
            Range r = Range.create(nodeLineSelect.getValueAsString().charAt(0));
            controller.setRuntimeRRDRange(r,
                                          FREE_NODES_COUNT,
                                          BUSY_NODES_COUNT,
                                          DEPLOYING_NODES_COUNT,
                                          DOWN_NODES_COUNT,
                                          LOST_NODES_COUNT,
                                          CONFIG_NODES_COUNT,
                                          NEEDED_NODES_COUNT,
                                          AVAILABLE_NODES_COUNT);
        });

        nodeLineHeaderLabel = new Label("<nobr style='font-size:1.4em;font-weight:bold;'>Nodes History<nobr>");
        nodeLineHeaderLabel.setHeight(24);

        Canvas filler = new Canvas();
        filler.setWidth100();

        HLayout nodeLineHeader = new HLayout();
        nodeLineHeader.setHeight(24);
        nodeLineHeader.setMembers(nodeLineHeaderLabel, filler, nodeLineForm);

        //
        /*
         * Instantaneous node state - Node State histogram
         */

        barChart = new BarChart();
        barChart.getOptions().setResponsive(true);
        barChart.getOptions().getLegend().setDisplay(false);

        BarDataset dataset1 = barChart.newDataset();
        BarBorderWidth border = new BarBorderWidth();
        border.setTop(0);
        border.setLeft(0);
        border.setRight(0);

        dataset1.setBorderWidth(border);
        dataset1.setData(0, 0, 0, 0, 0, 0, 0, 0);

        dataset1.setBackgroundColor("#3a668d",
                                    "#35a849",
                                    "#ffff00",
                                    "#fcaf3e",
                                    "#24c1ff",
                                    "#1e4ed7",
                                    "#ef2929",
                                    "#000000");

        barChart.getData().setLabels(datasetsAndColor.stream().map(Map.Entry::getKey).toArray(String[]::new));
        barChart.getData().setDatasets(dataset1);
        barChart.setHeight("150px");
        barChart.setWidth("100%");
        barChart.getOptions().setMaintainAspectRatio(false);
        barChart.getOptions().getLayout().getPadding().setRight(20);
        barChart.getOptions().getLayout().getPadding().setLeft(20);
        barChart.getOptions().getElements().getRectangle().setBorderWidth(10);

        nodeColHeaderLabel = new Label("<nobr style='font-size:1.4em;font-weight:bold;'>Nodes State</nobr>");
        nodeColHeaderLabel.setHeight(24);

        /*
         * Activity graph - Load history graph
         */

        activityChart = new LineChart();
        CartesianLinearAxis axis = new CartesianLinearAxis(activityChart);
        axis.getTicks().setAutoSkip(false);
        axis.getTicks().setMaxRotation(0);
        axis.getTicks().setMin(0.0);

        activityChart.getOptions().setResponsive(true);
        activityChart.getOptions().getLegend().setDisplay(false);
        activityChart.getOptions().getTitle().setDisplay(false);
        activityChart.getOptions().setSpanGaps(false);
        activityChart.getOptions().getElements().getLine().setTension(0.000001D);
        activityChart.getOptions().getScales().setYAxes(axis);
        activityChart.setHeight("250px");
        activityChart.setWidth("100%");
        activityChart.getOptions().setMaintainAspectRatio(false);

        LineDataset dataset = activityChart.newDataset();
        dataset.setBackgroundColor("#fcaf3e");
        dataset.setBorderColor("#fcaf3e");
        dataset.setFill(Fill.START);
        activityChart.getData().setDatasets(dataset);

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
            Range r = Range.create(loadSelect.getValueAsString().charAt(0));
            controller.setRuntimeRRDRange(r, AVERAGE_ACTIVITY);
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
        root.addMember(barChart);
        root.addMember(cc1);
        root.addMember(nodeLineHeader);
        root.addMember(nodeHistoryChart);
        root.addMember(cc2);
        root.addMember(loadHeader);
        root.addMember(activityChart);

        return root;
    }

    @Override
    public void statsUpdated(Map<String, StatHistory> values) {

        StatHistory freeNodes = values.get(FREE_NODES_COUNT);

        long now = new Date().getTime() / 1000; // seconds
        long dur = freeNodes.range.getDuration();
        long step = dur / freeNodes.values.size();

        StatHistory[] statHistories = new StatHistory[] { values.get(AVAILABLE_NODES_COUNT),
                                                          values.get(FREE_NODES_COUNT), values.get(NEEDED_NODES_COUNT),
                                                          values.get(BUSY_NODES_COUNT),
                                                          values.get(DEPLOYING_NODES_COUNT),
                                                          values.get(CONFIG_NODES_COUNT), values.get(DOWN_NODES_COUNT),
                                                          values.get(LOST_NODES_COUNT) };

        for (int d = 0; d < statHistories.length; ++d) {
            StatHistory statHistory = statHistories[d];
            LineDataset dataset = (LineDataset) nodeHistoryChart.getData().getDatasets().get(d);

            List<Double> dps = new ArrayList<>();
            List<String> labels = new ArrayList<>();
            for (int i = 0; i < statHistory.values.size(); i++) {
                long t = now - dur + step * i; // seconds

                DateTimeFormat.PredefinedFormat format = statHistory.range.getFormat();
                String timeStamp = DateTimeFormat.getFormat(format).format(new Date(t * 1000));
                labels.add(timeStamp);

                Long value = Math.round(statHistory.values.get(i));
                dps.add(value.doubleValue());
            }

            dataset.setData(dps);
            nodeHistoryChart.getData().setLabels(labels.toArray(new String[0]));
        }

        nodeHistoryChart.update();

        nodeLineHeaderLabel.setIcon(null);

        StatHistory loadHist = values.get(AVERAGE_ACTIVITY);

        LineDataset dataset = (LineDataset) activityChart.getData().getDatasets().get(0);

        dur = loadHist.range.getDuration();
        step = dur / loadHist.values.size();
        List<Double> dps = new ArrayList<>();
        List<String> labels = new ArrayList<>();

        for (int i = 0; i < loadHist.values.size(); i++) {
            long t = now - dur + step * i;
            DateTimeFormat.PredefinedFormat format = loadHist.range.getFormat();
            String timeStamp = DateTimeFormat.getFormat(format).format(new Date(t * 1000));

            Long value = Math.round(loadHist.values.get(i));
            dps.add(value.doubleValue());
            labels.add(timeStamp);
        }
        activityChart.getData().setLabels(labels.toArray(new String[0]));
        dataset.setData(dps);

        activityChart.update();
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
        int needed = controller.getModel().getNeededNodes();

        barChart.getData().getDatasets().get(0).setData(total, free, needed, busy, depl, conf, down, lost);
        barChart.update();
    }
}
