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
package org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.charts;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.ow2.proactive_grid_cloud_portal.common.client.Model;
import org.ow2.proactive_grid_cloud_portal.common.client.json.JSONUtils;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LogModel;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LoginModel;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMController;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMServiceAsync;
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.Reloadable;
import org.pepstock.charba.client.AbstractChart;
import org.pepstock.charba.client.IsChart;
import org.pepstock.charba.client.LineChart;
import org.pepstock.charba.client.PieChart;
import org.pepstock.charba.client.callbacks.TickCallback;
import org.pepstock.charba.client.callbacks.TooltipCustomCallback;
import org.pepstock.charba.client.callbacks.TooltipFilterCallback;
import org.pepstock.charba.client.configuration.Axis;
import org.pepstock.charba.client.configuration.CartesianCategoryAxis;
import org.pepstock.charba.client.configuration.CartesianLinearAxis;
import org.pepstock.charba.client.configuration.ConfigurationOptions;
import org.pepstock.charba.client.configuration.LineOptions;
import org.pepstock.charba.client.data.Dataset;
import org.pepstock.charba.client.data.LineDataset;
import org.pepstock.charba.client.enums.Fill;
import org.pepstock.charba.client.enums.InteractionMode;
import org.pepstock.charba.client.enums.Position;
import org.pepstock.charba.client.items.TooltipBodyItem;
import org.pepstock.charba.client.items.TooltipItem;
import org.pepstock.charba.client.items.TooltipLabelColor;
import org.pepstock.charba.client.items.TooltipModel;

import com.google.gwt.dom.client.DivElement;
import com.google.gwt.dom.client.Document;
import com.google.gwt.dom.client.SpanElement;
import com.google.gwt.dom.client.Style;
import com.google.gwt.i18n.client.DateTimeFormat;
import com.google.gwt.json.client.JSONArray;
import com.google.gwt.json.client.JSONNumber;
import com.google.gwt.json.client.JSONObject;
import com.google.gwt.json.client.JSONValue;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.google.gwt.user.client.ui.AbsolutePanel;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.VLayout;


/**
 * Chart that retrieves information from MBean.
 */
public abstract class MBeanChart extends VLayout implements Reloadable {

    protected final int MAX_ROWS_NUMBER = 100;

    protected RMController controller;

    protected String jmxServerUrl;

    protected String mbeanName;

    protected String[] attrs;

    protected String[] datasourceNames;

    protected AbstractChart chart;

    protected static Model.StatHistory.Range timeRange = Model.StatHistory.Range.MINUTE_1;

    protected Runnable onFinish;

    protected AbsolutePanel chartContainer;

    protected boolean areaChart = false;

    protected String[] colors = new String[] { "#fcaf3e", "#3a668d", "#35a849", "#fcaf3e", "#24c1ff", "#1e4ed7",
                                               "#ef2929", "#000000" };

    private List<String> xLabels = new LinkedList<>();

    public MBeanChart(RMController controller, String jmxServerUrl, String mbean, String[] attrs, String title) {
        this.controller = controller;
        this.jmxServerUrl = jmxServerUrl;
        this.mbeanName = mbean;
        this.attrs = attrs;

        setWidth100();
        setHeight100();

        if (title.length() > 0) {
            Label label = new Label("<nobr style='font-weight:bold;padding-left:10px;'>" + title + "<nobr>");
            label.setHeight(30);
            addMember(label);
        }

        chartContainer = new AbsolutePanel();
        chartContainer.setWidth("90%");
        chartContainer.setHeight("200px");
        //        chartContainer.getElement().getStyle().setMargin(30, Style.Unit.PX);
        chartContainer.getElement().getStyle().setPadding(30, Style.Unit.PX);

        chart = createChart();
        chart.setWidth("100%");
        chart.setHeight("100%");

        chart.getOptions().setMaintainAspectRatio(false);
        if (!(chart instanceof PieChart)) {
            LineChart lineChart = (LineChart) chart;

            CartesianCategoryAxis xAxis = new CartesianCategoryAxis(chart);
            xAxis.getGrideLines().setDisplay(false);
            lineChart.getOptions().getScales().setXAxes(xAxis);

            CartesianLinearAxis yAxis = new CartesianLinearAxis(chart);
            yAxis.getTicks().setBeginAtZero(true);
            lineChart.getOptions().getScales().setYAxes(yAxis);

        }

        chart.getOptions().getHover().setIntersect(false);
        chart.getOptions().getHover().setMode(InteractionMode.INDEX);

        chart.getOptions().getTooltips().setMode(InteractionMode.INDEX);
        chart.getOptions().getTooltips().setIntersect(false);
        chart.getOptions().getLegend().setPosition(Position.RIGHT);
        chart.getOptions().getLegend().getLabels().setBoxWidth(15);

        // lets not convert this object to LAMBDA! it fails
        chart.getOptions().getTooltips().setFilterCallback(new TooltipFilterCallback() {
            @Override
            public boolean onFilter(IsChart chart, TooltipItem item) {
                Double pointData = chart.getData()
                                        .getDatasets()
                                        .get(item.getDatasetIndex())
                                        .getData()
                                        .get(item.getIndex());
                return pointData != null && pointData > 0;
            }
        });

        chartContainer.add(chart);

        addMember(chartContainer);
    }

    public static String removingInternalEscaping(String result) {
        result = result.replace("\\\"", "\"");
        result = result.replace("\"{", "{");
        result = result.replace("}\"", "}");
        return result;
    }

    public void addXLabel(String label) {
        while (xLabels.size() >= MAX_ROWS_NUMBER) {
            xLabels.remove(0);
        }

        xLabels.add(label);

        chart.getData().setLabels(xLabels.toArray(new String[0]));
    }

    public void addPointToDataset(int index, double value) {
        if (index >= chart.getData().getDatasets().size()) {
            Dataset dataset = createDataset(index);
            ArrayList<Dataset> newDatasets = new ArrayList<>(chart.getData().getDatasets());
            newDatasets.add(dataset);
            chart.getData().setDatasets(newDatasets.toArray(new Dataset[0]));
        }

        List<Dataset> datasets = new ArrayList<>(chart.getData().getDatasets());
        Dataset dataset = datasets.get(index);
        List<Double> data = new ArrayList<>(dataset.getData());
        while (data.size() >= MAX_ROWS_NUMBER) {
            data.remove(0);
        }
        data.add(formatValue(value));
        dataset.setData(data);
        chart.getData().setDatasets(datasets.toArray(new Dataset[0]));
    }

    public void setYAxesTicksSuffix(String suffix) {
        ConfigurationOptions options = chart.getOptions();
        if (options instanceof LineOptions) {
            LineOptions lineOptions = (LineOptions) options;
            CartesianLinearAxis axis;
            if (lineOptions.getScales().getYAxes().isEmpty()) {
                axis = new CartesianLinearAxis(chart);
            } else {
                axis = (CartesianLinearAxis) lineOptions.getScales().getYAxes().get(0);

            }
            axis.getTicks().setCallback(new TickCallback() {
                @Override
                public String onCallback(Axis axis, double value, int index, List<Double> values) {
                    return value + suffix;
                }
            });

            lineOptions.getScales().setYAxes(axis);

        }

    }

    @Override
    public void reload() {
        RMServiceAsync rm = controller.getRMService();
        long t = System.currentTimeMillis();

        boolean realTime = timeRange.equals(Model.StatHistory.Range.MINUTE_1);

        LoginModel loginModel = LoginModel.getInstance();

        AsyncCallback callback = new AsyncCallback<String>() {
            public void onSuccess(String result) {
                if (onFinish != null) {
                    onFinish.run();
                }
                if (!loginModel.isLoggedIn())
                    return;

                LogModel.getInstance().logMessage("Fetched " + mbeanName + ":" + Arrays.toString(attrs) + " in " +
                                                  (System.currentTimeMillis() - t) + "ms");

                if (realTime) {
                    processResult(result);
                } else {
                    processHistoryResult(result);
                }
            }

            public void onFailure(Throwable caught) {
                if (onFinish != null) {
                    onFinish.run();
                }
                if (JSONUtils.getJsonErrorCode(caught) == 401) {
                    LogModel.getInstance().logMessage("You have been disconnected from the server.");
                }
            }
        };

        if (realTime) {
            rm.getNodeMBeanInfo(loginModel.getSessionId(), jmxServerUrl, mbeanName, Arrays.asList(attrs), callback);
        } else {
            try {
                rm.getNodeMBeanHistory(loginModel.getSessionId(),
                                       jmxServerUrl,
                                       mbeanName,
                                       Arrays.asList(attrs),
                                       String.valueOf(timeRange.getChar()),
                                       callback);
            } catch (Exception e) {
                LogModel.getInstance().logMessage(e.getMessage());
            }
        }
    }

    protected int getJsonInternalSize(JSONObject json) {
        for (String key : json.keySet()) {
            JSONArray values = json.get(key).isArray();
            if (values != null) {
                return values.size();
            }
        }

        return 0;
    }

    protected double[] getJsonSlice(JSONObject json, int i) {

        double[] res = new double[json.keySet().size()];

        int counter = 0;
        for (String key : json.keySet()) {
            JSONArray values = json.get(key).isArray();
            double numValue = 0;

            if (values != null) {
                JSONNumber num = values.get(i).isNumber();
                if (num != null) {
                    numValue = num.doubleValue();
                }
            }

            res[counter++] = numValue;
        }
        return res;
    }

    protected Dataset createDataset(int i) {
        if (chart instanceof LineChart) {
            LineDataset dataset = (LineDataset) chart.newDataset();
            if (i < colors.length) {
                dataset.setBorderColor(colors[i]);
            }

            if (datasourceNames != null && i < datasourceNames.length) {
                dataset.setLabel(datasourceNames[i]);
            } else {
                dataset.setLabel(String.valueOf(i));
            }

            dataset.setPointRadius(0);
            dataset.setBorderWidth(2);
            if (areaChart) {
                dataset.setFill(Fill.START);
                dataset.setBackgroundColor(dataset.getBorderColor().alpha(0.3));
            } else {
                dataset.setFill(Fill.FALSE);
                dataset.setBackgroundColor(dataset.getBorderColor());
            }
            return dataset;
        } else {
            LogModel.getInstance().logCriticalMessage("Chart has wrong type: " + chart.getClass().getName());
            return null;
        }
    }

    /**
     * When time selector (1m, 5m, 10m, 30m, etc) selects any value
     * except very first one (1 minute - it is special case)
     * this method is called.
     * Method should parse JSON and load all values to the chart.
     * @param result JSON string
     */
    public void processHistoryResult(String result) {
        result = removingInternalEscaping(result);

        JSONValue resultVal = controller.parseJSON(result);
        JSONObject json = resultVal.isObject();

        if (json == null) {
            return;
        }

        long now = new Date().getTime() / 1000;
        long dur = timeRange.getDuration();
        long size = getJsonInternalSize(json);

        long step = dur / size;

        final int length = getJsonSlice(json, 0).length;
        List<Dataset> datasets = new ArrayList<>();
        List<Double>[] dpss = new List[length];

        for (int i = 0; i < length; ++i) {
            Dataset dataset = createDataset(i);

            datasets.add(dataset);

            dpss[i] = new ArrayList<>();
        }

        List<String> labels = new ArrayList<>();
        for (int i = 0; i < size; i++) {

            double[] slice = getJsonSlice(json, i);
            long t = now - dur + step * i;
            DateTimeFormat.PredefinedFormat format = timeRange.getFormat();
            String timeStamp = DateTimeFormat.getFormat(format).format(new Date(t * 1000));

            labels.add(timeStamp);

            for (int sliceIndex = 0; sliceIndex < slice.length; sliceIndex++) {
                dpss[sliceIndex].add(formatValue(slice[sliceIndex]));
            }
        }

        chart.getData().setLabels(labels.toArray(new String[0]));

        for (int i = 0; i < length; ++i) {
            datasets.get(i).setData(dpss[i]);
        }

        chart.getData().setDatasets(datasets.toArray(new Dataset[0]));

        chart.update();
    }

    /**
     * Read processHistoryResult.
     * Instead of 'processHistoryResult' this method
     * is called only when '1 minute' interval is selected.
     * Method should parse JSON and add one value to the chart.
     * @param result JSON string
     */
    public abstract void processResult(String result);

    public abstract AbstractChart createChart();

    public double formatValue(double value) {
        return value;
    }

    public void onFinish(Runnable onFinish) {
        this.onFinish = onFinish;
    }

    public void selectRange(Model.StatHistory.Range timeRange) {
        MBeanChart.timeRange = timeRange;
        chart.getData().setDatasets();
        xLabels = new LinkedList<>();
        this.reload();
    }

    public void setColors(String... colors) {
        this.colors = colors;
    }

    public String[] getColors() {
        return colors;
    }

    public void setDatasourceNames(String... datasouceNames) {
        this.datasourceNames = datasouceNames;
    }

    public String[] getDatasourceNames() {
        return datasourceNames;
    }

    public void setAreaChart(boolean areaChart) {
        this.areaChart = areaChart;
    }

    public void setTooltipItemHandler(Function<String, String> handler) {
        setTooltipItemHandler(chart, handler);
    }

    public static void setTooltipItemHandler(AbstractChart chart, Function<String, String> handler) {
        chart.getOptions().getTooltips().setEnabled(false);

        chart.getOptions().getTooltips().setCustomCallback(new TooltipCustomCallback() {

            private DivElement element = null;

            @Override
            public void onCustom(IsChart chart, TooltipModel model) {

                if (model.getOpacity() == 0) {
                    element.getStyle().setOpacity(0);
                    return;
                }
                if (element == null) {
                    element = Document.get().createDivElement();
                    AbstractChart<?> chartInstance = (AbstractChart<?>) chart;
                    chartInstance.getElement().appendChild(element);
                }
                element.removeClassName("above");
                element.removeClassName("below");
                element.removeClassName("no-transform");
                if (model.getYAlign() != null) {
                    element.addClassName(model.getYAlign());
                } else {
                    element.addClassName("no-transform");
                }
                StringBuilder innerHTML = new StringBuilder("<table cellpadding=0>");

                if (model.getBody() != null && !model.getBody().isEmpty()) {
                    innerHTML.append("<tbody>");
                    if (model.getTitle() != null && !model.getTitle().isEmpty()) {
                        for (String title : model.getTitle()) {
                            innerHTML.append("<tr><td style='white-space: nowrap;'>")
                                     .append(title)
                                     .append("</td></tr>");
                        }
                    }
                    //                    innerHTML.append("</thead><tbody>");
                    List<TooltipLabelColor> colors = model.getLabelColors();
                    int index = 0;
                    for (TooltipBodyItem item : model.getBody()) {
                        List<String> lines = item.getLines();
                        lines = lines.stream().map(handler::apply).collect(Collectors.toList());
                        for (int i = 0; i < lines.size(); i++) {

                            TooltipLabelColor color = colors.get(index);
                            DivElement wrapper = Document.get().createDivElement();
                            SpanElement span = Document.get().createSpanElement();
                            span.getStyle().setDisplay(Style.Display.INLINE_BLOCK);
                            span.getStyle().setWidth(10, Style.Unit.PX);
                            span.getStyle().setHeight(10, Style.Unit.PX);
                            span.getStyle().setMarginRight(2, Style.Unit.PX);
                            span.getStyle().setBackgroundColor(color.getBackgroundColor().toRGBA());
                            span.getStyle().setBorderColor(color.getBorderColor().toRGBA());
                            span.getStyle().setBorderStyle(Style.BorderStyle.SOLID);
                            span.getStyle().setBorderWidth(2, Style.Unit.PX);
                            wrapper.appendChild(span);
                            innerHTML.append("<tr><td style='white-space: nowrap;'>")
                                     .append(wrapper.getInnerHTML())
                                     .append(lines.get(i))
                                     .append("</td></tr>");
                        }
                        index++;
                    }
                    innerHTML.append("</tbody>");
                }
                innerHTML.append("</table>");
                element.setInnerHTML(innerHTML.toString());
                if (chart.getCanvas().getAbsoluteLeft() < model.getCaretX() - 80) {
                    element.getStyle().setLeft(model.getCaretX() - 80, Style.Unit.PX);
                } else {
                    element.getStyle().setLeft(model.getCaretX(), Style.Unit.PX);
                }
                element.getStyle().setTop(model.getCaretY(), Style.Unit.PX);
                element.getStyle().setFontSize(model.getBodyFontSize(), Style.Unit.PX);
                element.getStyle().setPaddingLeft(model.getXPadding(), Style.Unit.PX);
                element.getStyle().setPaddingTop(model.getYPadding(), Style.Unit.PX);

                element.getStyle().setOpacity(1);
                element.getStyle().setBackgroundColor("rgba(0, 0, 0, .7)");
                element.getStyle().setPosition(com.google.gwt.dom.client.Style.Position.ABSOLUTE);
                element.getStyle().setColor("white");
                element.getStyle().setProperty("borderRadius", "3px");
                element.getStyle().setProperty("WebkitTransition", "all .1s ease");
                element.getStyle().setProperty("transition", "all .1s ease");
                element.getStyle().setProperty("pointerEvents", "none");
                element.getStyle().setProperty("WebkitTransform", "translate(-50%, 0)");
                element.getStyle().setProperty("transform", "translate(-50%, 0)");
            }
        });
    }

    public static String keepNDigitsAfterComma(String number, int nDigits) {
        // leave only 2 digits after dot
        if (number.contains(".")) {
            int i = number.indexOf(".");
            if (nDigits == 0) {
                number = number.substring(0, i);
            } else {
                i = Math.min(i + (nDigits + 1), number.length());
                number = number.substring(0, i);
            }
        }

        return number;
    }

    public static String parseAndKeepOnlyNDigits(String s, int nDigits) {
        int index = s.lastIndexOf(" ");
        String prefix = s.substring(0, index);
        String number = s.substring(index + 1);
        number = MBeanChart.keepNDigitsAfterComma(number, 2);
        return prefix + " " + number;
    }

    public static String[] VOLUME_UNITS = new String[] { "Kb", "Mb", "Gb", "Tb" };

    public static String[] THROUGHPUT_UNITS = new String[] { "b/s", "Kb/s", "Mb/s", "Gb/s", "Tb/s" };

    public static String addUnitDependsOnSize(String number, String[] units) {
        Double original = Double.parseDouble(number);
        Double copy = original;

        int i;
        for (i = 0; i < units.length - 1; ++i) {
            if (copy < 1024) {
                break;
            }
            copy /= 1024;
        }

        double value = original / Math.pow(1024, i);
        return keepNDigitsAfterComma(Double.toString(value), 3) + " " + units[i];

    }

}
