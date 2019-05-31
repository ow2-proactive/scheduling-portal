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
import java.util.List;

import org.ow2.proactive_grid_cloud_portal.common.client.Model;
import org.ow2.proactive_grid_cloud_portal.common.client.json.JSONUtils;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LogModel;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LoginModel;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMController;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMModel;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMServiceAsync;
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.Reloadable;
import org.pepstock.charba.client.AbstractChart;
import org.pepstock.charba.client.PieChart;
import org.pepstock.charba.client.data.Dataset;

import com.google.gwt.i18n.client.DateTimeFormat;
import com.google.gwt.json.client.JSONArray;
import com.google.gwt.json.client.JSONNumber;
import com.google.gwt.json.client.JSONObject;
import com.google.gwt.json.client.JSONValue;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.google.gwt.user.client.ui.AbsolutePanel;
import com.google.gwt.visualization.client.DataTable;
import com.google.gwt.visualization.client.LegendPosition;
import com.google.gwt.visualization.client.visualizations.corechart.CoreChart;
import com.google.gwt.visualization.client.visualizations.corechart.HorizontalAxisOptions;
import com.google.gwt.visualization.client.visualizations.corechart.Options;
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

    protected AbstractChart loadChart;

    protected DataTable loadTable;

    protected Options loadOpts;

    protected AbsolutePanel chartContainer;

    protected Model.StatHistory.Range timeRange = Model.StatHistory.Range.MINUTE_1;

    protected Runnable onFinish;

    public MBeanChart(RMController controller, String jmxServerUrl, String mbean, String[] attrs, String title) {
        this.controller = controller;
        this.jmxServerUrl = jmxServerUrl;
        this.mbeanName = mbean;
        this.attrs = attrs;

        loadOpts = Options.create();
        HorizontalAxisOptions loadAxis = HorizontalAxisOptions.create();
        loadAxis.setMaxAlternation(1);
        loadAxis.setSlantedText(false);
        loadOpts.setLegend(LegendPosition.NONE);
        loadOpts.setHAxisOptions(loadAxis);
        loadOpts.setColors("#fcaf3e", "#3a668d", "#35a849", "#fcaf3e", "#24c1ff", "#1e4ed7", "#ef2929", "#000000");
        loadAxis.setMinValue(0);

        loadTable = DataTable.create();

        setWidth100();
        setHeight100();

        if (title.length() > 0) {
            Label label = new Label("<nobr style='font-weight:bold;padding-left:10px;'>" + title + "<nobr>");
            label.setHeight(30);
            addMember(label);
        }

        chartContainer = new AbsolutePanel();
        chartContainer.setWidth("100%");
        chartContainer.setHeight("200px");

        loadChart = createChart(loadTable, loadOpts);
        loadChart.setWidth("100%");
        loadChart.setHeight("200px");
        chartContainer.add(loadChart);
        if (!(loadChart instanceof PieChart)) {
            addMember(getTimeSlotSelector());
        }
        addMember(chartContainer);
    }

    @Override
    public void reload() {
        final RMServiceAsync rm = controller.getRMService();
        final RMModel model = controller.getModel();
        final long t = System.currentTimeMillis();

        final boolean realTime = timeRange.equals(Model.StatHistory.Range.MINUTE_1);

        final LoginModel loginModel = LoginModel.getInstance();

        AsyncCallback callback = new AsyncCallback<String>() {
            public void onSuccess(String result) {
                if (onFinish != null) {
                    onFinish.run();
                }
                if (!loginModel.isLoggedIn())
                    return;

                LogModel.getInstance().logMessage("Fetched " + mbeanName + ":" + Arrays.toString(attrs) + " in " +
                                                  (System.currentTimeMillis() - t) + "ms");

                //                if (realTime) {
                //                    processResult(result);
                //                } else {
                processHistoryResult(result);
                //                }
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
                LogModel.getInstance().logCriticalMessage(e.getMessage());
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

    public void processHistoryResult(String result) {

        // removing internal escaping
        result = result.replace("\\\"", "\"");
        result = result.replace("\"{", "{");
        result = result.replace("}\"", "}");

        JSONValue resultVal = controller.parseJSON(result);
        JSONObject json = resultVal.isObject();

        if (json == null) {
            return;
        }

        loadTable.removeRows(0, loadTable.getNumberOfRows());
        long now = new Date().getTime() / 1000;
        long dur = timeRange.getDuration();
        long size = getJsonInternalSize(json);
        long step = dur / size;

        final Dataset dataset = loadChart.newDataset();

        List<Double> dps = new ArrayList<>();
        List<String> labels = new ArrayList<>();
        for (int i = 0; i < size; i++) {

            double[] slice = getJsonSlice(json, i);
            long t = now - dur + step * i;
            DateTimeFormat.PredefinedFormat format = timeRange.getFormat();
            String timeStamp = DateTimeFormat.getFormat(format).format(new Date(t * 1000));

            labels.add(timeStamp);

            //            for (int sliceIndex = 0; sliceIndex < slice.length; sliceIndex++) {
            //                loadTable.setValue(i, sliceIndex + 1, formatValue(slice[sliceIndex]));
            //            }
            dps.add(formatValue(slice[0]));
        }

        loadChart.getData().setLabels(labels.toArray(new String[0]));
        dataset.setData(dps);

        loadChart.getData().setDatasets(dataset);
        loadChart.update();
        //        loadChart.draw(loadTable, loadOpts);
    }

    protected void addRow() {
        if (loadTable.getNumberOfRows() > MAX_ROWS_NUMBER) {
            loadTable.removeRow(0);
        }
        loadTable.addRow();
    }

    public abstract AbstractChart createChart(DataTable data, Options opts);

    public abstract void processResult(String result);

    public double formatValue(double value) {
        return value;
    }

    public void onFinish(Runnable onFinish) {
        this.onFinish = onFinish;
    }

    public DynamicForm getTimeSlotSelector() {
        DynamicForm form = new DynamicForm();

        final SelectItem selectedRange = new SelectItem("statRange", "");
        LinkedHashMap<String, String> nodeLineValues = new LinkedHashMap<String, String>();
        for (Model.StatHistory.Range r : Model.StatHistory.Range.values()) {
            nodeLineValues.put("" + r.getChar(), r.getString());
        }
        selectedRange.setDefaultValue("" + Model.StatHistory.Range.MINUTE_1.getChar());
        selectedRange.setValueMap(nodeLineValues);

        selectedRange.addChangedHandler(new ChangedHandler() {
            @Override
            public void onChanged(ChangedEvent event) {
                timeRange = Model.StatHistory.Range.create(selectedRange.getValueAsString().charAt(0));
                loadTable.removeRows(0, loadTable.getNumberOfRows());
                reload();
            }
        });

        form.setItems(selectedRange);
        form.setHeight(24);
        form.setWidth(40);

        return form;
    }
}
