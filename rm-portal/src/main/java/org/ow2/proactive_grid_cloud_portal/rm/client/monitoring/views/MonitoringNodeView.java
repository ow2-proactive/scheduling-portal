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
package org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.views;

import java.util.Arrays;
import java.util.LinkedHashMap;

import org.ow2.proactive_grid_cloud_portal.common.client.Model;
import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSource.Host.Node;
import org.ow2.proactive_grid_cloud_portal.rm.client.NodeState;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMController;
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.charts.ClassesAreaChart;
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.charts.CpuUsageAreaChart;
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.charts.JVMMemoryAreaChart;
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.charts.MBeanChart;
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.charts.MBeanDetailedView;
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.charts.ThreadsAreaChart;
import org.ow2.proactive_grid_cloud_portal.rm.shared.RMConfig;

import com.google.gwt.user.client.Timer;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.tab.Tab;
import com.smartgwt.client.widgets.tab.TabSet;


/**
 * Node monitoring view.
 */
public class MonitoringNodeView extends VLayout implements AsyncCallback<String> {

    private Timer updater = null;

    private ReloadableChain chain;

    private TabSet tabs;

    private Label status;

    private RMController controller;

    Tab t2; //JVM Summary tab

    public MonitoringNodeView(RMController controller) {
        this.controller = controller;
    }

    public static Node previousSelectedNode = null;

    protected Model.StatHistory.Range timeRange = Model.StatHistory.Range.MINUTE_1;

    private MBeanChart heapMemory;

    private MBeanChart threads;

    private MBeanChart classes;

    private MBeanChart cpuUsage;

    private static VLayout selectedRangeLayout;

    public void init(Node node) {

        if (node.getNodeState() == NodeState.BUSY || node.getNodeState() == NodeState.FREE) {
            // good
        } else {
            // cannot monitor the node
            return;
        }

        setWidth100();
        String nodeUrl = node.getDefaultJMXUrl();
        if (!RMConfig.MONITORING_PROTOCOL_DEFAULT.equals(RMConfig.get().getMonitoringProtocol())) {
            nodeUrl = node.getProactiveJMXUrl();
        }

        heapMemory = new JVMMemoryAreaChart(controller, nodeUrl);
        threads = new ThreadsAreaChart(controller, nodeUrl);
        classes = new ClassesAreaChart(controller, nodeUrl);
        cpuUsage = new CpuUsageAreaChart(controller, nodeUrl);

        if (!node.checkNodeDetailsEquals(previousSelectedNode)) {
            t2 = new Tab("JVM Summary");
            String[] jvmAttrs = { "ManagementSpecVersion", "Name", "SpecName", "SpecVendor", "StartTime", "Uptime",
                                  "VmName", "VmVendor", "VmVersion", "BootClassPath", "ClassPath", "LibraryPath" };

            MBeanDetailedView jvmDetails = new MBeanDetailedView(this);
            jvmDetails.setCanSelectText(true);
            jvmDetails.load(controller, nodeUrl, "java.lang:type=Runtime", Arrays.asList(jvmAttrs));
            jvmDetails.setWidth100();

            t2.setPane(jvmDetails);
            previousSelectedNode = node;
        }
        chain = new ReloadableChain(heapMemory, threads, classes, cpuUsage);

        HLayout firstRow = new HLayout();
        HLayout secondRow = new HLayout();

        firstRow.addMember(heapMemory);
        firstRow.addMember(threads);
        secondRow.addMember(classes);
        secondRow.addMember(cpuUsage);

        Layout graphs = new VLayout();
        graphs.addMember(firstRow);
        graphs.addMember(secondRow);

        if (status != null) {
            removeMember(status);
        }
        removeMember(selectedRangeLayout);
        status = new Label("Retreiving data");
        status.setWidth100();
        status.setAlign(Alignment.CENTER);

        Tab t1 = new Tab("Overview");
        t1.setPane(graphs);

        tabs = new TabSet();
        tabs.setWidth100();
        tabs.setHeight100();
        tabs.setShowResizeBar(true);
        tabs.setTabs(t1, t2);
        tabs.hide();

        addMember(status);

        updater = new Timer() {
            @Override
            public void run() {
                chain.reload();
            }
        };
        updater.scheduleRepeating(RMConfig.get().getMonitoringPeriod());
        // Get two values right now to quickly display the charts
        chain.onFinish(new Runnable() {
            @Override
            public void run() {
                chain.onFinish(null);
                chain.reload();
            }
        });
        chain.reload();
        controller.checkNodePermission(null, false, node.getNodeUrl());
    }

    public VLayout getTimeSlotSelector() {
        selectedRangeLayout = new VLayout();
        DynamicForm dynamicForm = new DynamicForm();

        final SelectItem selectedRange = new SelectItem("statRange", "");
        LinkedHashMap<String, String> nodeLineValues = new LinkedHashMap<>();
        for (Model.StatHistory.Range r : Model.StatHistory.Range.values()) {
            nodeLineValues.put("" + r.getChar(), r.getString());
        }
        selectedRange.setDefaultValue("" + MonitoringHostView.getPreviousRange().getChar());
        selectedRange.setValueMap(nodeLineValues);

        selectedRange.addChangedHandler(event -> {
            timeRange = Model.StatHistory.Range.create(selectedRange.getValueAsString().charAt(0));
            heapMemory.selectRange(timeRange);
            threads.selectRange(timeRange);
            classes.selectRange(timeRange);
            cpuUsage.selectRange(timeRange);
            MonitoringHostView.setPreviousRange(Model.StatHistory.Range.create(selectedRange.getValueAsString()
                                                                                            .charAt(0)));
        });

        dynamicForm.setItems(selectedRange);
        dynamicForm.setHeight(20);
        dynamicForm.setWidth(40);
        selectedRangeLayout.addMember(dynamicForm);
        selectedRangeLayout.setDefaultLayoutAlign(Alignment.RIGHT);

        return selectedRangeLayout;
    }

    public void close() {
        try {
            if (updater != null) {
                updater.cancel();
                updater = null;
            }

            if (tabs != null) {
                removeMember(tabs);
                chain.stopReloading();
                tabs.destroy();
                tabs = null;
            }
        } catch (Exception e) {
            // ignore it
        }
    }

    @Override
    public void onFailure(Throwable caught) {
        close();
        status.setContents("Monitoring is unavailable on this node. <br>" +
                           "Either monitoring was disabled with the disableMonitoring option or an error occurred. <br>" +
                           "More information are available inside the node logs.");
        removeMember(selectedRangeLayout);
        selectedRangeLayout.destroy();
        controller.getRmPage().setMonitoringTabPageDisabled(true);
    }

    @Override
    public void onSuccess(String result) {
        removeMember(status);
        status.destroy();
        addMember(getTimeSlotSelector());
        if (tabs != null) {
            addMember(tabs);
            tabs.show();
        }
        controller.getRmPage().setMonitoringTabPageDisabled(false);
    }
}
