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

import java.util.Map;

import org.ow2.proactive_grid_cloud_portal.common.client.model.LogModel;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LoginModel;
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.NodeLabel;

import com.google.gwt.event.dom.client.KeyCodes;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.google.gwt.user.client.ui.TextArea;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;


public class ThreadDumpView implements RMListeners.NodesListener, RMListeners.NodeSelectedListener {

    private static final String NO_NODE_SELECTED_MESSAGE = "<h3>No node selected</h3>";

    private Label nodeLabel;

    private Label loadingLabel;

    private IButton nodeThreadDumpButton;

    private IButton rmThreadDumpButton;

    private TextArea threadDumpArea;

    private String nodeUrl;

    private String nodeSourceName;

    private String nodeHostName;

    private RMController controller;

    public ThreadDumpView(RMController controller) {
        this.controller = controller;
        RMEventDispatcher eventDispatcher = controller.getEventDispatcher();
        eventDispatcher.addNodesListener(this);
        eventDispatcher.addNodeSelectedListener(this);
    }

    public Canvas build() {
        initLabels();
        initThreadDumpButtons();
        initThreadDumpArea();
        return getCanvas();
    }

    @Override
    public void nodeUnselected() {
        this.nodeUrl = null;
        this.nodeSourceName = null;
        this.nodeHostName = null;

        this.nodeLabel.setContents(NO_NODE_SELECTED_MESSAGE);
        this.nodeLabel.setIcon("");
        this.nodeThreadDumpButton.disable();
        resetThreadDumpArea();
    }

    @Override
    public void nodeSelected(NodeSource.Host.Node node) {
        this.nodeUrl = node.getNodeUrl();
        this.nodeSourceName = node.getSourceName();
        this.nodeHostName = node.getHostName();

        this.nodeLabel.setContents("<h3>" + node.getNodeUrl() + "</h3>");
        this.nodeLabel.setIcon(node.getIcon());
        this.nodeThreadDumpButton.enable();
        resetThreadDumpArea();
    }

    @Override
    public void nodeSourceSelected(NodeSource ns) {
        this.nodeUrl = null;
        this.nodeHostName = null;

        this.nodeLabel.setContents(NO_NODE_SELECTED_MESSAGE);
        this.nodeLabel.setIcon("");
        this.nodeThreadDumpButton.disable();
        resetThreadDumpArea();
    }

    @Override
    public void hostSelected(NodeSource.Host h) {
        this.nodeUrl = null;
        this.nodeSourceName = null;

        this.nodeLabel.setContents(NO_NODE_SELECTED_MESSAGE);
        this.nodeLabel.setIcon("");
        this.nodeThreadDumpButton.disable();
        resetThreadDumpArea();
    }

    @Override
    public void nodesUpdated(Map<String, NodeSource> nodes) {
        NodeLabel.update(nodes, this.nodeLabel, this.nodeSourceName, this.nodeHostName, this.nodeUrl);
    }

    private void initLabels() {
        this.nodeLabel = new Label();
        this.nodeLabel.setHeight(25);
        this.nodeLabel.setContents(NO_NODE_SELECTED_MESSAGE);

        this.loadingLabel = new Label();
        this.loadingLabel.setHeight(25);
        this.loadingLabel.setIcon("loading.gif");
        this.loadingLabel.hide();
    }

    private void initThreadDumpButtons() {
        this.nodeThreadDumpButton = new IButton("Fetch Node's Thread Dump");
        this.nodeThreadDumpButton.setWidth("160px");
        this.nodeThreadDumpButton.addClickHandler(event -> {
            if (this.nodeUrl != null) {
                this.loadingLabel.show();
                this.controller.getRMService().getNodeThreadDump(LoginModel.getInstance().getSessionId(),
                                                                 this.nodeUrl,
                                                                 getThreadDumpCallback());
            }
        });
        this.nodeThreadDumpButton.disable();

        this.rmThreadDumpButton = new IButton("Fetch Resource Manager Thread Dump");
        this.rmThreadDumpButton.setWidth("220px");
        this.rmThreadDumpButton.addClickHandler(event -> {
            this.loadingLabel.show();
            this.controller.getRMService().getRMThreadDump(LoginModel.getInstance().getSessionId(),
                                                           getThreadDumpCallback());
        });
    }

    private void initThreadDumpArea() {
        this.threadDumpArea = new TextArea();
        this.threadDumpArea.setWidth("95%");
        this.threadDumpArea.setHeight("95%");
        this.threadDumpArea.setReadOnly(true);
        this.threadDumpArea.addDoubleClickHandler(handler -> this.threadDumpArea.selectAll());
        this.threadDumpArea.addKeyDownHandler(event -> {
            if (event.isControlKeyDown() && event.getNativeKeyCode() == KeyCodes.KEY_A) {
                this.threadDumpArea.selectAll();
            }
        });
    }

    private Canvas getCanvas() {
        HLayout fetchThreadDumpLayout = new HLayout();
        fetchThreadDumpLayout.setHeight("20px");
        fetchThreadDumpLayout.setMembersMargin(10);
        fetchThreadDumpLayout.addMember(this.nodeThreadDumpButton);
        fetchThreadDumpLayout.addMember(this.rmThreadDumpButton);
        fetchThreadDumpLayout.addMember(this.loadingLabel);

        VLayout threadDumpLayout = new VLayout(7);
        threadDumpLayout.setWidth100();
        threadDumpLayout.setHeight100();
        threadDumpLayout.setOverflow(Overflow.AUTO);
        threadDumpLayout.addMember(this.nodeLabel);
        threadDumpLayout.addMember(fetchThreadDumpLayout);
        threadDumpLayout.addMember(this.threadDumpArea);
        return threadDumpLayout;
    }

    private AsyncCallback<String> getThreadDumpCallback() {
        return new AsyncCallback<String>() {
            @Override
            public void onFailure(Throwable caught) {
                LogModel.getInstance().logCriticalMessage("Thread dump could not be fetched: " + caught.getMessage());
                ThreadDumpView.this.loadingLabel.hide();
                ThreadDumpView.this.threadDumpArea.setText("Thread dump could not be fetched. See Help -> Display logs for more details.");
            }

            @Override
            public void onSuccess(String result) {
                ThreadDumpView.this.loadingLabel.hide();
                ThreadDumpView.this.threadDumpArea.setText(result);
            }
        };
    }

    private void resetThreadDumpArea() {
        this.threadDumpArea.setText("");
    }

}
