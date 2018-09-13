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
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;


public class ThreadDumpView implements RMListeners.NodesListener, RMListeners.NodeSelectedListener {

    private Label noNodeLabel;

    private Label nodeLabel;

    private Label loadingLabel;

    private IButton nodeThreadDumpButton;

    private IButton rmThreadDumpButton;

    private TextArea threadDumpArea;

    private VLayout layout;

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
        VLayout canvas = new VLayout();
        canvas.setOverflow(Overflow.AUTO);

        createThreadDumpArea();
        createThreadDumpButtons();
        createLabels();

        HLayout fetchThreadDumpLayout = new HLayout();
        fetchThreadDumpLayout.setHeight("20px");
        fetchThreadDumpLayout.setMembersMargin(10);
        fetchThreadDumpLayout.addMember(this.nodeThreadDumpButton);
        fetchThreadDumpLayout.addMember(this.rmThreadDumpButton);
        fetchThreadDumpLayout.addMember(this.loadingLabel);
        createLayout(fetchThreadDumpLayout);

        canvas.setMembers(this.noNodeLabel, this.layout);
        return canvas;
    }

    public void nodeUnselected() {
        this.noNodeLabel.setContents("No node selected");
        this.noNodeLabel.setAlign(Alignment.CENTER);
        this.noNodeLabel.show();
        this.layout.hide();
        this.nodeUrl = null;
        this.nodeSourceName = null;
        this.nodeHostName = null;
    }

    public void nodeSelected(NodeSource.Host.Node node) {
        this.nodeLabel.setIcon(node.getIcon());

        this.noNodeLabel.hide();

        this.nodeUrl = node.getNodeUrl();
        this.nodeSourceName = node.getSourceName();
        this.nodeHostName = node.getHostName();
        this.nodeLabel.setContents("<h3>" + node.getNodeUrl() + "</h3>");
        resetThreadDumpArea();

        this.layout.show();
    }

    public void nodeSourceSelected(NodeSource ns) {
        this.nodeUrl = null;
        this.layout.hide();
        this.noNodeLabel.show();
    }

    public void hostSelected(NodeSource.Host h) {
        this.nodeUrl = null;
        this.layout.hide();
        this.noNodeLabel.show();
    }

    @Override
    public void nodesUpdated(Map<String, NodeSource> nodes) {
        NodeLabel.update(nodes, this.nodeLabel, this.nodeSourceName, this.nodeHostName, this.nodeUrl);
    }

    private void createLayout(HLayout executeAndLoading) {
        this.layout = new VLayout(7);
        this.layout.setWidth100();
        this.layout.setHeight100();

        this.layout.addMember(this.nodeLabel);
        this.layout.addMember(executeAndLoading);
        this.layout.addMember(this.threadDumpArea);

        this.layout.hide();
    }

    private void createThreadDumpButtons() {
        this.nodeThreadDumpButton = new IButton("Fetch Node's Thread Dump");
        this.nodeThreadDumpButton.setWidth("220px");
        this.nodeThreadDumpButton.addClickHandler(event -> {
            if (this.nodeUrl != null) {
                String nodeUrl = this.nodeUrl;
                this.loadingLabel.show();
                this.controller.getRMService().getNodeThreadDump(LoginModel.getInstance().getSessionId(),
                                                                 nodeUrl,
                                                                 new AsyncCallback<String>() {
                                                                     @Override
                                                                     public void onFailure(Throwable caught) {
                                                                         LogModel.getInstance()
                                                                                 .logCriticalMessage(caught.getMessage());
                                                                         ThreadDumpView.this.loadingLabel.hide();
                                                                         ThreadDumpView.this.threadDumpArea.setText("Thread dump could not be fetched. See Help -> Display logs for more details.");
                                                                     }

                                                                     @Override
                                                                     public void onSuccess(String result) {
                                                                         ThreadDumpView.this.loadingLabel.hide();
                                                                         ThreadDumpView.this.threadDumpArea.setText(result);
                                                                     }
                                                                 });
            }
        });

        this.rmThreadDumpButton = new IButton("Fetch Resource Manager Thread Dump");
        this.rmThreadDumpButton.setWidth("220px");
        this.rmThreadDumpButton.addClickHandler(event -> {
            this.loadingLabel.show();
            this.controller.getRMService().getRMThreadDump(LoginModel.getInstance().getSessionId(),
                                                           new AsyncCallback<String>() {
                                                               @Override
                                                               public void onFailure(Throwable caught) {
                                                                   LogModel.getInstance()
                                                                           .logCriticalMessage(caught.getMessage());
                                                                   ThreadDumpView.this.loadingLabel.hide();
                                                                   ThreadDumpView.this.threadDumpArea.setText("Thread dump could not be fetched. See Help -> Display logs for more details.");
                                                               }

                                                               @Override
                                                               public void onSuccess(String result) {
                                                                   ThreadDumpView.this.loadingLabel.hide();
                                                                   ThreadDumpView.this.threadDumpArea.setText(result);
                                                               }
                                                           });
        });
    }

    private void createLabels() {
        this.noNodeLabel = new Label("No node selected");
        this.noNodeLabel.setWidth100();
        this.noNodeLabel.setAlign(Alignment.CENTER);

        this.nodeLabel = new Label();
        this.nodeLabel.setIcon(RMImages.instance.node_add_16().getSafeUri().asString());
        this.nodeLabel.setHeight(16);

        this.loadingLabel = new Label();
        this.loadingLabel.setIcon("loading.gif");
        this.loadingLabel.setHeight(25);
        this.loadingLabel.hide();
    }

    private void createThreadDumpArea() {
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

    private void resetThreadDumpArea() {
        this.threadDumpArea.setText("");
    }

}
