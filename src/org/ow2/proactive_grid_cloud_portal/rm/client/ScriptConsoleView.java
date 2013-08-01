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
package org.ow2.proactive_grid_cloud_portal.rm.client;

import java.util.Arrays;

import org.ow2.proactive_grid_cloud_portal.common.client.JSUtil;
import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSource.Host;
import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSource.Host.Node;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMListeners.NodeSelectedListener;

import com.google.codemirror2_gwt.client.CodeMirrorConfig;
import com.google.codemirror2_gwt.client.CodeMirrorWrapper;
import com.google.gwt.core.client.Callback;
import com.google.gwt.dom.client.Document;
import com.google.gwt.event.logical.shared.AttachEvent;
import com.google.gwt.user.client.ui.TextArea;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.RadioGroupItem;
import com.smartgwt.client.widgets.form.fields.events.ChangeEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangeHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;


/**
 * 
 * Allows to execute a script on a node and see its output.
 * 
 * 4 script engines are supported and available from every RM's node:
 * "Groovy", "JavaScript", "Ruby", "Python"
 * 
 */
public class ScriptConsoleView implements NodeSelectedListener {

    private Label label = null;

    private VLayout nodeCanvas = null;
    private Label nodeLabel = null;
    private Label outputLabel = null;
    private Label outputText = null;
    private TextArea scriptArea = null;
    private IButton execute = null;
    private String nodeUrl = null;
    private Label loadingLabel = null;
    private CodeMirrorWrapper codeMirror = null;

    private static final String[] engineNames = { "Groovy", "JavaScript", "Ruby", "Python" };
    private static final String[] engineCodeHighliters = { "text/x-groovy", "text/javascript", "text/x-ruby",
            "text/x-python" };
    private RadioGroupItem selectedEngine;

    private RMController controller;

    ScriptConsoleView(RMController controller) {
        this.controller = controller;
        controller.getEventDispatcher().addNodeSelectedListener(this);
    }

    Canvas build() {
        VLayout vl = new VLayout();
        vl.setOverflow(Overflow.AUTO);

        scriptArea = new TextArea();
        scriptArea.setWidth("97%");
        scriptArea.setHeight("300px");
        scriptArea.getElement().setId("highlighted-text-area");

        this.execute = new IButton("Execute");
        this.execute.addClickHandler(new ClickHandler() {
            @Override
            public void onClick(ClickEvent event) {
                if (nodeUrl != null) {
                    loadingLabel.show();
                    String engine = selectedEngine.getValueAsString();
                    engine = engine.toLowerCase();

                    controller.executeScript(codeMirror.getValue(), engine, nodeUrl,
                            new Callback<String, String>() {
                                @Override
                                public void onSuccess(String result) {
                                    loadingLabel.hide();
                                    outputLabel.show();
                                    outputLabel.setContents("<h3>Output:</h3>");
                                    outputText.setContents(result.replaceAll("\n", "<br/>"));
                                }

                                @Override
                                public void onFailure(String reason) {
                                    loadingLabel.hide();
                                    outputLabel.hide();
                                    outputText.setContents(reason.replaceAll("\n", "<br/>"));
                                }
                            });
                }
            }
        });

        this.label = new Label("No node selected");
        this.label.setWidth100();
        this.label.setAlign(Alignment.CENTER);

        this.loadingLabel = new Label();
        this.loadingLabel.setIcon("loading.gif");
        this.loadingLabel.setHeight(25);

        this.outputLabel = new Label();
        this.outputLabel.setHeight(20);
        this.outputText = new Label();
        this.outputText.setHeight(30);
        this.outputText.setCanSelectText(true);

        this.nodeCanvas = new VLayout(7);
        this.nodeCanvas.setWidth100();
        this.nodeCanvas.setHeight100();
        this.nodeLabel = new Label();
        this.nodeLabel.setIcon(RMImages.instance.node_add_16().getSafeUri().asString());
        this.nodeLabel.setHeight(16);

        String helpMessage = "Type in an arbitrary script and execute it on the node.";
        helpMessage += "Useful for trouble-shooting and diagnostics.";
        helpMessage += "Use the 'println' command to see the output ";
        helpMessage += "(if you use System.out, it will go to the server's stdout, which is harder to see.) Groovy example:<br/><br/>";
        helpMessage += "<div style='padding-left:20px'>println System.getenv(\"PATH\")</div>";
        helpMessage += "<div style='padding-left:20px'>println \"uname -a\".execute().text</div>";
        helpMessage += "<div style='padding-left:20px;padding-bottom:10px'>println \"jstack pid\".execute().text</div>";

        Label help = new Label(helpMessage);
        help.setHeight(16);
        help.setCanSelectText(true);

        selectedEngine = new RadioGroupItem();
        selectedEngine.setValue("Groovy");
        selectedEngine.setShowTitle(false);
        selectedEngine.setVertical(false);
        selectedEngine.setTitle("engine");
        selectedEngine.setValueMap(engineNames);

        HLayout executeAndLoading = new HLayout();
        executeAndLoading.setHeight("40px");

        executeAndLoading.addMember(execute);
        executeAndLoading.addMember(loadingLabel);
        loadingLabel.hide();

        final DynamicForm form = new DynamicForm();
        form.setFields(selectedEngine);

        this.nodeCanvas.addMember(nodeLabel);
        this.nodeCanvas.addMember(help);
        this.nodeCanvas.addMember(form);
        this.nodeCanvas.addMember(scriptArea);
        this.nodeCanvas.addMember(executeAndLoading);
        this.nodeCanvas.addMember(outputLabel);
        this.nodeCanvas.addMember(outputText);

        this.nodeCanvas.hide();

        scriptArea.addAttachHandler(new com.google.gwt.event.logical.shared.AttachEvent.Handler() {
            @Override
            public void onAttachOrDetach(AttachEvent event) {
                initCodeMirror();
            }
        });

        selectedEngine.addChangeHandler(new ChangeHandler() {
            @Override
            public void onChange(ChangeEvent event) {
                if (codeMirror != null) {
                    String engine = event.getValue().toString();
                    String highliter = engineCodeHighliters[Arrays.asList(engineNames).indexOf(engine)];
                    codeMirror.setOption("mode", highliter);
                }
            }
        });

        JSUtil.addStyle("codemirror-3.14/lib/codemirror.css");
        JSUtil.addScript("codemirror-3.14/lib/codemirror.js",
                "codemirror-3.14/addon/comment/comment.js",
                "codemirror-3.14/addon/edit/matchbrackets.js",
                "codemirror-3.14/mode/groovy/groovy.js",
                "codemirror-3.14/mode/javascript/javascript.js",
                "codemirror-3.14/mode/ruby/ruby.js",
                "codemirror-3.14/mode/python/python.js");

        vl.setMembers(label, nodeCanvas);
        return vl;
    }

    public void initCodeMirror() {

        if (codeMirror == null && Document.get().getElementById("highlighted-text-area") != null) {
            CodeMirrorConfig config = CodeMirrorConfig.makeBuilder();
            config = config.setMode(engineCodeHighliters[0]).setShowLineNumbers(true).setMatchBrackets(true);
            codeMirror = CodeMirrorWrapper.createEditor(scriptArea.getElement(), config);
        }
    }

    public void nodeUnselected() {
        this.label.setContents("No node selected");
        this.label.setAlign(Alignment.CENTER);
        this.label.show();
        this.nodeCanvas.hide();
        this.nodeUrl = null;
    }

    public void nodeSelected(Node node) {
        this.nodeLabel.setIcon(node.getNodeState().getIcon());

        this.label.hide();

        this.nodeUrl = node.getNodeUrl();
        this.nodeLabel.setContents("<h3>" + node.getNodeUrl() + "</h3>");
        this.nodeCanvas.show();
    }

    public void nodeSourceSelected(NodeSource ns) {
        this.nodeCanvas.hide();
        this.label.show();
    }

    public void hostSelected(Host h) {
        this.nodeCanvas.hide();
        this.label.show();
    }
}
