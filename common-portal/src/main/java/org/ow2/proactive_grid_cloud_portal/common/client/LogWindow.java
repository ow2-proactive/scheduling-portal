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
package org.ow2.proactive_grid_cloud_portal.common.client;

import org.ow2.proactive_grid_cloud_portal.common.client.Listeners.LogListener;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LogModel;

import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.widgets.HTMLPane;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;


/**
 * Displays logs issued by the event dispatcher,
 * mostly async callback from the scheduler rpc
 * 
 * 
 * @author mschnoor
 *
 */
public class LogWindow implements LogListener {

    /** root layout */
    private Window window = null;

    private Controller controller = null;

    /** log content */
    private String fullContent = "";

    /** log content with only important events */
    private String briefContent = "";

    /** contains the log content */
    private HTMLPane text = null;

    /** current line */
    private int fullLineCount = 0;

    private int briefLineCount = 0;

    /** display all messages / only important */
    private boolean displayAllMessages = false;

    public LogWindow(Controller controller) {
        this.controller = controller;
        LogModel.getInstance().addLogListener(this);
        this.build();
    }

    public void show() {
        this.window.show();
    }

    public void hide() {
        this.window.hide();
    }

    public boolean isVisible() {
        return this.window.isVisible();
    }

    private void clear() {
        this.briefContent = "";
        this.fullContent = "";
        fullLineCount = 0;
        briefLineCount = 0;
        this.text.setContents(" "); // whitespace otherwise they won't be replaced in text panel
    }

    public void destroy() {
        this.hide();
        this.window.destroy();
    }

    @Override
    public void logMessage(String message) {
        fullLineCount++;
        String style = "padding: 3px;";
        if (fullLineCount % 2 == 0) {
            style += "background-color:#FAFAFA; border-bottom: 1px solid #EDEDED; border-top: 1px solid #EDEDED;";
        }
        this.fullContent += "<div style='" + style + "' >" + message + "</div>";
        if (fullLineCount > 100)
            fullContent = fullContent.substring(fullContent.indexOf("</div>") + 6);

        if (this.displayAllMessages) {
            this.text.setContents(this.fullContent + "<br>");
            this.text.scrollToBottom();
        }
    }

    @Override
    public void logImportantMessage(String message) {
        briefLineCount++;
        String style = "padding: 3px;width:100%;";
        if (briefLineCount % 2 == 0) {
            style += "background-color:#FAFAFA; border-bottom: 1px solid #EDEDED; border-top: 1px solid #EDEDED;";
        }
        this.briefContent += "<div style='" + style + "' >" + message + "</div>";
        if (briefLineCount > 100)
            briefContent = briefContent.substring(briefContent.indexOf("</div>") + 6);

        this.text.setContents(this.briefContent + "<br>");

        // add to verbose log also
        this.logMessage(message);
    }

    @Override
    public void logCriticalMessage(String message) {
        briefLineCount++;
        fullLineCount++;

        String style = "background-color:#FFAAAA; padding: 3px;width:100%;";
        String line = "<div style='" + style + "' >" + message + "</div>";

        fullContent += line;
        briefContent += line;

        if (fullLineCount > 100)
            fullContent = fullContent.substring(fullContent.indexOf("</div>") + 6);
        if (briefLineCount > 100)
            briefContent = briefContent.substring(briefContent.indexOf("</div>") + 6);

        if (this.displayAllMessages) {
            this.text.setContents(this.fullContent + "<br>");
        } else {
            this.text.setContents(this.briefContent + "<br>");
        }

        this.text.scrollToBottom();
    }

    private void build() {
        VLayout root = new VLayout();
        root.setWidth100();
        root.setHeight100();
        root.setMembersMargin(5);
        root.setPadding(5);

        this.text = new HTMLPane();
        this.text.setOverflow(Overflow.AUTO);
        this.text.setHeight100();
        this.text.setBorder("1px solid #ccc");
        //this.text.setShowEdges(true);

        IButton close = new IButton("Close");
        close.setIcon(Images.instance.cancel_16().getSafeUri().asString());
        close.addClickHandler(new ClickHandler() {
            public void onClick(ClickEvent event) {
                hide();
            }
        });
        IButton clear = new IButton("Clear");
        clear.setIcon(Images.instance.clear_16().getSafeUri().asString());
        clear.addClickHandler(new ClickHandler() {
            public void onClick(ClickEvent event) {
                clear();
            }
        });

        DynamicForm form = new DynamicForm();
        final CheckboxItem showAll = new CheckboxItem("showAll", "Show all events");
        showAll.addChangedHandler(new ChangedHandler() {
            @Override
            public void onChanged(ChangedEvent event) {
                displayAllMessages = showAll.getValueAsBoolean();
                text.setContents(((displayAllMessages) ? fullContent : briefContent) + "<br>");
                text.scrollToBottom();
            }
        });
        showAll.setValue(false);
        form.setItems(showAll);
        form.setWidth100();
        form.setColWidths("20", "*");

        HLayout vl = new HLayout();
        vl.setLayoutLeftMargin(5);
        vl.setHeight(clear.getHeight());
        vl.setWidth100();
        vl.setMembersMargin(5);
        vl.setMembers(form, clear, close);

        root.addMember(this.text);
        root.addMember(vl);

        int cw = com.google.gwt.user.client.Window.getClientWidth();
        int ch = com.google.gwt.user.client.Window.getClientHeight();
        int winWidth = cw * 50 / 100;
        int winHeight = ch * 80 / 100;
        int winLeft = cw * 30 / 100;
        winWidth = Math.min(1000, winWidth);
        winHeight = Math.min(1000, winHeight);

        this.window = new Window();
        this.window.setTitle("Logs");
        // this.window.setShowMinimizeButton(false);
        this.window.setShowShadow(true);
        // this.window.setIsModal(true);
        // this.window.setShowModalMask(true);
        this.window.addItem(root);
        this.window.setHeight(winWidth);
        this.window.setWidth(winHeight);
        this.window.centerInPage();
        this.window.setLeft(winLeft);
        this.window.setCanDragReposition(true);
        this.window.setCanDragResize(true);
    }
}
