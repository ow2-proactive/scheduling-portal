/*
 * ################################################################
 *
 * ProActive Parallel Suite(TM): The Java(TM) library for
 *    Parallel, Distributed, Multi-Core Computing for
 *    Enterprise Grids & Clouds
 *
 * Copyright (C) 1997-2015 INRIA/University of
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
package org.ow2.proactive_grid_cloud_portal.scheduler.client;

import java.util.HashMap;
import java.util.List;

import com.smartgwt.client.widgets.layout.VLayout;
import org.ow2.proactive_grid_cloud_portal.scheduler.shared.JobVisuMap;
import com.google.gwt.dom.client.Element;
import com.google.gwt.dom.client.NodeList;
import com.google.gwt.event.dom.client.LoadEvent;
import com.google.gwt.user.client.ui.HTML;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.layout.Layout;


/**
 * Displays an html of the currently selected job if available
 *
 */
public class VisualizationViewHtml implements VisualizationView {

    private Layout wrapper = new Layout();
    private HTML htmlPanel;
    private HashMap<String, Element> task2Dom = null;
    private List<Task> currentTasks = null;

    private static final String TASK_STATUS_PREFIX = "task-status-";
    private Label noJobSelectedMessage;

    public void imageUpdated(String jobId, String path) {
        hideHtml();
    }

    public void mapUpdated(String jobId, JobVisuMap map) {
        hideHtml();
    }

    public void onLoad(LoadEvent event) {
    }

    public void jobSelected(Job job) {
        noJobSelectedMessage.setVisible(false);
    }

    public void jobUnselected() {
        htmlPanel.setHTML("");
        wrapper.setWidth100();
        wrapper.setHeight100();
        noJobSelectedMessage.setVisible(true);
    }

    public void visualizationUnavailable(String jobId) {
        hideHtml();
    }

    public void tasksUpdating(boolean jobChanged) {
    }

    public void tasksUpdated(List<Task> tasks) {
        if (tasks != null) { // received HTML before tasks
            for (Task t : tasks) {
                if (task2Dom != null) {
                    Element taskElem = task2Dom.get(t.getName());
                    if (taskElem != null) {
                        String classes = taskElem.getClassName();
                        int statusIndex = classes.indexOf(TASK_STATUS_PREFIX);
                        if (statusIndex != -1) {
                            classes = classes.substring(0, statusIndex);
                        }

                        classes += " " + TASK_STATUS_PREFIX + String.valueOf(t.getStatus()).toLowerCase();
                        taskElem.setClassName(classes);
                    }
                }
            }
            currentTasks = tasks;
        }
    }

    public void tasksUpdatedFailure(String message) {
    }

    @Override
    public void htmlUpdated(String jobId, String html) {
        showHtml();
        htmlPanel.setHTML(html);

        String width = extractCss(html, "width");
        String height = extractCss(html, "height");
        wrapper.setSize(width, height);

        task2Dom = new HashMap<String, Element>();
        NodeList<Element> elements = htmlPanel.getElement().getElementsByTagName("div");
        if (elements != null) {
            for (int i = 0; i < elements.getLength(); i++) {
                Element elem = elements.getItem(i);
                if (elem.getClassName().contains("task")) {
                    // task div - finding it's name
                    String taskName = elem.getInnerText();
                    taskName = taskName.replaceAll("&nbsp", " ");
                    taskName = taskName.replaceAll(String.valueOf((char) 160), " ");
                    taskName = taskName.trim();
                    task2Dom.put(taskName, elem);
                }
            }
        }

        tasksUpdated(currentTasks);
    }

    private void showHtml() {
        if (htmlPanel != null) {
            wrapper.show();
        }
    }

    private void hideHtml() {
        if (htmlPanel != null) {
            wrapper.hide();
        }
    }

    public void setRoot(Layout layout) {

        wrapper = new VLayout();
        wrapper.setWidth100();
        wrapper.setHeight100();

        this.noJobSelectedMessage = new Label("No job selected.");
        this.noJobSelectedMessage.setAlign(Alignment.CENTER);
        this.noJobSelectedMessage.setWidth100();
        wrapper.addMember(noJobSelectedMessage);

        htmlPanel = new HTML();
        htmlPanel.getElement().setId("html-view");
        wrapper.addMember(htmlPanel);

        layout.addMember(wrapper);
    }

    private String extractCss(String html, String cssName) {
        int index = html.indexOf(cssName);
        if (index != -1) {
            String res = "";
            int i=cssName.length()+1;
            while ((index+i) < html.length() && html.charAt(index+i) != ';') {
                res += html.charAt(index+i);
                i++;
            }
            return res;
        }

        return "";
    }
}
