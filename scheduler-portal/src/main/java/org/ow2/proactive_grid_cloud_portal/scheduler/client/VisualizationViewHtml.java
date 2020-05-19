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
package org.ow2.proactive_grid_cloud_portal.scheduler.client;

import java.util.HashMap;
import java.util.List;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.JobsController;
import org.ow2.proactive_grid_cloud_portal.scheduler.shared.JobVisuMap;

import com.google.gwt.dom.client.Element;
import com.google.gwt.dom.client.NodeList;
import com.google.gwt.event.dom.client.LoadEvent;
import com.google.gwt.user.client.ui.HTML;
import com.google.gwt.user.client.ui.HasHorizontalAlignment;
import com.google.gwt.user.client.ui.HorizontalPanel;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;


/**
 * Displays an html of the currently selected job if available
 *
 */
public class VisualizationViewHtml implements VisualizationView {

    private Layout wrapper = new Layout();

    private HTML htmlPanel;

    private HashMap<String, Element> task2Dom = null;

    private IButton button = new IButton("Open in Studio");

    Job job = null;

    private List<Task> currentTasks = null;

    private static final String TASK_STATUS_PREFIX = "task-status-";

    private Label noJobSelectedMessage;

    private Label noVisualizationMessage;

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
        noVisualizationMessage.setVisible(false);
        button.setVisible(true);
        this.job = job;

    }

    public void jobUnselected() {
        htmlPanel.setHTML("");
        wrapper.setWidth100();
        wrapper.setHeight100();
        noJobSelectedMessage.setVisible(true);
        noVisualizationMessage.setVisible(false);
        button.setVisible(false);
    }

    public void visualizationUnavailable(String jobId) {
        htmlPanel.setHTML("");
        wrapper.setWidth100();
        wrapper.setHeight100();
        noVisualizationMessage.setVisible(true);
        noJobSelectedMessage.setVisible(false);
        button.setVisible(false);
    }

    public void tasksUpdating() {
    }

    public void tasksUpdated(List<Task> tasks, long totalTasks) {
        if (tasks != null) { // received HTML before tasks
            for (Task t : tasks) {
                if (task2Dom != null) {
                    String name = t.getName();
                    int i1 = name.indexOf('#');
                    if (i1 > -1)
                        name = name.substring(0, i1);

                    int i2 = name.indexOf('*');
                    if (i2 > -1)
                        name = name.substring(0, i2);

                    Element taskElem = task2Dom.get(name);
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

        tasksUpdated(currentTasks, currentTasks.size());
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
        HorizontalPanel hp = new HorizontalPanel();
        hp.setHorizontalAlignment(HasHorizontalAlignment.ALIGN_LEFT);
        hp.setWidth("40%");
        hp.setHeight(button.getHeight() + 5 + "px");
        button.setAutoFit(true);
        button.setVisible(false);
        button.addClickHandler(openStudio());
        hp.add(button);
        wrapper.addMember(hp);

        this.noVisualizationMessage = new Label("The graphical Visualization is not available for this Workflow. To get one for the next execution, please open the Workflow in the Studio, and store it in the Catalog.");
        this.noVisualizationMessage.setAlign(Alignment.CENTER);
        this.noVisualizationMessage.setWidth100();
        wrapper.addMember(noVisualizationMessage);

        htmlPanel = new HTML();
        htmlPanel.getElement().setId("html-view");
        wrapper.addMember(htmlPanel);

        layout.addMember(wrapper);
    }

    private ClickHandler openStudio() {
        return new ClickHandler() {
            @Override
            public void onClick(ClickEvent event) {
                com.google.gwt.user.client.Window.open(JobsController.STUDIO_URL + job.getId().toString(),
                                                       "_blank",
                                                       "");
            }
        };
    }

    private String extractCss(String html, String cssName) {
        int index = html.indexOf(cssName);
        if (index != -1) {
            String res = "";
            int i = cssName.length() + 1;
            while ((index + i) < html.length() && html.charAt(index + i) != ';') {
                res += html.charAt(index + i);
                i++;
            }
            return res;
        }

        return "";
    }

    @Override
    public void selectedJobUpdated(Job job) {
    }
}
