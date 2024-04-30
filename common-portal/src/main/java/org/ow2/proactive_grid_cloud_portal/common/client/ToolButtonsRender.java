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

import com.google.gwt.core.client.GWT;
import com.google.gwt.resources.client.ImageResource;
import com.google.gwt.user.client.Window;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;


public class ToolButtonsRender {
    private static final String GREY_BUTTON_BORDER = "1px solid #858585";

    private static final String BASE_MODULE_PATH = "/rm";

    public ToolStripButton getSchedulerLinkButton() {
        return getToolStripButton(Images.instance.scheduler_30(), "Scheduling & Orchestration", "scheduler/");
    }

    public ToolStripButton getSchedulerHighlightedLinkButton() {
        return getToolStripButtonHighlighted(Images.instance.scheduler_30(),
                                             "Scheduling & Orchestration",
                                             "scheduler/");
    }

    public ToolStripButton getStudioLinkButton() {
        ImageResource imageResource = Images.instance.studio_30();
        String title = "Workflow Studio";
        String url = "studio/#workflows";

        return getToolStripButton(imageResource, title, url);
    }

    public ToolStripButton getResourceManagerLinkButton() {
        return getToolStripButton(Images.instance.rm_30(), "Resource Manager", "rm/");
    }

    public ToolStripButton getResourceManagerHighlightedLinkButton() {
        return getToolStripButtonHighlighted(Images.instance.rm_30(), "Resource Manager", "rm/");
    }

    public ToolStripButton getAutomationDashboardLinkButton() {
        return getToolStripButton(Images.instance.automation_dashboard_30(),
                                  "Automation Dashboard",
                                  "automation-dashboard/#/workflow-execution");
    }

    public ToolStripButton getLogoutButton(String login, final Controller controller) {
        ToolStripButton logoutButton = getSimpleToolStripButton(Images.instance.logout_30(), login);
        logoutButton.setIconOrientation("right");
        logoutButton.setTooltip("Logout");
        logoutButton.setBorder(GREY_BUTTON_BORDER);

        logoutButton.addClickHandler(event -> SC.confirm("Logout", "Are you sure you want to exit?", value -> {
            if (value) {
                controller.logout();
            }
        }));
        return logoutButton;
    }

    private ToolStripButton getToolStripButton(ImageResource imageResource, String title, final String url) {
        ToolStripButton toolStripButton = getToolStripButtonWithoutBorder(imageResource, title, url);

        toolStripButton.setBorder(GREY_BUTTON_BORDER);
        return toolStripButton;
    }

    private ToolStripButton getToolStripButtonHighlighted(ImageResource imageResource, String title, final String url) {
        // Workaround to change text color of a button in smartgwt
        String inlineStyledTitle = "<span style='color:#ffffff;'>" + title + "</span>";
        ToolStripButton toolStripButton = getToolStripButtonWithoutBorder(imageResource, inlineStyledTitle, url);

        toolStripButton.setBorder(GREY_BUTTON_BORDER);
        toolStripButton.setBackgroundColor("#E86D1F");
        toolStripButton.setBaseStyle("toolStripButton no-hover");
        return toolStripButton;
    }

    private ToolStripButton getToolStripButtonWithoutBorder(ImageResource imageResource, String title,
            final String url) {
        ToolStripButton toolStripButton = getSimpleToolStripButton(imageResource, title);
        String baseUrl = GWT.getHostPageBaseURL().replace(BASE_MODULE_PATH, "");
        String absoluteUrl = baseUrl + url;
        toolStripButton.addClickHandler(event -> Window.open(absoluteUrl, absoluteUrl, ""));
        return toolStripButton;
    }

    private ToolStripButton getSimpleToolStripButton(ImageResource imageResource, String title) {
        ToolStripButton toolStripButton = new ToolStripButton(title);
        toolStripButton.setIcon(imageResource.getSafeUri().asString());
        toolStripButton.setIconSize(25);
        return toolStripButton;
    }
}
