/*
 *  *
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
 *  * $$ACTIVEEON_INITIAL_DEV$$
 */
package org.ow2.proactive_grid_cloud_portal.common.client;

import com.google.gwt.user.client.Window;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

public class ToolButtonsRender {
    private static final String GREY_BUTTON_BORDER = "1px solid #858585";

    public ToolStripButton getSchedulerLinkButton() {
        ToolStripButton schedulerButton = new ToolStripButton("Scheduling & Orchestration");
        schedulerButton.setIcon(Images.instance.scheduler_30().getSafeUri().asString());
        setBorderAndIconSize(schedulerButton);

        schedulerButton.addClickHandler(new ClickHandler() {
            public void onClick(ClickEvent event) {
                Window.open("/scheduler", "", "");
            }
        });
        return schedulerButton;
    }

    public ToolStripButton getStudioLinkButton() {
        ToolStripButton studioButton = new ToolStripButton("Workflow Studio");
        studioButton.setIcon(Images.instance.studio_30().getSafeUri().asString());
        setBorderAndIconSize(studioButton);

        studioButton.addClickHandler(new ClickHandler() {
            public void onClick(ClickEvent event) {
                Window.open("/studio", "", "");
            }
        });
        return studioButton;
    }

    public ToolStripButton getResourceManagerLinkButton() {
        ToolStripButton resourceManagerButton = new ToolStripButton("Resource Manager");
        resourceManagerButton.setIcon(Images.instance.rm_30().getSafeUri().asString());
        setBorderAndIconSize(resourceManagerButton);

        resourceManagerButton.addClickHandler(new ClickHandler() {
            public void onClick(ClickEvent event) {
                Window.open("/rm", "", "");
            }
        });

        return resourceManagerButton;
    }

    public ToolStripButton getCloudAutomationLinkButton() {
        ToolStripButton cloudAutomationButton = new ToolStripButton("Cloud Automation");
        cloudAutomationButton.setIcon(Images.instance.pca_30().getSafeUri().asString());
        setBorderAndIconSize(cloudAutomationButton);

        cloudAutomationButton.addClickHandler(new ClickHandler() {
            public void onClick(ClickEvent event) {
                Window.open("/cloud-automation", "", "");
            }
        });

        return cloudAutomationButton;
    }

    public ToolStripButton getNotificationPortalLinkButton() {
        ToolStripButton notificationPortalButton = new ToolStripButton("Notifications");
        notificationPortalButton.setIcon(Images.instance.notification_30().getSafeUri().asString());
        setBorderAndIconSize(notificationPortalButton);

        notificationPortalButton.addClickHandler(new ClickHandler() {
            public void onClick(ClickEvent event) {
                Window.open("/notification-service", "", "");
            }
        });

        return notificationPortalButton;
    }

    public ToolStripButton getLogoutButton(String login, final Controller controller){
        ToolStripButton logoutButton = new ToolStripButton("Logout" + login);
        logoutButton.setIcon(Images.instance.logout_30().getSafeUri().asString());
        setBorderAndIconSize(logoutButton);
        logoutButton.setIconOrientation("right");
        logoutButton.setTooltip("Logout");

        logoutButton.addClickHandler(new ClickHandler() {
            public void onClick(ClickEvent event) {
                SC.confirm("Logout", "Are you sure you want to exit?", new BooleanCallback() {
                    public void execute(Boolean value) {
                        if (value) {
                            controller.logout();
                        }
                    }
                });
            }
        });
        return logoutButton;
    }

    private void setBorderAndIconSize(ToolStripButton toolStripButton){
        toolStripButton.setIconSize(25);
        toolStripButton.setBorder(GREY_BUTTON_BORDER);
    }
}
