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

import com.google.gwt.json.client.JSONArray;
import com.google.gwt.json.client.JSONObject;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.HTMLPane;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;


/**
 * About Window: copyrights and logos
 *
 *
 */
public class AccountInfoWindow {

    private Window window = null;

    private final Controller controller;

    private HTMLPane text;

    public AccountInfoWindow(Controller controller) {
        this.controller = controller;
        this.build();
    }

    private void build() {
        HLayout pane = new HLayout();
        pane.setWidth100();
        pane.setHeight(220);
        pane.setBackgroundColor("#ffffff");

        text = new HTMLPane();
        controller.setCurrentUserData(this);
        text.setWidth100();
        text.setStyleName("paddingLeftAndRight");

        pane.addMember(text);

        HLayout buttons = new HLayout();
        buttons.setAlign(Alignment.RIGHT);
        buttons.setWidth100();
        buttons.setHeight(10);
        buttons.setMargin(10);
        IButton ok = new IButton("Close");
        ok.setIcon(Images.instance.cancel_16().getSafeUri().asString());
        ok.addClickHandler(event -> AccountInfoWindow.this.hide());
        buttons.addMember(ok);

        VLayout root = new VLayout();
        root.setBackgroundColor("#dddddd");
        root.setWidth100();
        root.setHeight100();

        root.addMember(pane);
        root.addMember(buttons);

        this.window = new Window();
        this.window.setTitle("Account Info");
        this.window.setShowMinimizeButton(false);
        this.window.setShowShadow(true);
        this.window.setIsModal(true);
        this.window.setShowModalMask(true);
        this.window.addItem(root);
        this.window.setWidth(600);
        this.window.setHeight(293);
        this.window.centerInPage();
        this.window.setCanDragReposition(false);
    }

    public void setCurrentUserData(JSONObject json) {
        StringBuilder userData = new StringBuilder();
        setUsername(json, userData);

        setStringProperty(json, "domain", userData, "<br><br><b>Domain:</b> ");

        setStringProperty(json, "tenant", userData, "<br><br><b>Tenant:</b>  ");

        setArrayProperty(userData, "<br><br><b>Groups:</b>  ", json, "groups");

        setArrayProperty(userData, "<br><br><b>Admin roles:</b>  ", json, "adminRoles");

        setArrayProperty(userData,
                         "<br><br><b>Portal Access Permission Display:</b>  ",
                         json,
                         "portalAccessPermissionDisplay");
        text.setContents(userData.toString());
    }

    private void setArrayProperty(StringBuilder userData, String str, JSONObject json, String key) {
        userData.append(str);
        JSONArray groups = json.get(key).isArray();
        for (int i = 0; i < groups.size(); i++) {
            userData.append(groups.get(i).isString().stringValue());
            if (i < groups.size() - 1) {
                userData.append(", ");
            }
        }
    }

    private void setStringProperty(JSONObject json, String key, StringBuilder userData, String str) {
        String property = json.get(key).isString() != null ? json.get(key).isString().stringValue() : null;
        userData.append(str).append(property == null ? "" : property);
    }

    private void setUsername(JSONObject json, StringBuilder userData) {
        String username = json.get("userName").isString() != null ? json.get("userName").isString().stringValue()
                                                                  : null;
        userData.append("<h3>Username: ").append(username == null ? "" : username).append("</h3>");
    }

    /**
     * Bring up the modal window
     */
    public void show() {
        this.window.show();
    }

    /**
     * Hide the modal window
     */
    public void hide() {
        this.window.hide();
    }

    /**
     * Destroy the window
     */
    public void destroy() {
        this.hide();
        this.window.destroy();
    }
}
