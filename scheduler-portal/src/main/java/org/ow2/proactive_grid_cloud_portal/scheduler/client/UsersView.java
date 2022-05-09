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

import java.util.Date;
import java.util.List;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.UsersListener;

import com.google.gwt.i18n.client.DateTimeFormat;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;


/**
 * Displays info about connected scheduler users in a grid
 * 
 * 
 * @author mschnoor
 *
 */
public class UsersView implements UsersListener {

    private static final String HOST_ATTR = "hostName";

    private static final String USER_ATTR = "userName";

    private static final String TENANT_ATTR = "tenant";

    private static final String CONN_TIME_ATTR = "connectionTime";

    private static final String SUBMIT_TIME_ATTR = "submitTime";

    private static final String SUBMIT_NUM_ATTR = "submitNumber";

    /**
     * entries in the grid
     *
     */
    private class UserRecord extends ListGridRecord {
        public UserRecord(SchedulerUser user) {
            setAttribute(HOST_ATTR, user.getHostName());
            setAttribute(USER_ATTR, user.getUsername());
            setAttribute(TENANT_ATTR, user.getTenant());

            if (user.getConnectionTime() > 0)
                setAttribute(CONN_TIME_ATTR, getDate(user.getConnectionTime()));
            if (user.getLastSubmitTime() > 0)
                setAttribute(SUBMIT_TIME_ATTR, getDate(user.getLastSubmitTime()));

            setAttribute(SUBMIT_NUM_ATTR, user.getSubmitNumber());
        }
    }

    private String getDate(long d) {
        return DateTimeFormat.getFormat("MM/dd hh:mm:ss").format(new Date(d));
    }

    /** contains this view's widgets */
    private Layout root = null;

    /** contains the users info */
    private ListGrid usersGrid = null;

    /**
     * Default constructor
     * 
     * @param controller
     */
    public UsersView(SchedulerController controller) {
        controller.getEventDispatcher().addUsersListener(this);
    }

    /**
     * Create and return the view's widgets
     * 
     * @return a widget to add in the container that should display this view
     */
    public Layout build() {
        this.root = new VLayout();
        this.root.setWidth100();
        this.root.setHeight100();

        this.usersGrid = new ListGrid();
        this.usersGrid.setWidth100();
        this.usersGrid.setHeight100();
        this.usersGrid.setCanFreezeFields(true);
        this.usersGrid.setSelectionType(SelectionStyle.NONE);

        ListGridField hostField = new ListGridField(HOST_ATTR, "Hostname");
        alignCenter(hostField);

        ListGridField userField = new ListGridField(USER_ATTR, "User");
        userField.setWidth(120);
        alignCenter(userField);

        ListGridField tenantField = new ListGridField(TENANT_ATTR, "Tenant");
        userField.setWidth(120);
        alignCenter(tenantField);

        ListGridField connField = new ListGridField(CONN_TIME_ATTR, "Connected at");
        alignCenter(connField);
        ListGridField subTimeField = new ListGridField(SUBMIT_TIME_ATTR, "Last submit");
        alignCenter(subTimeField);

        ListGridField subNumField = new ListGridField(SUBMIT_NUM_ATTR, "Jobs");
        subNumField.setWidth(80);
        alignCenter(subNumField);

        this.usersGrid.setFields(userField, tenantField, subNumField, connField, subTimeField, hostField);

        this.root.addMember(this.usersGrid);

        return this.root;
    }

    private void alignCenter(ListGridField field) {
        field.setAlign(Alignment.CENTER);
        field.setCellAlign(Alignment.CENTER);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.ow2.proactive_grid_cloud_portal.client.Listeners.UsersListener#usersUpdated(java.util.
     * List)
     */
    public void usersUpdated(List<SchedulerUser> users) {
        // not many entries, not called often ; don't bother doing something pretty
        ListGridRecord[] data = new ListGridRecord[users.size()];
        int i = 0;
        for (SchedulerUser user : users) {
            data[i] = new UserRecord(user);
            i++;
        }
        this.usersGrid.setData(data);
    }

}
