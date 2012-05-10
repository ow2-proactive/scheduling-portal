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
package org.ow2.proactive_grid_cloud_portal.scheduler.client;

import java.util.Date;
import java.util.List;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.UsersListener;

import com.google.gwt.i18n.client.DateTimeFormat;
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

        ListGridField userField = new ListGridField(USER_ATTR, "User");
        userField.setWidth(80);

        ListGridField connField = new ListGridField(CONN_TIME_ATTR, "Connected at");

        ListGridField subTimeField = new ListGridField(SUBMIT_TIME_ATTR, "Last submit");

        ListGridField subNumField = new ListGridField(SUBMIT_NUM_ATTR, "Jobs");
        subNumField.setWidth(60);

        this.usersGrid.setFields(userField, subNumField, connField, subTimeField, hostField);

        this.root.addMember(this.usersGrid);

        return this.root;
    }

    /*
     * (non-Javadoc)
     * @see org.ow2.proactive_grid_cloud_portal.client.Listeners.UsersListener#usersUpdated(java.util.List)
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
