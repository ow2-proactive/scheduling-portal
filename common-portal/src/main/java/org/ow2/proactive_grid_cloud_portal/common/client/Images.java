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
import com.google.gwt.resources.client.ClientBundle;
import com.google.gwt.resources.client.ImageResource;


/**
 * Image Bundle to optimizes external resource handling
 * 
 * @author mschnoor
 *
 */
public interface Images extends ClientBundle {

    public static final Images instance = GWT.create(Images.class);

    @Source("images/automation_dashboard_30.png")
    ImageResource automation_dashboard_30();

    @Source("images/rm_30.png")
    ImageResource rm_30();

    @Source("images/scheduler_30.png")
    ImageResource scheduler_30();

    @Source("images/studio_30.png")
    ImageResource studio_30();

    @Source("images/notification_30.png")
    ImageResource notification_30();

    @Source("images/logout_30.png")
    ImageResource logout_30();

    @Source("images/about_16.png")
    ImageResource about_16();

    @Source("images/clear_16.png")
    ImageResource clear_16();

    @Source("images/connect_16.png")
    ImageResource connect_16();

    @Source("images/exit_18.png")
    ImageResource exit_18();

    @Source("images/info_32.png")
    ImageResource info_32();

    @Source("images/key_16.png")
    ImageResource key_16();

    @Source("images/log_16.png")
    ImageResource log_16();

    @Source("images/search_16.png")
    ImageResource search_16();

    @Source("images/server_16.png")
    ImageResource server_16();

    @Source("images/settings_16.png")
    ImageResource settings_16();

    @Source("images/stats_16.png")
    ImageResource stats_16();

    @Source("images/user_16.png")
    ImageResource user_16();

    @Source("images/expand_16.png")
    ImageResource expand_16();

    @Source("images/close_16.png")
    ImageResource close_16();

    @Source("images/ok_16.png")
    ImageResource ok_16();

    @Source("images/cancel_16.png")
    ImageResource cancel_16();

    @Source("images/net_error_16.png")
    ImageResource net_error_16();

    @Source("images/icon_manual.png")
    ImageResource icon_manual();

}
