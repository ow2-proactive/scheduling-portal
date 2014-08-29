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

    @Source("images/about_16.png")
    ImageResource about_16();

    @Source("images/clear_16.png")
    ImageResource clear_16();

    @Source("images/connect_16.png")
    ImageResource connect_16();

    @Source("images/exit_16.png")
    ImageResource exit_16();

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

}
