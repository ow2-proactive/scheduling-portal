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
package org.ow2.proactive_grid_cloud_portal.rm.client;

import com.google.gwt.core.client.GWT;
import com.google.gwt.resources.client.ClientBundle;
import com.google.gwt.resources.client.ImageResource;


/**
 * Image Bundle to optimizes external resource handling
 * 
 * @author mschnoor
 *
 */
public interface RMImages extends ClientBundle {

    public static final RMImages instance = GWT.create(RMImages.class);

    @Source("images/good.png")
    ImageResource good();

    @Source("images/node_free_16_token.png")
    ImageResource free_token();

    @Source("images/node_busy_16_token.png")
    ImageResource busy_token();

    @Source("images/node_free_16_locked_token.png")
    ImageResource free_locked_token();

    @Source("images/node_busy_16_locked_token.png")
    ImageResource busy_locked_token();

    @Source("images/scheduler_kill_16.png")
    ImageResource kill();

    @Source("images/host_16.png")
    ImageResource host_16();

    @Source("images/host_virtual_16.png")
    ImageResource host_virtual_16();

    @Source("images/nodesource_deployed.png")
    ImageResource nodesource_deployed();

    @Source("images/nodesource_undeployed.png")
    ImageResource nodesource_undeployed();

    @Source("images/nodesource_edit.png")
    ImageResource nodesource_edit();

    @Source("images/node_remove_16.png")
    ImageResource node_remove_16();

    @Source("images/node_add_16.png")
    ImageResource node_add_16();

    @Source("images/node_add_16_locked.png")
    ImageResource node_add_16_locked();

    @Source("images/node_configuring_16.png")
    ImageResource node_configuring_16();

    @Source("images/node_configuring_16_locked.png")
    ImageResource node_configuring_16_locked();

    @Source("images/node_lost_16.png")
    ImageResource node_lost_16();

    @Source("images/node_lost_16_locked.png")
    ImageResource node_lost_16_locked();

    @Source("images/node_free_16.png")
    ImageResource node_free_16();

    @Source("images/node_free_16_locked.png")
    ImageResource node_free_16_locked();

    @Source("images/node_down_16.png")
    ImageResource node_down_16();

    @Source("images/node_down_16_locked.png")
    ImageResource node_down_16_locked();

    @Source("images/node_deploying_16.png")
    ImageResource node_deploying_16();

    @Source("images/node_deploying_16_locked.png")
    ImageResource node_deploying_16_locked();

    @Source("images/node_busy_16.png")
    ImageResource node_busy_16();

    @Source("images/node_busy_16_locked.png")
    ImageResource node_busy_16_locked();

    @Source("images/node_torelease_16.png")
    ImageResource node_torelease_16();

    @Source("images/node_torelease_16_locked.png")
    ImageResource node_torelease_16_locked();

    @Source("images/open_16.png")
    ImageResource open_16();

    @Source("images/refresh.png")
    ImageResource refresh();

    @Source("images/padlock.png")
    ImageResource padlock();

}
