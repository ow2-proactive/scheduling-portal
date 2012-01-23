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

	@Source("images/host_16.png")
	ImageResource host_16();

	@Source("images/host_virtual_16.png")
	ImageResource host_virtual_16();

	@Source("images/nodesource_16.png")
	ImageResource nodesource_16();

	@Source("images/node_locked_16.png")
	ImageResource node_locked_16();

	@Source("images/node_remove_16.png")
	ImageResource node_remove_16();

	@Source("images/node_add_16.png")
	ImageResource node_add_16();

	@Source("images/node_configuring_16.png")
	ImageResource node_configuring_16();

	@Source("images/node_lost_16.png")
	ImageResource node_lost_16();

	@Source("images/node_free_16.png")
	ImageResource node_free_16();

	@Source("images/node_down_16.png")
	ImageResource node_down_16();

	@Source("images/node_deploying_16.png")
	ImageResource node_deploying_16();

	@Source("images/node_busy_16.png")
	ImageResource node_busy_16();

	@Source("images/node_torelease_16.png")
	ImageResource node_torelease_16();

	@Source("images/open_16.png")
	ImageResource open_16();

	@Source("images/logo_32.png")
	ImageResource logo_32();

	@Source("images/logo_350.png")
	ImageResource logo_350();

}
