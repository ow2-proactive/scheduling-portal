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

/**
 * This package is dedicated to compact view which shows
 * flat represenction of nodesource, hosts, and nodes of RM.
 *
 * Data model is the following. We have list of nodesources.
 * NodeSource contains hosts and deploying nodes. Host contains nodes.
 *
 * Compact View contains two panels: @see {@link org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.views.compact.CompactFlowPanel}
 * and @ee {@link org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.views.compact.CompactFlowPanelOwn}.
 *
 * Both of these panels represent model as linear structure.
 *
 * First panel is normal default panel.
 *
 * Second panel contains only "my nodes". It is nodes that are currently used by the user.
 *
 * @see {@link org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.views.compact.NodeRemover} class contains 'rich' internal state.
 * This class is a wrapper to CompactFlowPanel that allows effictive search and removal from panel.
 *
 *
 */
package org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.views.compact;