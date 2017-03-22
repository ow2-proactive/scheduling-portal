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
package org.ow2.proactive_grid_cloud_portal.scheduler.server;

import java.util.logging.Logger;


/**
 * Queries provider: create a query as for example
 * 
 * 
 *
 *   public Query revisionAndjobsinfo() {
 *      try {
 *          Jobs.Builder jobsBuilder = new Jobs.Builder().excludeDataManagement()
 *                                                        .excludeRemovedTime()
 *                                                        .excludeVariables();
 *          Query.Builder queryBuilder = new Query.Builder().query(jobsBuilder.build().getQueryString());
 *          return queryBuilder.build();
 *
 *       } catch (Exception e) {
 *          LOGGER.log(Level.SEVERE, e.getMessage());
 *          return null;
 *       }
 *   }
 * 
 * @author ActiveEon Team
 * @since Mar 8, 2017
 */
public final class GraphQLQueries {

    private static GraphQLQueries client;

    /**
     * Logger
     */
    private static final Logger LOGGER = Logger.getLogger(GraphQLQueries.class.getName());

    private GraphQLQueries() {
    }

    public static GraphQLQueries get() {
        if (client == null) {
            client = new GraphQLQueries();
        }
        return client;
    }

}
