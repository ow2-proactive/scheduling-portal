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

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.ow2.proactive.scheduling.api.graphql.beans.input.JobInput;
import org.ow2.proactive.scheduling.api.graphql.beans.input.Jobs;
import org.ow2.proactive.scheduling.api.graphql.beans.input.Query;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.JobStatus;
import org.ow2.proactive_grid_cloud_portal.scheduler.shared.filter.Constraint;
import org.ow2.proactive_grid_cloud_portal.scheduler.shared.filter.Field;
import org.ow2.proactive_grid_cloud_portal.scheduler.shared.filter.FilterModel;


/**
 * Queries provider
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

    private JobInput getJobInputWithStatus(JobStatus status, String user, String id, String priority, String name) {
        JobInput.Builder input = new JobInput.Builder().status(status.name().toUpperCase());
        if (user != null)
            input.owner(user);
        if (id != null)
            input.id(id);
        if (priority != null)
            input.priority(priority);
        if (name != null)
            input.jobName(name);
        return input.build();
    }

    public Query getRevisionAndjobsInfoQuery(final String user, final boolean pending, final boolean running,
            final boolean finished, String startCursor, String endCursor, int pageSize, boolean first,
            FilterModel filterModel) {
        try {
            Jobs.Builder jobsBuilder = new Jobs.Builder().excludeDataManagement().excludeRemovedTime();

            if (startCursor != null)
                jobsBuilder.after(startCursor);
            if (endCursor != null)
                jobsBuilder.before(endCursor);

            if (first)
                jobsBuilder.first(pageSize);
            else
                jobsBuilder.last(pageSize);

            List<JobInput> input = new ArrayList<>();
            jobsBuilder.input(input);

            for (JobStatus status : JobStatus.values()) {
                boolean fetch;
                switch (status) {
                    case PENDING:
                        fetch = pending;
                        break;
                    case RUNNING:
                        fetch = running;
                        break;
                    case FINISHED:
                        fetch = finished;
                        break;
                    default:
                        fetch = true;
                }

                if (fetch) {
                    if (filterModel.isMatchAny() && !filterModel.getConstraints().isEmpty()) {
                        for (Constraint constraint : filterModel.getConstraints()) {
                            input.add(getJobInputWithStatus(status,
                                                            user,
                                                            constraint.getFilteringString(Field.ID),
                                                            constraint.getFilteringString(Field.PRIORITY),
                                                            constraint.getFilteringString(Field.NAME)));
                        }
                    } else {
                        String id = null;
                        String priority = null;
                        String name = null;

                        for (Constraint contraint : filterModel.getConstraints()) {
                            id = getValue(id, contraint.getFilteringString(Field.ID));
                            priority = getValue(priority, contraint.getFilteringString(Field.PRIORITY));
                            name = getValue(name, contraint.getFilteringString(Field.NAME));
                        }
                        input.add(getJobInputWithStatus(status, user, id, priority, name));
                    }
                }
            }

            Query.Builder queryBuilder = new Query.Builder().query(jobsBuilder.build().getQueryString());
            return queryBuilder.build();
        } catch (Exception e) {
            LOGGER.log(Level.SEVERE, e.getMessage());
            return null;
        }
    }

    private String getValue(String oldValue, String newValue) {
        if (oldValue == null)
            return newValue;
        if (newValue == null)
            return oldValue;
        return oldValue + newValue;
    }

}
