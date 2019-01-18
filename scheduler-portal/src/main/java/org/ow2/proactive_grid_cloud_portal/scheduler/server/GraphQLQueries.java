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
import java.util.Collections;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.ow2.proactive.scheduling.api.graphql.beans.input.JobInput;
import org.ow2.proactive.scheduling.api.graphql.beans.input.Jobs;
import org.ow2.proactive.scheduling.api.graphql.beans.input.Query;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.JobStatus;
import org.ow2.proactive_grid_cloud_portal.scheduler.shared.filter.Action;
import org.ow2.proactive_grid_cloud_portal.scheduler.shared.filter.Constraint;
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

            List<JobInput> input = getJobInputs(user, pending, running, finished, filterModel);
            jobsBuilder.input(input);

            Query.Builder queryBuilder = new Query.Builder().query(jobsBuilder.build().getQueryString());
            return queryBuilder.build();
        } catch (Exception e) {
            LOGGER.log(Level.SEVERE, e.getMessage());
            return null;
        }
    }

    /**
     * Get the list of filters for graphql query
     * @param user the name of the user
     * @param pending get pending jobs
     * @param running get running jobs
     * @param finished get finished jobs
     * @param filterModel object containing filter contraints
     * @return the list of filtering input
     */
    private List<JobInput> getJobInputs(final String user, final boolean pending, final boolean running,
            final boolean finished, FilterModel filterModel) {
        List<JobInput> input = new ArrayList<>();

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
                        input.add(getJobInput(status, user, Collections.singletonList(constraint)));
                    }
                } else {
                    input.add(getJobInput(status, user, filterModel.getConstraints()));
                }
            }
        }
        return input;
    }

    private JobInput getJobInput(JobStatus status, String user, List<Constraint> constraints) {
        JobInput.Builder input = new JobInput.Builder();
        input.status(status.name().toUpperCase());

        String id = null;
        String priority = null;
        String name = null;
        String statusString = null;
        String projectName = null;
        String submittedTime = null;

        for (Constraint constraint : constraints) {
            String value = constraint.getValue();
            switch (constraint.getTargetField()) {
                case ID: {
                    switch (constraint.getAction()) {
                        case EQUALS:
                            id = getValue(id, value);
                            break;
                        case GREATER_THAN_OR_EQUAL_TO:
                            input.afterId(value);
                            break;
                        case LESS_THAN_OR_EQUAL_TO:
                            input.beforeId(value);
                            break;
                        default:
                            break;
                    }
                    break;
                }
                case STATE: {
                    statusString = getValue(statusString, value.toUpperCase());
                    break;
                }
                case PRIORITY: {
                    priority = getValue(priority, value.toUpperCase());
                    break;
                }
                case USER: {
                    user = getFilteringString(constraint.getAction(), user, value);
                    break;
                }
                case NAME: {
                    name = getFilteringString(constraint.getAction(), name, value);
                    break;
                }
                case PROJECT_NAME: {
                    projectName = getFilteringString(constraint.getAction(), projectName, value);
                    break;
                }
                case SUBMITTED_TIME: {
                    switch (constraint.getAction()) {
                        case GREATER_THAN_OR_EQUAL_TO:
                            input.afterSubmittedTime(value);
                            break;
                        case LESS_THAN_OR_EQUAL_TO:
                            input.beforeSubmittedTime(value);
                            break;
                        default:
                            break;
                    }
                    break;
                }
            }
        }

        if (user != null)
            input.owner(user);
        if (id != null)
            input.id(id);
        if (priority != null)
            input.priority(priority);
        if (name != null)
            input.jobName(name);
        if (statusString != null)
            input.status(statusString);
        if (projectName != null)
            input.projectName(projectName);

        return input.build();
    }

    private String getValue(String oldValue, String newValue) {
        if (oldValue == null)
            return newValue;
        if (newValue == null)
            return oldValue;
        if (oldValue.equals(newValue))
            return oldValue;
        return oldValue + newValue;
    }

    private String getFilteringString(Action action, String oldValue, String newValue) {
        String filteringString = oldValue;
        switch (action) {
            case EQUALS:
                filteringString = getValue(filteringString, newValue);
                break;
            case CONTAINS:
                filteringString = getValue(filteringString, "*" + newValue + "*");
                break;
            case STARTS_WITH:
                filteringString = getValue(filteringString, newValue + "*");
                break;
            default:
                break;
        }
        return filteringString;
    }

}
