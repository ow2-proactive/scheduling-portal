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

import org.joda.time.format.ISODateTimeFormat;
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

    private static final String RETURN_NOTHING_FILTER = "RETURN_NOTHING_FILTER";

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

            if (startCursor != null) {
                jobsBuilder.after(startCursor);
            }

            if (endCursor != null) {
                jobsBuilder.before(endCursor);
            }

            if (first) {
                jobsBuilder.first(pageSize);
            } else {
                jobsBuilder.last(pageSize);
            }

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

        // Gather job status to consider
        ArrayList<String> statusNames = new ArrayList<>(JobStatus.values().length);
        for (JobStatus status : JobStatus.values()) {
            switch (status) {
                case PENDING:
                    if (pending)
                        statusNames.add(status.name().toUpperCase());
                    break;
                case RUNNING:
                    if (running)
                        statusNames.add(status.name().toUpperCase());
                    break;
                case FINISHED:
                    if (finished)
                        statusNames.add(status.name().toUpperCase());
                    break;
                default:
                    statusNames.add(status.name().toUpperCase());
            }
        }

        // Create filters
        List<JobInput> jobInputs = new ArrayList<>();
        if (filterModel.isMatchAny() && !filterModel.getConstraints().isEmpty()) {
            for (Constraint constraint : filterModel.getConstraints()) {
                jobInputs.add(getJobInput(statusNames, user, Collections.singletonList(constraint)));
            }
        } else {
            jobInputs.add(getJobInput(statusNames, user, filterModel.getConstraints()));
        }
        return jobInputs;
    }

    private JobInput getJobInput(List<String> statusNames, String user, List<Constraint> constraints) {
        JobInput.Builder input = new JobInput.Builder();
        input.status(statusNames);

        String id = null;
        String afterId = null;
        String beforeId = null;
        String status = null;
        String priority = null;
        String userFilter = null;
        String name = null;
        String projectName = null;
        long afterSubmittedTime = -1;
        long beforeSubmittedTime = -1;

        String filter;
        long dateInMs;

        for (Constraint constraint : constraints) {

            String value = constraint.getValue();
            if (value != null) {

                switch (constraint.getTargetField()) {

                    case ID: {

                        switch (constraint.getAction()) {
                            case EQUALS:
                                if (id == null)
                                    id = value;
                                else if (value != id)
                                    return input.jobName(RETURN_NOTHING_FILTER).build();
                                break;
                            case GREATER_THAN_OR_EQUAL_TO:
                                if (afterId == null || Integer.valueOf(value) > Integer.valueOf(afterId))
                                    afterId = value;
                                break;
                            case LESS_THAN_OR_EQUAL_TO:
                                if (beforeId == null || Integer.valueOf(value) < Integer.valueOf(beforeId))
                                    beforeId = value;
                                break;
                            default:
                                break;
                        }
                        break;
                    }

                    case STATE: {

                        if (status == null)
                            status = value.toUpperCase();
                        else if (value.toUpperCase() != status)
                            return input.jobName(RETURN_NOTHING_FILTER).build();
                        break;
                    }

                    case PRIORITY: {

                        if (priority == null)
                            priority = value.toUpperCase();
                        else if (value.toUpperCase() != priority)
                            return input.jobName(RETURN_NOTHING_FILTER).build();
                        break;
                    }

                    case USER: {

                        if (constraint.getAction() == Action.EQUALS) {
                            if (user == null)
                                user = value;
                            else if (value != user)
                                return input.jobName(RETURN_NOTHING_FILTER).build();
                        } else if ((filter = getFilter(constraint, value)) != null)
                            user = filter;
                        break;
                    }

                    case NAME: {

                        if (constraint.getAction() == Action.EQUALS) {
                            if (name == null)
                                name = value;
                            else if (value != name)
                                return input.jobName(RETURN_NOTHING_FILTER).build();
                        } else if ((filter = getFilter(constraint, value)) != null)
                            name = filter;
                        break;
                    }

                    case PROJECT_NAME: {

                        if (constraint.getAction() == Action.EQUALS) {
                            if (projectName == null)
                                projectName = value;
                            else if (value != projectName)
                                return input.jobName(RETURN_NOTHING_FILTER).build();
                        } else if ((filter = getFilter(constraint, value)) != null)
                            projectName = filter;
                        break;
                    }

                    case SUBMITTED_TIME: {

                        try {

                            dateInMs = ISODateTimeFormat.dateTimeParser().parseDateTime(value).toDate().getTime();

                            switch (constraint.getAction()) {
                                case GREATER_THAN_OR_EQUAL_TO:
                                    if (afterSubmittedTime == -1 || dateInMs > afterSubmittedTime)
                                        afterSubmittedTime = dateInMs;
                                    break;
                                case LESS_THAN_OR_EQUAL_TO:
                                    if (beforeSubmittedTime == -1 || dateInMs < beforeSubmittedTime)
                                        beforeSubmittedTime = dateInMs;
                                    break;
                                default:
                                    break;
                            }
                        } catch (Exception e) {
                            LOGGER.log(Level.SEVERE, "Error when parsing date filter \"" + value + "\"", e);
                        }
                        break;
                    }
                }
            }

        }

        // Update the job input object and return it
        if (id != null)
            input.id(id);
        if (afterId != null)
            input.afterId(afterId);
        if (beforeId != null)
            input.beforeId(beforeId);
        if (status != null)
            input.status(status);
        if (priority != null)
            input.priority(priority);

        if (userFilter != null)
            input.owner(userFilter);
        else
            input.owner(user);

        if (name != null)
            input.jobName(name);
        if (projectName != null)
            input.projectName(projectName);
        if (afterSubmittedTime != -1)
            input.afterSubmittedTime("" + afterSubmittedTime);
        if (beforeSubmittedTime != -1)
            input.beforeSubmittedTime("" + beforeSubmittedTime);

        return input.build();
    }

    private String getFilter(Constraint constraint, String value) {
        switch (constraint.getAction()) {
            case NOT_EQUAL:
                return "!" + value;
            case CONTAINS:
                return "*" + value + "*";
            case NOT_CONTAIN:
                return "!*" + value + "*";
            case STARTS_WITH:
                return value + "*";
            default:
                return null;
        }
    }
}
