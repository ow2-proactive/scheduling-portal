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
package org.ow2.proactive_grid_cloud_portal.scheduler.shared.filter;

import com.google.gwt.user.client.rpc.IsSerializable;


/**
 * @author ActiveEon Team
 * @since May 9, 2017
 */
public enum Field implements IsSerializable {
    ID("id"),
    PRIORITY("priority"),
    NAME("name"),
    USER("user"),
    TENANT("tenant"),
    STATE("status"),
    PROJECT_NAME("project"),
    BUCKET_NAME("bucket"),
    SUBMITTED_TIME("Submitted time"),
    START_TIME("Start time"),
    LAST_UPDATED_TIME("Last-updated time"),
    FINISHED_TIME("Finished time"),
    NUMBER_OF_PENDING_TASKS("Pending tasks"),
    NUMBER_OF_RUNNING_TASKS("Running tasks"),
    NUMBER_OF_FINISHED_TASKS("Finished tasks"),
    NUMBER_OF_FAULTY_TASKS("Faulty tasks"),
    NUMBER_OF_FAILED_TASKS("Failed tasks"),
    NUMBER_OF_IN_ERROR_TASKS("In-error tasks"),
    CUMULATED_CORE_TIME("Cumulated core time"),
    PARENT_ID("Parent id"),
    CHILDREN_COUNT("Children count"),
    NUMBER_OF_NODES("Number of nodes"),
    NUMBER_OF_NODES_IN_PARALLEL("Number of nodes in parallel"),
    SUBMISSION_MODE("Submitted from");

    private String name;

    private Field(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public static Field get(String name) {
        for (Field field : Field.values()) {
            if (field.name.equals(name)) {
                return field;
            }
        }
        return ID;
    }
}
