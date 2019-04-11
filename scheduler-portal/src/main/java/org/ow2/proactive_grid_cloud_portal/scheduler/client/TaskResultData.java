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
package org.ow2.proactive_grid_cloud_portal.scheduler.client;

import java.io.Serializable;
import java.util.Map;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.json.SchedulerJSONUtils;

import com.google.gwt.json.client.JSONObject;


/**
 * A representation for the business object corresponding to a Task.
 * @author ahagea
 *
 */
@SuppressWarnings("serial")
public class TaskResultData implements Serializable {

    private long id;

    private String readableName;

    private String value;

    private String serializedException;

    private String exceptionMessage;

    private Map<String, String> metadata;

    @Override
    public String toString() {
        return "TaskResultData{" + "id=" + id + ", readableName='" + readableName + '\'' + ", value='" + value + '\'' +
               ", serializedException='" + serializedException + '\'' + ", exceptionMessage='" + exceptionMessage +
               '\'' + ", metadata=" + metadata + '}';
    }

    public static TaskResultData parseJson(JSONObject jsonTaskResultData) {
        TaskResultData result = new TaskResultData();

        JSONObject taskId = jsonTaskResultData.get("id").isObject();
        result.setId(SchedulerJSONUtils.getLongOrElse(taskId, "id", 0L));
        result.setReadableName(SchedulerJSONUtils.getStringOrElse(taskId, "readableName", ""));

        result.setValue(SchedulerJSONUtils.getStringOrElse(jsonTaskResultData, "value", ""));
        result.setValue(SchedulerJSONUtils.getStringOrElse(jsonTaskResultData, "serializedException", ""));
        result.setValue(SchedulerJSONUtils.getStringOrElse(jsonTaskResultData, "exceptionMessage", ""));

        result.setMetadata(SchedulerJSONUtils.extractMap(jsonTaskResultData.get("metadata")));
        return result;
    }

    public long getId() {
        return id;
    }

    public void setId(long id) {
        this.id = id;
    }

    public String getReadableName() {
        return readableName;
    }

    public void setReadableName(String readableName) {
        this.readableName = readableName;
    }

    public String getValue() {
        return value;
    }

    public void setValue(String value) {
        this.value = value;
    }

    public String getSerializedException() {
        return serializedException;
    }

    public void setSerializedException(String serializedException) {
        this.serializedException = serializedException;
    }

    public String getExceptionMessage() {
        return exceptionMessage;
    }

    public void setExceptionMessage(String exceptionMessage) {
        this.exceptionMessage = exceptionMessage;
    }

    public Map<String, String> getMetadata() {
        return metadata;
    }

    public void setMetadata(Map<String, String> metadata) {
        this.metadata = metadata;
    }
}
