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
package org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.jobs;

import java.util.List;

import org.ow2.proactive_grid_cloud_portal.common.client.JSUtil;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Job;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.JobStatus;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.util.JobColumnsUtil;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.ColumnsFactory;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.GridColumns;
import org.ow2.proactive_grid_cloud_portal.scheduler.shared.SchedulerPortalDisplayConfig;
import org.ow2.proactive_grid_cloud_portal.scheduler.shared.SchedulerPortalDisplayConfig.JSONColumn;

import com.smartgwt.client.data.Record;


/**
 * A factory that give columns and records for jobs.
 */
public class JobsColumnsFactory implements ColumnsFactory<Job> {

    public static final GridColumns ID_ATTR = new GridColumns("id", "Id", 80, true, true);

    public static final GridColumns STATE_ATTR = new GridColumns("status", "Status", 80, true, false);

    public static final GridColumns ISSUES_ATTR = new GridColumns("issues", "Issues", 80, true, false);

    public static final GridColumns USER_ATTR = new GridColumns("user", "User", 120, true, false);

    public static final GridColumns TENANT_ATTR = new GridColumns("tenant", "Tenant", 120, true, false, true);

    public static final GridColumns PROGRESS_ATTR = new GridColumns("progress", "Progress", 120, true, false);

    public static final GridColumns PRIORITY_ATTR = new GridColumns("priority", "Priority", 100, true, false);

    public static final GridColumns DURATION_ATTR = new GridColumns("duration", "Duration", 120, true, false);

    public static final GridColumns NAME_ATTR = new GridColumns("name", "Name", 200, true, false);

    public static final GridColumns PROJECT_NAME_ATTR = new GridColumns("project", "Project", -1, true, false);

    public static final GridColumns BUCKET_NAME_ATTR = new GridColumns("bucket", "Bucket", -1, true, false, true);

    public static final GridColumns SUBMISSION_MODE_ATTRIBUTE = new GridColumns("submissionMode",
                                                                                "Submission mode",
                                                                                -1,
                                                                                true,
                                                                                false,
                                                                                true);

    public static final GridColumns SUBMIT_TIME_ATTR = new GridColumns("submitTime",
                                                                       "Submitted at",
                                                                       120,
                                                                       true,
                                                                       false,
                                                                       true);

    public static final GridColumns START_TIME_ATTR = new GridColumns("startTime",
                                                                      "Started at",
                                                                      120,
                                                                      true,
                                                                      false,
                                                                      true);

    public static final GridColumns FINISHED_TIME_ATTR = new GridColumns("finishedTime",
                                                                         "Finished at",
                                                                         120,
                                                                         true,
                                                                         false,
                                                                         true);

    private static final GridColumns[] COLUMNS = new GridColumns[] { ID_ATTR, STATE_ATTR, ISSUES_ATTR, USER_ATTR,
                                                                     TENANT_ATTR, PROGRESS_ATTR, PRIORITY_ATTR,
                                                                     DURATION_ATTR, NAME_ATTR, PROJECT_NAME_ATTR,
                                                                     BUCKET_NAME_ATTR, SUBMIT_TIME_ATTR,
                                                                     START_TIME_ATTR, FINISHED_TIME_ATTR,
                                                                     SUBMISSION_MODE_ATTRIBUTE };

    protected static final GridColumns[] COLUMNS_TO_ALIGN = new GridColumns[] { ID_ATTR, STATE_ATTR, ISSUES_ATTR,
                                                                                USER_ATTR, TENANT_ATTR, PROGRESS_ATTR,
                                                                                PRIORITY_ATTR, DURATION_ATTR };

    private static GridColumns[] EXTRA_COLUMNS;

    private static GridColumns[] ALL_COLUMNS;

    static {
        List<JSONColumn> extraColumns = SchedulerPortalDisplayConfig.get().getExtraColumns();

        EXTRA_COLUMNS = new GridColumns[extraColumns.size()];
        for (int i = 0; i < extraColumns.size(); i++) {
            JSONColumn columnConfiguration = extraColumns.get(i);
            EXTRA_COLUMNS[i] = new GridColumns(columnConfiguration.getName(),
                                               columnConfiguration.getTitle(),
                                               columnConfiguration.isHidden());
        }

        ALL_COLUMNS = new GridColumns[COLUMNS.length + EXTRA_COLUMNS.length];
        System.arraycopy(COLUMNS, 0, ALL_COLUMNS, 0, COLUMNS.length);
        System.arraycopy(EXTRA_COLUMNS, 0, ALL_COLUMNS, COLUMNS.length, EXTRA_COLUMNS.length);
    }

    @Override
    public GridColumns[] getColumns() {
        return ALL_COLUMNS;
    }

    protected void buildCommonRecordAttributes(Job item, Record record) {
        record.setAttribute(ID_ATTR.getName(), item.getId());

        long duration = -1;

        record.setAttribute(STATE_ATTR.getName(), item.getStatus().toString());
        record.setAttribute(ISSUES_ATTR.getName(), buildIssuesAttr(item));
        record.setAttribute(USER_ATTR.getName(), item.getUser());
        record.setAttribute(TENANT_ATTR.getName(), item.getTenant());
        record.setAttribute(PRIORITY_ATTR.getName(), item.getPriority().toString());
        record.setAttribute(NAME_ATTR.getName(), item.getName());
        record.setAttribute(PROJECT_NAME_ATTR.getName(), item.getProjectName());
        record.setAttribute(BUCKET_NAME_ATTR.getName(), item.getBucketName());
        record.setAttribute(SUBMISSION_MODE_ATTRIBUTE.getName(), item.getSubmissionMode());

        if (item.getStatus() != JobStatus.IN_ERROR) {
            if (item.getFinishTime() > 0 && item.getStartTime() > 0) {
                duration = item.getFinishTime() - item.getStartTime();
            }
        } else {
            duration = item.getInErrorTime() - item.getStartTime();
        }

        record.setAttribute(DURATION_ATTR.getName(), duration);
        record.setAttribute(SUBMIT_TIME_ATTR.getName(), JSUtil.getTime(item.getSubmitTime()));
        record.setAttribute(START_TIME_ATTR.getName(),
                            (item.getStartTime() > item.getSubmitTime()) ? JSUtil.getTime(item.getStartTime()) : "");
        record.setAttribute(FINISHED_TIME_ATTR.getName(),
                            (item.getFinishTime() > item.getStartTime()) ? JSUtil.getTime(item.getFinishTime()) : "");
        for (GridColumns extraColumn : EXTRA_COLUMNS) {
            String columnName = extraColumn.getName();
            //Fetch value associated to a column
            String value = JobColumnsUtil.getColumnValue(columnName, item.getGenericInformation(), item.getVariables());
            if (value != null) {
                record.setAttribute(columnName, value);
            }
        }
    }

    private Object buildIssuesAttr(Job item) {
        int nbIssues = item.getFailedTasks() + item.getFaultyTasks() + item.getInErrorTasks();

        if (nbIssues == 0) {
            return "";
        }

        return nbIssues;
    }

    @Override
    public void buildRecord(Job item, Record record) {
        buildCommonRecordAttributes(item, record);

        float progress = (float) item.getFinishedTasks() / (float) item.getTotalTasks();
        record.setAttribute(PROGRESS_ATTR.getName(), progress);
    }

}
