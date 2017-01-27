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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.List;
import java.util.SortedMap;
import java.util.TreeMap;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.TasksModel;

import com.google.gwt.safehtml.shared.SafeHtmlUtils;


/**
 * Wraps the output of a job, as it was received at a given time
 * <p>
 * Each task output is stored in a map sorted by finished time
 * each entry is this map is a list of output lines
 * 
 * 
 * @author mschnoor
 *
 */
public class JobOutput {

    private static final String PLATFORM_INDEPENDENT_LINE_BREAK = "\r\n?|\n";

    private static final Comparator<Task> TASK_FINISHED_TIME_COMPARATOR = new Comparator<Task>() {
        @Override
        public int compare(Task o1, Task o2) {
            if (o1.equals(o2)) {
                return 0;
            }
            return (int) (o1.getFinishTime() - o2.getFinishTime());
        }
    };

    /** the output content, split by task and line
     * key is timestamp for task finish time for sorting */
    protected SortedMap<Task, List<String>> lines;

    protected boolean complete = false;

    protected OutputMode outputMode = OutputMode.LOG_OUT_ERR;

    /** id of the job */
    private String jobId;

    protected boolean isLive = false;

    protected boolean liveEnabled = true;

    protected List<List<String>> liveContent;

    /**
     * Default constructor
     * 
     * @param jobId id of the job representing this output
     */
    public JobOutput(String jobId) {
        this.jobId = jobId;
        this.lines = new TreeMap<Task, List<String>>(TASK_FINISHED_TIME_COMPARATOR);
    }

    public List<String> update(Task task, String out, OutputMode outputMode) {
        ArrayList<String> remoteVisuLines = new ArrayList<String>();

        if (this.outputMode != outputMode) {
            this.outputMode = outputMode;
            this.resetLines();
        }

        List<String> taskLines = this.lines.get(task);
        if (taskLines == null) {
            taskLines = new ArrayList<String>();
            this.lines.put(task, taskLines);
        } else {
            taskLines.clear();
        }

        for (String line : lineByLine(out)) {
            if (line.contains(TasksModel.PA_REMOTE_CONNECTION)) {
                remoteVisuLines.add(line);
            }
            line = formatLine(line);

            if (!line.trim().isEmpty()) {
                taskLines.add(line);
            }
        }
        return remoteVisuLines;
    }

    /**
     * Update the live output
     * @param out the out chunk to append
     * @return a list of line that contain PA_REMOTE_CONNECTION for remote visu
     */
    public List<String> updateLive(String out) {
        ArrayList<String> remoteVisuLines = new ArrayList<String>();

        String[] expl = lineByLine(out);
        String formatedLine;
        for (String str : expl) {
            if (str.contains(TasksModel.PA_REMOTE_CONNECTION)) {
                remoteVisuLines.add(str);
            }
            formatedLine = formatLine(str);
            if (!formatedLine.isEmpty()) {
                this.liveContent.get(0).add(formatedLine);
            }
        }
        return remoteVisuLines;
    }

    /**
     * @return the output lines, exploded in a list, per task (finished time as key for sorting)
     */
    public Collection<List<String>> getLines() {
        if (this.isLive) {
            return this.liveContent;
        } else {
            return this.lines.values();
        }
    }

    /**
     * @return the output lines for a given task.
     */
    public List<String> getLines(Task task) {
        return this.lines.get(task);
    }

    /**
     * @return the id of the job associated with this output
     */
    public String getJobId() {
        return this.jobId;
    }

    public boolean isLive() {
        return isLive;
    }

    public void setLive(boolean isLive) {
        this.isLive = isLive;
        if (isLive && this.liveContent == null) {
            this.liveContent = new ArrayList<List<String>>();
            this.liveContent.add(new ArrayList<String>());
        }
    }

    private String[] lineByLine(String lines) {
        return lines.split(PLATFORM_INDEPENDENT_LINE_BREAK);
    }

    private String formatLine(String str) {
        if (str.trim().isEmpty()) {
            return "";
        } else {
            String safeString;
            // Timestamp is colored when the default pattern is in use
            if (str.matches("\\[.*\\].*")) {
                safeString = SafeHtmlUtils.htmlEscape(str).replaceFirst("]", "]</span>");
                return "<nobr><span style='color:gray;'>" + safeString + "</nobr><br>";
            } else {
                safeString = SafeHtmlUtils.htmlEscape(str);
                return "<nobr>" + safeString + "</nobr><br>";
            }
        }
    }

    public boolean isLiveEnabled() {
        return liveEnabled;
    }

    public void setLiveEnabled(boolean liveEnabled) {
        this.liveEnabled = liveEnabled;
    }

    public OutputMode getOutputMode() {
        return outputMode;
    }

    public boolean isComplete() {
        return complete;
    }

    public void setComplete(boolean complete) {
        this.complete = complete;
    }

    public void resetLines() {
        this.complete = false;
        this.lines.clear();
    }
}
