/*
 * ################################################################
 *
 * ProActive Parallel Suite(TM): The Java(TM) library for
 *    Parallel, Distributed, Multi-Core Computing for
 *    Enterprise Grids & Clouds
 *
 * Copyright (C) 1997-2015 INRIA/University of
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
package org.ow2.proactive_grid_cloud_portal.scheduler.client;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.SortedMap;
import java.util.TreeMap;


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
    private SortedMap<Task, List<String>> lines;

    /** id of the job */
    private int jobId;
    
    
    protected boolean complete = false;
    

    /**
     * Default constructor
     * 
     * @param jobId id of the job representing this output
     */
    public JobOutput(int jobId) {
        this.jobId = jobId;
        this.lines = new TreeMap<Task, List<String>>(TASK_FINISHED_TIME_COMPARATOR);
    }

    /**
     * Update the output for a single task
     *
     * @param task the task we update
     * @param output output for this task
     */
    public void update(Task task, List<String> output) {
        List<String> tl = this.lines.get(task);
        if (tl != null) {
            tl.clear();
        } else {
            tl = new ArrayList<String>();
            this.lines.put(task, tl);
        }
        tl.addAll(output);
    }

    /**
     * @return the output lines, exploded in a list, per task (finished time as key for sorting)
     */
    public SortedMap<Task, List<String>> getLines() {
        return this.lines;
    }

    /**
     * @return the id of the job associated with this output
     */
    public int getJobId() {
        return this.jobId;
    }

    public boolean isComplete() {
        return complete;
    }

    public void setComplete(boolean complete) {
        this.complete = complete;
    }
    
}
