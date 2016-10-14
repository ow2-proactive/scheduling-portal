/*
 *  *
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
 *  * $$PROACTIVE_INITIAL_DEV$$
 */
package org.ow2.proactive_grid_cloud_portal.scheduler.client;

import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.List;

import org.junit.Test;

public class JobOutputTest {

    @Test
    public void testLinesCanBeRetrievedByTask() throws Exception {
        JobOutput jobOutput = new JobOutput("1");
        Task aTask = new Task();
        aTask.setJobId(1);
        aTask.setId(1);
        jobOutput.update(aTask, "[...] output", OutputMode.LOG_OUT_ERR);

        List<String> taskOutput = jobOutput.getLines(aTask);

        assertEquals("<nobr>[...] output</nobr><br>", taskOutput.get(0));
    }

    @Test
    public void testLinesAreSortedByTaskFinishTime() throws Exception {
        JobOutput jobOutput = new JobOutput("1");
        Task firstTask = createTask(1, 42);
        Task secondTask = createTask(2, 57);
        jobOutput.update(secondTask, "[...] output_second", OutputMode.LOG_OUT_ERR);
        jobOutput.update(firstTask, "[...] output_first", OutputMode.LOG_OUT_ERR);

        List<List<String>> allTaskOutput = new ArrayList<List<String>>(jobOutput.getLines());

        assertEquals("<nobr>[...] output_first</nobr><br>", allTaskOutput.get(0).get(0));
        assertEquals("<nobr>[...] output_second</nobr><br>", allTaskOutput.get(1).get(0));
    }

    // PORTAL-348
    @Test
    public void tasks_are_unique_in_job_ouput() throws Exception {
        JobOutput jobOutput = new JobOutput("1");
        Task firstTask = createTask(1, 42);

        jobOutput.update(firstTask, "output", OutputMode.LOG_OUT_ERR);

        firstTask.setFinishTime(123);
        jobOutput.update(firstTask, "finished", OutputMode.LOG_OUT_ERR);

        assertEquals(1, jobOutput.getLines().size());

        Task firstTaskDifferentObject = createTask(1, 456);
        jobOutput.update(firstTaskDifferentObject, "finished_new_object", OutputMode.LOG_OUT_ERR);

        assertEquals(1, jobOutput.getLines().size());
    }

    private Task createTask(int id, int finishedTime) {
        Task firstTask = new Task();
        firstTask.setId(id);
        firstTask.setFinishTime(finishedTime);
        return firstTask;
    }
}
