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

import static org.junit.Assert.assertTrue;

import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;

import org.junit.Before;
import org.junit.Test;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.ExecutionsModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.JobsModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.OutputModel;

public class SchedulerModelImplTest {

    private static final String STYLE_FOR_TASK_NAME = "<span style='color:gray;'>";

    private OutputModel outputModel;
    
    protected SchedulerModelImpl schedulerModel;
    
    
    protected ExecutionsModel executionsModel;
    
    protected JobsModel jobsModel;

    @Before
    public void setUp() throws Exception {
        this.schedulerModel = new SchedulerModelImpl();
        
        this.executionsModel = new ExecutionsModel(schedulerModel);
        this.schedulerModel.setExecutionsModel(executionsModel);
        
        outputModel = new OutputModel(this.schedulerModel);
        this.schedulerModel.setOutputModel(outputModel);
        
        this.jobsModel = new JobsModel(executionsModel);
        this.executionsModel.setJobsModel(jobsModel);
    }

    // PORTAL-244 PORTAL-243
    @Test
    public void testSetTaskOutput_LinuxLineBreak() {
        addJob(42);

        Task task = new Task();
        outputModel.setTaskOutput("42", task, "[Compute1@192.168.1.168;16:07:40] first line\n[Compute1@192.168.1.168;16:07:40] second line");

        JobOutput jobOutput = outputModel.getJobOutput("42", false);
        String firstLine = jobOutput.getLines(task).get(0);
        String secondLine = jobOutput.getLines(task).get(1);

        assertTrue(firstLine.contains("first"));
        assertTrue(firstLine.contains(STYLE_FOR_TASK_NAME));
        assertTrue(secondLine.contains("second"));
        assertTrue(secondLine.contains(STYLE_FOR_TASK_NAME));
    }

    // PORTAL-244 PORTAL-243
    @Test
    public void testSetTaskOutput_WindowsLineBreak() {
        addJob(42);

        Task task = new Task();
        outputModel.setTaskOutput("42", task, "[Compute1@192.168.1.168;16:07:40] first line\r\n[Compute1@192.168.1.168;16:07:40] second line");

        JobOutput jobOutput = outputModel.getJobOutput("42", false);
        String firstLine = jobOutput.getLines(task).get(0);
        String secondLine = jobOutput.getLines(task).get(1);

        assertTrue(firstLine.contains("first"));
        assertTrue(firstLine.contains(STYLE_FOR_TASK_NAME));
        assertTrue(secondLine.contains("second"));
        assertTrue(secondLine.contains(STYLE_FOR_TASK_NAME));
    }

    // PORTAL-244 PORTAL-243
    @Test
    public void testAppendLiveOutput_LinuxLineBreak() {
        addJob(42);

        JobOutput out = outputModel.getJobOutput("42", true);
        outputModel.setCurrentOutput(out);
        outputModel.getCurrentOutput().setLive(true);
        outputModel.appendLiveOutput("42", "[Compute1@192.168.1.168;16:07:40] first line\n[Compute1@192.168.1.168;16:07:40] second line");

        Collection<List<String>> jobOutput = outputModel.getCurrentOutput().getLines();

        List<String> output = jobOutput.iterator().next();
        
        assertTrue(output.get(0).contains("first"));
        assertTrue(output.get(0).contains(STYLE_FOR_TASK_NAME));
        assertTrue(output.get(1).contains("second"));
        assertTrue(output.get(1).contains(STYLE_FOR_TASK_NAME));
    }

    // PORTAL-244 PORTAL-243
    @Test
    public void testAppendLiveOutput_WindowsLineBreak() {
        addJob(42);

        JobOutput out = outputModel.getJobOutput("42", true);
        outputModel.setCurrentOutput(out);
        outputModel.getCurrentOutput().setLive(true);
        outputModel.appendLiveOutput("42", "[Compute1@192.168.1.168;16:07:40] first line\r\n[Compute1@192.168.1.168;16:07:40] second line");

        Collection<List<String>> jobOutput = outputModel.getCurrentOutput().getLines();

        List<String> output = jobOutput.iterator().next();

        assertTrue(output.get(0).contains("first"));
        assertTrue(output.get(0).contains(STYLE_FOR_TASK_NAME));
        assertTrue(output.get(1).contains("second"));
        assertTrue(output.get(1).contains(STYLE_FOR_TASK_NAME));
    }

    @Test
    public void testAppendLiveOutput_WrongFormat() {
        addJob(42);

        JobOutput out = outputModel.getJobOutput("42", true);
        outputModel.setCurrentOutput(out);
        outputModel.getCurrentOutput().setLive(true);
        outputModel.appendLiveOutput("42", "first line\r\nsecond line");

        Collection<List<String>> jobOutput = outputModel.getCurrentOutput().getLines();

        List<String> output = jobOutput.iterator().next();

        assertTrue(output.isEmpty());
    }

    private void addJob(int jobId) {
        Job job = new Job(jobId);
        job.setStatus(JobStatus.RUNNING);
        this.jobsModel.setJobs(new LinkedHashMap<Integer, Job>(Collections.singletonMap(jobId, job)), 1, 1);
    }
}
