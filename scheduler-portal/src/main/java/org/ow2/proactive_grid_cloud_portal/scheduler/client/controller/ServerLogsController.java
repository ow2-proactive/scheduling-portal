/*
 * ################################################################
 *
 * ProActive Parallel Suite(TM): The Java(TM) library for
 *    Parallel, Distributed, Multi-Core Computing for
 *    Enterprise Grids & Clouds
 *
 * Copyright (C) 1997-2011 INRIA/University of
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

package org.ow2.proactive_grid_cloud_portal.scheduler.client.controller;

import org.ow2.proactive_grid_cloud_portal.common.client.json.JSONUtils;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LogModel;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LoginModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Scheduler;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerController;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerModelImpl;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerServiceAsync;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.ExecutionsModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.ServerLogsView;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.ServerLogsView.ShowLogsCallback;

import com.google.gwt.http.client.Request;
import com.google.gwt.json.client.JSONObject;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.smartgwt.client.widgets.layout.Layout;


/**
 * Controller for the server logs view.
 * @author the activeeon team.
 *
 */
public class ServerLogsController {

    protected SchedulerController parentController;
    
    protected ServerLogsView view;
    
    
    public ServerLogsController(SchedulerController parentController) {
        this.parentController = parentController;
    }
    
    public Layout buildView(){
        this.view = new ServerLogsView(this);
        return this.view.build();
    }
    
    
    
    protected int getCurrentJobId(){
        ExecutionsModel executionsModel = ((SchedulerModelImpl) parentController.getModel()).getExecutionsModel();
        return executionsModel.getSelectedJob().getId();
    }
    
    
    /**
     * Fetch server logs for a single task
     * 
     * @param jobId id of the job containing this task
     * @param taskname task for which the output should be fetched
     * @param logs one of {@link SchedulerServiceAsync#LOG_ALL}, {@link SchedulerServiceAsync#LOG_STDERR},
     *   {@link SchedulerServiceAsync#LOG_STDOUT}
     */
    public void getTaskServerLogs(final String taskname, final ShowLogsCallback logs) {
        final int jobId = getCurrentJobId();
        SchedulerServiceAsync scheduler = Scheduler.getSchedulerService();
        Request req = scheduler.getTaskServerLogs(LoginModel.getInstance().getSessionId(), jobId, taskname,
                new AsyncCallback<String>() {
            public void onFailure(Throwable caught) {
                String msg = JSONUtils.getJsonErrorMessage(caught);
                // might be an exception
                try {
                    JSONObject json = JSONUtils.parseJSON(caught.getMessage()).isObject();
                    if (json.containsKey("stackTrace")) {
                        msg = json.get("stackTrace").isString().stringValue();
                        msg = msg.replace("\t", "&nbsp;&nbsp;&nbsp;&nbsp;");
                        msg = msg.replace("\n", "<br>");
                    }
                } catch (Throwable t) {
                    // not json
                }
                LogModel.getInstance().logMessage("Failed to get server logs for task " +
                        taskname + " in job " + jobId /* + ": " + msg */);
            }

            public void onSuccess(String result) {
                LogModel.getInstance().logMessage("Successfully fetched server logs for task " + taskname +
                        " in job " + jobId);
                logs.show(result);
            }
        });
    }

    /**
     * Fetch server logs for a single job
     * 
     * @param jobId id of the job containing this task
     * @param logs one of {@link SchedulerServiceAsync#LOG_ALL}, {@link SchedulerServiceAsync#LOG_STDERR},
     *   {@link SchedulerServiceAsync#LOG_STDOUT}
     */
    public void getJobServerLogs(final ShowLogsCallback logs) {
        final int jobId = getCurrentJobId();
        SchedulerServiceAsync scheduler = Scheduler.getSchedulerService();
        Request req = scheduler.getJobServerLogs(LoginModel.getInstance().getSessionId(), jobId,
                new AsyncCallback<String>() {
            public void onFailure(Throwable caught) {
                String msg = JSONUtils.getJsonErrorMessage(caught);
                // might be an exception
                try {
                    JSONObject json = JSONUtils.parseJSON(caught.getMessage()).isObject();
                    if (json.containsKey("stackTrace")) {
                        msg = json.get("stackTrace").isString().stringValue();
                        msg = msg.replace("\t", "&nbsp;&nbsp;&nbsp;&nbsp;");
                        msg = msg.replace("\n", "<br>");
                    }
                } catch (Throwable t) {
                    // not json
                }
                LogModel.getInstance().logMessage("Failed to get server logs for a job " +
                        jobId);
            }

            public void onSuccess(String result) {
                LogModel.getInstance().logMessage("Successfully fetched server logs for job " + jobId);
                logs.show(result);
            }
        });
    }



    public SchedulerController getParentController() {
        return parentController;
    }
    
    
    
}
