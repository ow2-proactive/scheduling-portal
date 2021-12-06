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
package org.ow2.proactive_grid_cloud_portal.scheduler.client.controller;

import static org.ow2.proactive_grid_cloud_portal.common.client.json.JSONUtils.parseJSON;

import java.util.Arrays;
import java.util.Map;
import java.util.Set;

import org.ow2.proactive_grid_cloud_portal.common.client.json.JSONException;
import org.ow2.proactive_grid_cloud_portal.common.client.json.JSONUtils;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LogModel;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LoginModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.ActionsWindow;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Scheduler;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerServiceAsync;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.json.SchedulerJSONUtils;

import com.google.gwt.json.client.JSONObject;
import com.google.gwt.json.client.JSONValue;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.layout.VLayout;


/**
 * Controller for job signals.
 * @author the activeeon team.
 *
 */
public class ActionsController {

    private final String signalName;

    private final String jobId;

    private VLayout messageLayout;

    private ActionsWindow actionsWindow;

    public ActionsController(String signalName, String jobId) {
        this.signalName = signalName;
        this.jobId = jobId;
    }

    /**
     * Sends signal to a job
     */
    public void addJobSignal() {
        SchedulerServiceAsync scheduler = Scheduler.getSchedulerService();
        String sessionId = LoginModel.getInstance().getSessionId();
        scheduler.addJobSignal(sessionId, signalName, jobId, new AsyncCallback<Set<String>>() {
            public void onSuccess(Set<String> result) {
                LogModel.getInstance().logImportantMessage("Successfully added signal " + signalName + ". Job " +
                                                           jobId + " has the following signals " + result);
            }

            public void onFailure(Throwable caught) {
                String message = JSONUtils.getJsonErrorMessage(caught);
                LogModel.getInstance().logImportantMessage("Failed to add job signals : " + message);
            }
        });
    }

    /**
     * Sends signal with variables to a job
     *
     * @param updatedVariables the signal variables to be send
     */
    public void addJobSignalWithVariables(Map<String, String> updatedVariables) {
        SchedulerServiceAsync scheduler = Scheduler.getSchedulerService();
        scheduler.addJobSignalWithVariables(LoginModel.getInstance().getSessionId(),
                                            signalName,
                                            jobId,
                                            updatedVariables,
                                            new AsyncCallback<Set<String>>() {
                                                public void onSuccess(Set<String> result) {
                                                    LogModel.getInstance()
                                                            .logImportantMessage("Successfully added signal " + jobId +
                                                                                 ".Job " + jobId +
                                                                                 " has the following signals " +
                                                                                 result);
                                                    actionsWindow.hide();
                                                }

                                                public void onFailure(Throwable caught) {
                                                    String message = JSONUtils.getJsonErrorMessage(caught);
                                                    LogModel.getInstance().logImportantMessage(
                                                                                               "Failed to add signal variables for job " +
                                                                                               jobId + message);
                                                }
                                            });
    }

    /**
     * Validates the signal variables
     *
     * @param updatedVariables the signal variables to be validated
     */
    public void validateJobSignal(Map<String, String> updatedVariables, boolean addSignal) {
        SchedulerServiceAsync scheduler = Scheduler.getSchedulerService();
        scheduler.validateJobSignal(LoginModel.getInstance().getSessionId(),
                                    signalName,
                                    jobId,
                                    updatedVariables,
                                    new AsyncCallback<String>() {
                                        public void onSuccess(String result) {
                                            handleOnSuccessResult(result, updatedVariables, addSignal);
                                        }

                                        public void onFailure(Throwable caught) {
                                            handleOnErrorResult(caught);
                                        }
                                    });
    }

    private void handleOnSuccessResult(String result, Map<String, String> updatedVariables, boolean addSignal) {
        String errorMessage = getErrorMessage(result, messageLayout);
        if (errorMessage != null && !errorMessage.isEmpty()) {
            LogModel.getInstance().logImportantMessage("Failed to validate signal variables for job " + jobId);
            setLabelMessage(messageLayout, errorMessage, "errorMessage");
        } else {
            LogModel.getInstance().logImportantMessage("Successfully validated variables " + updatedVariables +
                                                       " for signal " + signalName + " on job " + jobId);
            setLabelMessage(messageLayout, "Variables are valid", "infoMessage");
            actionsWindow.updateVariables(result);
            actionsWindow.setVarsLayout();
            if (addSignal) {
                addJobSignalWithVariables(updatedVariables);
            }
        }
    }

    private void handleOnErrorResult(Throwable caught) {
        String message = JSONUtils.getJsonErrorMessage(caught);
        LogModel.getInstance().logImportantMessage("Failed to validate signal variables for job " + jobId + message);
        setLabelMessage(messageLayout, "Failed to validate signal variables : ", "errorMessage");
    }

    private String getErrorMessage(String result, VLayout messageLayout) {
        try {
            JSONValue val = parseJSON(result);
            JSONObject jsonJobInfo = val.isObject();
            if (jsonJobInfo == null) {
                throw new JSONException("Expected JSON Object: " + result);
            }
            return SchedulerJSONUtils.getStringOrDefault(jsonJobInfo.get("errorMessage"));
        } catch (JSONException e) {
            LogModel.getInstance().logImportantMessage("Failed to validate signal variables for job " + jobId);
            setLabelMessage(messageLayout, "Variables are valid", "infoMessage");
            return null;
        }
    }

    private void setLabelMessage(VLayout layout, String s, String infoMessage) {
        Arrays.asList(layout.getMembers()).forEach(layout::removeMember);
        Label infoLabel = new Label(s);
        infoLabel.setHeight(30);
        infoLabel.setWidth100();
        infoLabel.setMargin(10);
        infoLabel.setAlign(Alignment.CENTER);
        infoLabel.setStyleName(infoMessage);
        layout.addMember(infoLabel);
    }

    public void setMessageLayout(VLayout messageLayout) {
        this.messageLayout = messageLayout;
    }

    public void setActionsWindow(ActionsWindow actionsWindow) {
        this.actionsWindow = actionsWindow;
    }
}
