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
package org.ow2.proactive_grid_cloud_portal.common.client.model;

import java.util.*;


public class LoginModel {

    private boolean logged = false;

    private String login = null;

    private String sessionId = null;

    private static LoginModel instance = null;

    private static final String PERMISSION_SCHEDULER_FRONTEND_START = "org.ow2.proactive.scheduler.core.SchedulerFrontend.start";

    private static final String PERMISSION_SCHEDULER_FRONTEND_STOP = "org.ow2.proactive.scheduler.core.SchedulerFrontend.stop";

    private static final String PERMISSION_SCHEDULER_FRONTEND_PAUSE = "org.ow2.proactive.scheduler.core.SchedulerFrontend.pause";

    private static final String PERMISSION_SCHEDULER_FRONTEND_FREEZE = "org.ow2.proactive.scheduler.core.SchedulerFrontend.freeze";

    private static final String PERMISSION_SCHEDULER_FRONTEND_RESUME = "org.ow2.proactive.scheduler.core.SchedulerFrontend.resume";

    private static final String PERMISSION_SCHEDULER_FRONTEND_SHUTDOWN = "org.ow2.proactive.scheduler.core.SchedulerFrontend.shutdown";

    private static final String PERMISSION_SCHEDULER_FRONTEND_KILL = "org.ow2.proactive.scheduler.core.SchedulerFrontend.kill";

    private static final String PERMISSION_PAUSE_JOB = "pauseJob";

    private static final String PERMISSION_RESTART_ALL_IN_ERROR_TASKS = "restartAllInErrorTasks";

    private static final String PERMISSION_RESUME_JOB = "resumeJob";

    private static final String PERMISSION_CHANGE_JOB_PRIORITY = "changeJobPriority";

    private static final String PERMISSION_KILL_JOB = "killJob";

    private static final String PERMISSION_GET_JOB_CONTENT = "getJobContent"; //Export XML, re_submit

    private static final String PERMISSION_REMOVE_JOB = "removeJob";

    private static final String PERMISSION_GET_JOB_SERVER_LOGS = "getJobServerLogs";

    private static final String PERMISSION_GET_JOB_STATE = "getJobState";

    private static final String PERMISSION_GET_JOB_RESULT = "getJobResult";

    private static final String PERMISSION_RM_EXECUTE_SCRIPT = "org.ow2.proactive.resourcemanager.core.RMCore.executeScript";

    private static final String PERMISSION_RM_GET_RM_THREAD_DUMP = "org.ow2.proactive.resourcemanager.core.RMCore.getRMThreadDump";

    private static final String PERMISSION_RM_GET_NODE_THREAD_DUMP = "org.ow2.proactive.resourcemanager.core.RMCore.getNodeThreadDump";

    private static final String PERMISSION_RM_LOCK_NODES = "org.ow2.proactive.resourcemanager.core.RMCore.lockNodes";

    private static final String PERMISSION_RM_UNLOCK_NODES = "org.ow2.proactive.resourcemanager.core.RMCore.unlockNodes";

    private static final String PERMISSION_RM_REMOVE_NODES = "org.ow2.proactive.resourcemanager.core.RMCore.removeNode";

    private static final String PERMISSION_GET_NODE_SOURCE_CONFIGURATION = "org.ow2.proactive.resourcemanager.core.RMCore.getNodeSourceConfiguration";

    private static final String PERMISSION_UPDATE_DYNAMIC_PARAMETERS = "org.ow2.proactive.resourcemanager.core.RMCore.updateDynamicParameters";

    private static final String PERMISSION_UNDEPLOY_NODE_SOURCE = "org.ow2.proactive.resourcemanager.core.RMCore.undeployNodeSource";

    private static final String PERMISSION_DEPLOY_NODE_SOURCE = "org.ow2.proactive.resourcemanager.core.RMCore.deployNodeSource";

    // a map containing the job id as key and another map as value containing the method name and true/false if the user has
    // the permission to the method for the current jobId
    Map<String, Map<String, Boolean>> schedulerPermissions = null;

    // a map containing the method name and true if the user has the permission to the method (eg: the user has the permission to kill a job)
    // this permissions are set per user session
    private Map<String, Boolean> sessionPermissions = null;

    // a map containing the node name and true if the user has the permission to access the node
    private Map<String, Boolean> RMNodePermissions = null;

    public static LoginModel getInstance() {
        if (instance == null) {
            instance = new LoginModel();
            instance.schedulerPermissions = new HashMap<>();
            instance.sessionPermissions = new HashMap<>();
            instance.RMNodePermissions = new HashMap<>();
        }
        return instance;
    }

    /**
     * @return true if a user is currently logged in
     */
    public boolean isLoggedIn() {
        return this.logged;
    }

    public void setLoggedIn(boolean loggedIn) {
        this.logged = loggedIn;
        this.sessionId = null;
        this.login = null;
        this.schedulerPermissions = new HashMap<>();
        this.sessionPermissions = new HashMap<>();
        this.RMNodePermissions = new HashMap<>();
    }

    /**
     * @return the username of the currently logged in user, if available, or null
     */
    public String getLogin() {
        return this.login;
    }

    public void setLogin(String login) {
        this.login = login;
    }

    /**
     * @return session id of the currently logged in user
     */
    public String getSessionId() {
        return this.sessionId;
    }

    public void setSessionId(String id) {
        this.sessionId = id;
    }

    public static void addSchedulerPermissions(Map<String, Map<String, Boolean>> permissions) {
        permissions.forEach((key, value) -> instance.schedulerPermissions.put(key, value));
    }

    public static void addSessionPermissions(Map<String, Boolean> permissions) {
        permissions.forEach((key, value) -> instance.sessionPermissions.put(key, value));
    }

    public boolean userDoesNotHavePermissionToPauseTob(List<String> jobIds) {
        return userDoesNotHavePermissionToMethod(jobIds, PERMISSION_PAUSE_JOB);
    }

    public boolean userDoesNotHavePermissionToRestartAllInErrorTask(List<String> jobIds) {
        return userDoesNotHavePermissionToMethod(jobIds, PERMISSION_RESTART_ALL_IN_ERROR_TASKS);
    }

    public boolean userDoesNotHavePermissionToResumeJob(List<String> jobIds) {
        return userDoesNotHavePermissionToMethod(jobIds, PERMISSION_RESUME_JOB);
    }

    public boolean userDoesNotHavePermissionToChangeJobPriority(List<String> jobIds) {
        return userDoesNotHavePermissionToMethod(jobIds, PERMISSION_CHANGE_JOB_PRIORITY);
    }

    public boolean userDoesNotHavePermissionToKillJob(List<String> jobIds) {
        return userDoesNotHavePermissionToMethod(jobIds, PERMISSION_KILL_JOB);
    }

    public boolean userDoesNotHavePermissionToGetContent(List<String> jobIds) {
        return userDoesNotHavePermissionToMethod(jobIds, PERMISSION_GET_JOB_CONTENT);
    }

    public boolean userDoesNotHavePermissionToRemoveJob(List<String> jobIds) {
        return userDoesNotHavePermissionToMethod(jobIds, PERMISSION_REMOVE_JOB);
    }

    public boolean userDoesNotHavePermissionToGetJobServerLogs(List<String> jobIds) {
        return userDoesNotHavePermissionToMethod(jobIds, PERMISSION_GET_JOB_SERVER_LOGS);
    }

    public boolean userDoesNotHavePermissionToGetJobState(List<String> jobIds) {
        return userDoesNotHavePermissionToMethod(jobIds, PERMISSION_GET_JOB_STATE);
    }

    public boolean userDoesNotHavePermissionToGetJobResult(List<String> jobIds) {
        return userDoesNotHavePermissionToMethod(jobIds, PERMISSION_GET_JOB_RESULT);
    }

    private boolean userDoesNotHavePermissionToMethod(List<String> jobIds, String method) {
        for (String jobId : jobIds) {
            Map<String, Boolean> permissionForJob = instance.schedulerPermissions.get(jobId);
            if (permissionForJob != null && permissionForJob.containsKey(method) && !permissionForJob.get(method)) {
                return true;
            }
        }
        return false;
    }

    public boolean permissionCashedForJobIds(List<String> jobIds) {
        if (instance.schedulerPermissions == null || instance.schedulerPermissions.isEmpty()) {
            return false;
        }
        for (String jobId : jobIds) {
            Map<String, Boolean> permissionForJob = instance.schedulerPermissions.get(jobId);
            if (permissionForJob == null || !permissionForJob.keySet().containsAll(getJobPermissionMethods())) {
                return false;
            }
        }
        return true;
    }

    public List getJobPermissionMethods() {
        List<String> methods = new ArrayList<>();
        methods.add(PERMISSION_PAUSE_JOB);
        methods.add(PERMISSION_RESTART_ALL_IN_ERROR_TASKS);
        methods.add(PERMISSION_RESUME_JOB);
        methods.add(PERMISSION_CHANGE_JOB_PRIORITY);
        methods.add(PERMISSION_KILL_JOB);
        methods.add(PERMISSION_GET_JOB_CONTENT);
        methods.add(PERMISSION_REMOVE_JOB);
        methods.add(PERMISSION_GET_JOB_SERVER_LOGS);
        methods.add(PERMISSION_GET_JOB_STATE);
        methods.add(PERMISSION_GET_JOB_RESULT);
        return methods;
    }

    public List getSchedulerPermissionMethods() {
        List<String> methods = new ArrayList<>();
        methods.add(PERMISSION_SCHEDULER_FRONTEND_START);
        methods.add(PERMISSION_SCHEDULER_FRONTEND_STOP);
        methods.add(PERMISSION_SCHEDULER_FRONTEND_PAUSE);
        methods.add(PERMISSION_SCHEDULER_FRONTEND_FREEZE);
        methods.add(PERMISSION_SCHEDULER_FRONTEND_RESUME);
        methods.add(PERMISSION_SCHEDULER_FRONTEND_SHUTDOWN);
        methods.add(PERMISSION_SCHEDULER_FRONTEND_KILL);
        return methods;
    }

    public List getRmSessionPermissionMethods() {
        List<String> methods = new ArrayList<>();
        methods.add(PERMISSION_RM_LOCK_NODES);
        methods.add(PERMISSION_RM_UNLOCK_NODES);
        methods.add(PERMISSION_RM_REMOVE_NODES);
        methods.add(PERMISSION_GET_NODE_SOURCE_CONFIGURATION);
        methods.add(PERMISSION_UPDATE_DYNAMIC_PARAMETERS);
        methods.add(PERMISSION_UNDEPLOY_NODE_SOURCE);
        methods.add(PERMISSION_DEPLOY_NODE_SOURCE);
        methods.add(PERMISSION_RM_EXECUTE_SCRIPT);
        methods.add(PERMISSION_RM_GET_RM_THREAD_DUMP);
        methods.add(PERMISSION_RM_GET_NODE_THREAD_DUMP);
        return methods;
    }

    public boolean userHasPermissionToStartScheduler() {
        return sessionPermissions.containsKey(PERMISSION_SCHEDULER_FRONTEND_START) &&
               sessionPermissions.get(PERMISSION_SCHEDULER_FRONTEND_START);
    }

    public boolean userHasPermissionToStopScheduler() {
        return sessionPermissions.containsKey(PERMISSION_SCHEDULER_FRONTEND_STOP) &&
               sessionPermissions.get(PERMISSION_SCHEDULER_FRONTEND_STOP);
    }

    public boolean userHasPermissionToFreezeScheduler() {
        return sessionPermissions.containsKey(PERMISSION_SCHEDULER_FRONTEND_FREEZE) &&
               sessionPermissions.get(PERMISSION_SCHEDULER_FRONTEND_FREEZE);
    }

    public boolean userHasPermissionToResumeScheduler() {
        return sessionPermissions.containsKey(PERMISSION_SCHEDULER_FRONTEND_RESUME) &&
               sessionPermissions.get(PERMISSION_SCHEDULER_FRONTEND_RESUME);
    }

    public boolean userHasPermissionToPauseScheduler() {
        return sessionPermissions.containsKey(PERMISSION_SCHEDULER_FRONTEND_PAUSE) &&
               sessionPermissions.get(PERMISSION_SCHEDULER_FRONTEND_PAUSE);
    }

    public boolean userHasPermissionToShutDownScheduler() {
        return sessionPermissions.containsKey(PERMISSION_SCHEDULER_FRONTEND_SHUTDOWN) &&
               sessionPermissions.get(PERMISSION_SCHEDULER_FRONTEND_SHUTDOWN);
    }

    public boolean userHasPermissionToKillScheduler() {
        return sessionPermissions.containsKey(PERMISSION_SCHEDULER_FRONTEND_KILL) &&
               sessionPermissions.get(PERMISSION_SCHEDULER_FRONTEND_KILL);
    }

    public boolean userHasPermissionToLockNodes() {
        return sessionPermissions.get(PERMISSION_RM_LOCK_NODES);
    }

    public boolean userHasPermissionToUnLockNodes() {
        return sessionPermissions.get(PERMISSION_RM_UNLOCK_NODES);
    }

    public boolean userHasPermissionToRemoveNodes() {
        return sessionPermissions.get(PERMISSION_RM_REMOVE_NODES);
    }

    public boolean userHasPermissionToGetNodeSourceConfiguration() {
        return sessionPermissions.get(PERMISSION_GET_NODE_SOURCE_CONFIGURATION);
    }

    public boolean userHasPermissionToUpdateDynamicParameters() {
        return sessionPermissions.get(PERMISSION_UPDATE_DYNAMIC_PARAMETERS);
    }

    public boolean userHasPermissionToUndeployNodeSource() {
        return sessionPermissions.get(PERMISSION_UNDEPLOY_NODE_SOURCE);
    }

    public boolean userHasPermissionToDeployNodeSource() {
        return sessionPermissions.get(PERMISSION_DEPLOY_NODE_SOURCE);
    }

    public boolean userDoesNotHavePermissionToExecuteScript() {
        return sessionPermissions.containsKey(PERMISSION_RM_EXECUTE_SCRIPT) &&
               !sessionPermissions.get(PERMISSION_RM_EXECUTE_SCRIPT);
    }

    public boolean userDoesNotHavePermissionToGetNodeThreadDump() {
        return sessionPermissions.containsKey(PERMISSION_RM_GET_NODE_THREAD_DUMP) &&
               !sessionPermissions.get(PERMISSION_RM_GET_NODE_THREAD_DUMP);
    }

    public boolean userDoesNotHavePermissionToGetRmThreadDump() {
        return sessionPermissions.containsKey(PERMISSION_RM_GET_RM_THREAD_DUMP) &&
               !sessionPermissions.get(PERMISSION_RM_GET_RM_THREAD_DUMP);
    }

    public boolean userPermissionWasReceivedForNode(String node) {
        return RMNodePermissions.containsKey(node);
    }

    public boolean sessionPermissionWasReceivedForMethod(String method) {
        return sessionPermissions.containsKey(method);
    }

    /**
     * Add the user permissions for the nodes to the cashed map to avoid sending unnecessary requests
     * @param permissions the map the node and true/false if the user has or has not the permission to the node
     */
    public void addRMPermissions(Map<String, Boolean> permissions) {
        permissions.forEach((key, value) -> instance.RMNodePermissions.put(key, value));
    }

    /**
     * Check if the user has permissions to all of the given nodes
     * @param nodeUrls the list of nodes
     * @return true if the user has permissions to all of the given nodes
     */
    public boolean userHasPermissionForAllSelectedNodes(Set<String> nodeUrls) {
        return nodeUrls.stream().allMatch(node -> RMNodePermissions.containsKey(node) && RMNodePermissions.get(node));
    }

}
