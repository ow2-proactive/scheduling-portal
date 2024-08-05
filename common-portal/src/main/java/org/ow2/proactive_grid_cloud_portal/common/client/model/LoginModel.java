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

    private static final String PERMISSION_SCHEDULER_FRONTEND_SET_LABELS = "org.ow2.proactive.scheduler.core.SchedulerFrontend.setLabels";

    private static final String PERMISSION_PAUSE_JOB = "pauseJob";

    private static final String PERMISSION_RESTART_ALL_IN_ERROR_TASKS = "restartAllInErrorTasks";

    private static final String PERMISSION_RESUME_JOB = "resumeJob";

    private static final String PERMISSION_CHANGE_JOB_PRIORITY = "changeJobPriority";

    private static final String PERMISSION_KILL_JOB = "killJob";

    private static final String PERMISSION_GET_JOB_CONTENT = "getJobContent"; //Export XML, re_submit

    private static final String PERMISSION_REMOVE_JOB = "removeJob";

    private static final String PERMISSION_CHANGE_START_AT = "changeStartAt";

    private static final String PERMISSION_GET_JOB_SERVER_LOGS = "getJobServerLogs";

    private static final String PERMISSION_GET_JOB_STATE = "getJobState";

    private static final String PERMISSION_GET_JOB_RESULT = "getJobResult";

    private static final String PERMISSION_SET_LABEL_ON_JOBS = "setLabelOnJobs";

    private static final String PERMISSION_RM_EXECUTE_SCRIPT = "org.ow2.proactive.resourcemanager.core.RMCore.executeScript";

    private static final String PERMISSION_RM_GET_RM_THREAD_DUMP = "org.ow2.proactive.resourcemanager.core.RMCore.getRMThreadDump";

    private static final String PERMISSION_RM_GET_NODE_THREAD_DUMP = "org.ow2.proactive.resourcemanager.core.RMCore.getNodeThreadDump";

    private static final String PERMISSION_RM_LOCK_NODES = "org.ow2.proactive.resourcemanager.core.RMCore.lockNodes";

    private static final String PERMISSION_RM_UNLOCK_NODES = "org.ow2.proactive.resourcemanager.core.RMCore.unlockNodes";

    private static final String PERMISSION_RM_REMOVE_NODE = "org.ow2.proactive.resourcemanager.core.RMCore.removeNode";

    private static final String PERMISSION_RM_REMOVE_NODE_SOURCE = "org.ow2.proactive.resourcemanager.core.RMCore.removeNodeSource";

    private static final String PERMISSION_RM_GET_NODE_SOURCE_CONFIGURATION = "org.ow2.proactive.resourcemanager.core.RMCore.getNodeSourceConfiguration";

    private static final String PERMISSION_RM_UPDATE_DYNAMIC_PARAMETERS = "org.ow2.proactive.resourcemanager.core.RMCore.updateDynamicParameters";

    private static final String PERMISSION_RM_EDIT_NODE_SOURCE = "org.ow2.proactive.resourcemanager.core.RMCore.editNodeSource";

    private static final String PERMISSION_RM_UNDEPLOY_NODE_SOURCE = "org.ow2.proactive.resourcemanager.core.RMCore.undeployNodeSource";

    private static final String PERMISSION_RM_DEPLOY_NODE_SOURCE = "org.ow2.proactive.resourcemanager.core.RMCore.deployNodeSource";

    private static final String PERMISSION_RM_GET_SUPPORTED_NODE_SOURCE_INFRASTRUCTURES = "org.ow2.proactive.resourcemanager.core.RMCore.getSupportedNodeSourceInfrastructures";

    private static final String PERMISSION_RM_GET_INFRAS_TO_POLICIES_MAPPING = "org.ow2.proactive.resourcemanager.core.RMCore.getInfrasToPoliciesMapping";

    private static final String PERMISSION_RM_DEFINE_NODE_SOURCE = "org.ow2.proactive.resourcemanager.core.RMCore.defineNodeSource";

    public static final String AUTOMATION_DASHBOARD_PORTAL = "automation-dashboard";

    public static final String CHANGE_PRIORITIES_PERMISSION = "prioritiesPermission";

    public static final String[] AUTOMATION_DASHBOARD_PORTALS = new String[] { "catalog-portal", "workflow-execution",
                                                                               "service-automation", "job-analytics",
                                                                               "job-gantt", "node-gantt",
                                                                               "job-planner-calendar-def",
                                                                               "job-planner-calendar-def-workflows",
                                                                               "job-planner-execution-planning",
                                                                               "job-planner-gantt-chart",
                                                                               "notification-portal" };

    public static final String STUDIO_PORTAL = "studio";

    public static final String SCHEDULER_PORTAL = "scheduler";

    public static final String RM_PORTAL = "rm";

    // a map containing the job id as key and another map as value containing the method name and true/false if the user has
    // the permission to the method for the current jobId
    Map<String, Map<String, Boolean>> schedulerPermissions = null;

    // a map containing the method name and true if the user has the permission to the method (eg: the user has the permission to kill a job)
    // these permissions are set per user session
    private Map<String, Boolean> sessionPermissions = null;

    // a map containing the node name and true if the user has the admin or provider permission for the node/nodeSource
    private Map<String, Boolean> RMNodeProviderPermissions = null;

    // a map containing the node name and true if the user has the admin permission for the nodeSource
    private Map<String, Boolean> RMNodeAdminPermissions = null;

    private Map<String, Boolean> portalsAccess = null;

    private List<String> userPrioritiesPermission = null;

    public static LoginModel getInstance() {
        if (instance == null) {
            instance = new LoginModel();
            instance.schedulerPermissions = new HashMap<>();
            instance.sessionPermissions = new HashMap<>();
            instance.RMNodeProviderPermissions = new HashMap<>();
            instance.RMNodeAdminPermissions = new HashMap<>();
            instance.portalsAccess = new HashMap<>();
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
        this.RMNodeProviderPermissions = new HashMap<>();
        this.RMNodeAdminPermissions = new HashMap<>();
        this.portalsAccess = new HashMap<>();
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
     * @return The authorized priorities the user can set for a job regarding his user role
     */
    public List<String> getUserPrioritiesPermission() {
        return this.userPrioritiesPermission;
    }

    public void setUserPrioritiesPermission(List<String> userPrioritiesPermission) {
        this.userPrioritiesPermission = userPrioritiesPermission;
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
        instance.schedulerPermissions.putAll(permissions);
    }

    public static void addSessionPermissions(Map<String, Boolean> permissions) {
        instance.sessionPermissions.putAll(permissions);
    }

    public static void addPortalsPermissions(List<String> allowedPortals) {
        List<String> dashboardPortals = Arrays.asList(AUTOMATION_DASHBOARD_PORTALS);
        for (String portal : dashboardPortals) {
            instance.portalsAccess.put(portal, false);
        }
        instance.portalsAccess.put(STUDIO_PORTAL, false);
        instance.portalsAccess.put(SCHEDULER_PORTAL, false);
        instance.portalsAccess.put(RM_PORTAL, false);
        for (String portal : allowedPortals) {
            instance.portalsAccess.put(portal, true);
        }
        if (allowedPortals.stream().anyMatch(portal -> dashboardPortals.contains(portal))) {
            instance.portalsAccess.put(AUTOMATION_DASHBOARD_PORTAL, true);
        }
    }

    public boolean userDoesNotHavePermissionToPauseJobs(List<String> jobIds) {
        return userDoesNotHavePermissionToMethod(jobIds, PERMISSION_PAUSE_JOB);
    }

    public boolean userDoesNotHavePermissionToRestartAllInErrorTasks(List<String> jobIds) {
        return userDoesNotHavePermissionToMethod(jobIds, PERMISSION_RESTART_ALL_IN_ERROR_TASKS);
    }

    public boolean userDoesNotHavePermissionToResumeJobs(List<String> jobIds) {
        return userDoesNotHavePermissionToMethod(jobIds, PERMISSION_RESUME_JOB);
    }

    public boolean userDoesNotHavePermissionToChangeJobsPriority(List<String> jobIds) {
        return userDoesNotHavePermissionToMethod(jobIds, PERMISSION_CHANGE_JOB_PRIORITY);
    }

    public boolean userDoesNotHavePermissionToKillJobs(List<String> jobIds) {
        return userDoesNotHavePermissionToMethod(jobIds, PERMISSION_KILL_JOB);
    }

    public boolean userDoesNotHavePermissionToGetJobsContent(List<String> jobIds) {
        return userDoesNotHavePermissionToMethod(jobIds, PERMISSION_GET_JOB_CONTENT);
    }

    public boolean userDoesNotHavePermissionToRemoveJobs(List<String> jobIds) {
        return userDoesNotHavePermissionToMethod(jobIds, PERMISSION_REMOVE_JOB);
    }

    public boolean userDoesNotHavePermissionToGetJobsServerLogs(List<String> jobIds) {
        return userDoesNotHavePermissionToMethod(jobIds, PERMISSION_GET_JOB_SERVER_LOGS);
    }

    public boolean userDoesNotHavePermissionToGetJobsState(List<String> jobIds) {
        return userDoesNotHavePermissionToMethod(jobIds, PERMISSION_GET_JOB_STATE);
    }

    public boolean userDoesNotHavePermissionToGetJobsResult(List<String> jobIds) {
        return userDoesNotHavePermissionToMethod(jobIds, PERMISSION_GET_JOB_RESULT);
    }

    public boolean userDoesNotHavePermissionToSetLabelOnJobs(List<String> jobIds) {
        return userDoesNotHavePermissionToMethod(jobIds, PERMISSION_SET_LABEL_ON_JOBS);
    }

    public boolean userDoesNotHavePermissionToChangeStartAtValue(List<String> jobIds) {
        return userDoesNotHavePermissionToMethod(jobIds, PERMISSION_CHANGE_START_AT);
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

    public List<String> getJobPermissionMethods() {
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
        methods.add(PERMISSION_SET_LABEL_ON_JOBS);
        methods.add(PERMISSION_CHANGE_START_AT);
        return methods;
    }

    public List<String> getSchedulerPermissionMethods() {
        List<String> methods = new ArrayList<>();
        methods.add(PERMISSION_SCHEDULER_FRONTEND_START);
        methods.add(PERMISSION_SCHEDULER_FRONTEND_STOP);
        methods.add(PERMISSION_SCHEDULER_FRONTEND_PAUSE);
        methods.add(PERMISSION_SCHEDULER_FRONTEND_FREEZE);
        methods.add(PERMISSION_SCHEDULER_FRONTEND_RESUME);
        methods.add(PERMISSION_SCHEDULER_FRONTEND_SHUTDOWN);
        methods.add(PERMISSION_SCHEDULER_FRONTEND_KILL);
        methods.add(PERMISSION_SCHEDULER_FRONTEND_SET_LABELS);
        return methods;
    }

    public List<String> getRmSessionPermissionMethods() {
        List<String> methods = new ArrayList<>();
        methods.add(PERMISSION_RM_LOCK_NODES);
        methods.add(PERMISSION_RM_UNLOCK_NODES);
        methods.add(PERMISSION_RM_REMOVE_NODE);
        methods.add(PERMISSION_RM_REMOVE_NODE_SOURCE);
        methods.add(PERMISSION_RM_GET_NODE_SOURCE_CONFIGURATION);
        methods.add(PERMISSION_RM_UPDATE_DYNAMIC_PARAMETERS);
        methods.add(PERMISSION_RM_EDIT_NODE_SOURCE);
        methods.add(PERMISSION_RM_UNDEPLOY_NODE_SOURCE);
        methods.add(PERMISSION_RM_DEPLOY_NODE_SOURCE);
        methods.add(PERMISSION_RM_EXECUTE_SCRIPT);
        methods.add(PERMISSION_RM_GET_RM_THREAD_DUMP);
        methods.add(PERMISSION_RM_GET_NODE_THREAD_DUMP);
        methods.add(PERMISSION_RM_GET_SUPPORTED_NODE_SOURCE_INFRASTRUCTURES);
        methods.add(PERMISSION_RM_GET_INFRAS_TO_POLICIES_MAPPING);
        methods.add(PERMISSION_RM_DEFINE_NODE_SOURCE);
        return methods;
    }

    public List<String> getPortalsPermissionsNames() {
        List<String> portals = new ArrayList<>();
        portals.addAll(Arrays.asList(AUTOMATION_DASHBOARD_PORTALS));
        portals.add(STUDIO_PORTAL);
        portals.add(SCHEDULER_PORTAL);
        portals.add(RM_PORTAL);
        return portals;
    }

    public boolean userHasPermissionToAutomationDashboard() {
        return portalsAccess.get(AUTOMATION_DASHBOARD_PORTAL) != null ? portalsAccess.get(AUTOMATION_DASHBOARD_PORTAL)
                                                                      : false;
    }

    public boolean userHasPermissionToStudio() {
        return portalsAccess.get(STUDIO_PORTAL) != null ? portalsAccess.get(STUDIO_PORTAL) : false;
    }

    public boolean userHasPermissionToScheduler() {
        return portalsAccess.get(SCHEDULER_PORTAL) != null ? portalsAccess.get(SCHEDULER_PORTAL) : false;
    }

    public boolean userHasPermissionToRm() {
        return portalsAccess.get(RM_PORTAL) != null ? portalsAccess.get(RM_PORTAL) : false;
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

    public boolean userHasPermissionToSetLabels() {
        return sessionPermissions.containsKey(PERMISSION_SCHEDULER_FRONTEND_SET_LABELS) &&
               sessionPermissions.get(PERMISSION_SCHEDULER_FRONTEND_SET_LABELS);
    }

    public boolean userHasPermissionToLockNodes() {
        return sessionPermissions.get(PERMISSION_RM_LOCK_NODES);
    }

    public boolean userHasPermissionToUnLockNodes() {
        return sessionPermissions.get(PERMISSION_RM_UNLOCK_NODES);
    }

    public boolean userHasPermissionToRemoveNode() {
        return sessionPermissions.get(PERMISSION_RM_REMOVE_NODE);
    }

    public boolean userHasPermissionToRemoveNodeSource() {
        return sessionPermissions.get(PERMISSION_RM_REMOVE_NODE_SOURCE);
    }

    public boolean userHasPermissionToGetNodeSourceConfiguration() {
        return sessionPermissions.get(PERMISSION_RM_GET_NODE_SOURCE_CONFIGURATION);
    }

    public boolean userHasPermissionToUpdateDynamicParameters() {
        return sessionPermissions.get(PERMISSION_RM_UPDATE_DYNAMIC_PARAMETERS);
    }

    public boolean userHasPermissionToEditNodeSource() {
        return sessionPermissions.get(PERMISSION_RM_EDIT_NODE_SOURCE);
    }

    public boolean userHasPermissionToUndeployNodeSource() {
        return sessionPermissions.get(PERMISSION_RM_UNDEPLOY_NODE_SOURCE);
    }

    public boolean userHasPermissionToDeployNodeSource() {
        return sessionPermissions.get(PERMISSION_RM_DEPLOY_NODE_SOURCE);
    }

    public boolean userHasPermissionToExecuteScript() {
        return sessionPermissions.get(PERMISSION_RM_EXECUTE_SCRIPT);
    }

    public boolean userHasPermissionToGetNodeThreadDump() {
        return sessionPermissions.get(PERMISSION_RM_GET_NODE_THREAD_DUMP);
    }

    public boolean userHasPermissionToGetRmThreadDump() {
        return sessionPermissions.get(PERMISSION_RM_GET_RM_THREAD_DUMP);
    }

    public boolean userHasPermissionToGetInfrasToPoliciesMapping() {
        return sessionPermissions.get(PERMISSION_RM_GET_INFRAS_TO_POLICIES_MAPPING);
    }

    public boolean userHasPermissionToGetSupportedNodeSourceInfras() {
        return sessionPermissions.get(PERMISSION_RM_GET_SUPPORTED_NODE_SOURCE_INFRASTRUCTURES);
    }

    public boolean userHasPermissionToDefineNodeSource() {
        return sessionPermissions.get(PERMISSION_RM_DEFINE_NODE_SOURCE);
    }

    public boolean userProviderPermissionWasReceivedForNode(String node) {
        return RMNodeProviderPermissions.containsKey(node);
    }

    public boolean userAdminPermissionWasReceivedForNodeSource(String node) {
        return RMNodeAdminPermissions.containsKey(node);
    }

    public boolean sessionPermissionWasReceivedForMethod(String method) {
        return sessionPermissions.containsKey(method);
    }

    public boolean sessionPermissionWasReceivedForPortal(String portal) {
        return portalsAccess.containsKey(portal);
    }

    /**
     * Add the user admin and provider permissions for the nodes/nodeSources to the cashed map to avoid sending unnecessary requests
     *
     * @param url the node/nodeSource url
     * @param permission true/false if the user has or has not admin or provider permission to the nodeSource
     */
    public void addRMProviderPermissions(String url, boolean permission) {
        instance.RMNodeProviderPermissions.put(url, permission);
    }

    /**
     * Add the user admin permissions for the nodeSources to the cashed map to avoid sending unnecessary requests
     *
     * @param url the nodeSource url
     * @param permission true/false if the user has or not admin permission to the nodeSource
     */
    public void addRMAdminPermissions(String url, boolean permission) {
        instance.RMNodeAdminPermissions.put(url, permission);
    }

    /**
     * Check if the user has admin or provider permissions to the given node/nodeSource
     *
     * @param url the url of the node source
     * @return true if the user has admin or provider permissions to the given node/nodeSource
     */
    public boolean userHasProviderPermissionForNode(String url) {
        return RMNodeProviderPermissions.containsKey(url) && RMNodeProviderPermissions.get(url);
    }

    /**
     * Check if the user has admin permissions to the given nodeSources
     *
     * @param url the url of the node source
     * @return true if the user has admin permissions to the given nodeSources
     */
    public boolean userHasAdminPermissionForNodeSource(String url) {
        return RMNodeAdminPermissions.containsKey(url) && RMNodeAdminPermissions.get(url);
    }

}
