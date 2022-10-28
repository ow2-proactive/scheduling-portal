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
package org.ow2.proactive_grid_cloud_portal.rm.client;

import java.util.*;
import java.util.stream.Collectors;

import org.ow2.proactive_grid_cloud_portal.common.client.*;
import org.ow2.proactive_grid_cloud_portal.common.client.Model.StatHistory;
import org.ow2.proactive_grid_cloud_portal.common.client.Model.StatHistory.Range;
import org.ow2.proactive_grid_cloud_portal.common.client.json.JSONUtils;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LogModel;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LoginModel;
import org.ow2.proactive_grid_cloud_portal.common.shared.Config;
import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSource.Host;
import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSource.Host.Node;
import org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.serialization.NodeSourceConfigurationParser;
import org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.serialization.export.catalog.ExportToCatalogConfirmWindow;
import org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.serialization.export.file.ExportInfrastructureToFileHandler;
import org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.serialization.export.file.ExportNodeSourceToFileHandler;
import org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.serialization.export.file.ExportPolicyToFileHandler;
import org.ow2.proactive_grid_cloud_portal.rm.shared.CatalogKind;
import org.ow2.proactive_grid_cloud_portal.rm.shared.RMConfig;

import com.google.gwt.core.client.Callback;
import com.google.gwt.core.client.GWT.UncaughtExceptionHandler;
import com.google.gwt.http.client.Request;
import com.google.gwt.json.client.*;
import com.google.gwt.user.client.Cookies;
import com.google.gwt.user.client.Random;
import com.google.gwt.user.client.Timer;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.RecordList;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;


/**
 * Logic that interacts between the remote RM and the local Model
 * <p>
 * The Controller can be accessed statically by the client to ensure
 * coherent modification of the Model data:
 * <ul><li>views submit actions to the Controller,
 * <li>the Controller performs the actions,
 * <li>the Controller updates new Data to the Model,
 * <li>the view displays what it reads from the Model.
 * </code>
 *
 * @author mschnoor
 */
public class RMController extends Controller implements UncaughtExceptionHandler {

    static final String LOCAL_SESSION_COOKIE = "pa.sched.local_session";

    private static final int AUTO_LOGIN_TIMER_PERIOD_IN_MS = 1000;

    @Override
    public String getLoginSettingKey() {
        return LOGIN_SETTING;
    }

    @Override
    public String getLogo350Url() {
        return RMImagesUnbundled.LOGO_350;
    }

    @Override
    public String getPortalLogo() {
        return RMImagesUnbundled.PPS_DEPLOY;
    }

    /** if this is different than LOCAL_SESSION cookie, we need to disconnect */
    private String localSessionNum;

    /** periodically updates the local state */
    private Timer updater = null;

    /** periodically fetches runtime stats */
    private Timer statsUpdater = null;

    /** remote gwt service */
    private RMServiceAsync rm = null;

    /** stores client data */
    private RMModelImpl model = null;

    /** shown when not logged in */
    private LoginPage loginPage = null;

    /** shown when logged in */
    private RMPage rmPage = null;

    /** result of the latest call to {@link RMServiceAsync#getStatHistory(String, String, AsyncCallback)} */
    private Request statHistReq = null;

    /** system.currenttimemillis of last StatHistory call */
    private long lastStatHistReq = 0;

    private Timer autoLoginTimer;

    private NodeSourceConfigurationParser nodeSourceConfigurationParser;

    /**
     * Default constructor
     *
     * @param rm rm server
     */
    RMController(RMServiceAsync rm) {
        this.rm = rm;
        this.model = new RMModelImpl();
        this.nodeSourceConfigurationParser = new NodeSourceConfigurationParser();
        this.init();
    }

    /**
     * Call this once upon creation
     */
    private void init() {
        final String session = Settings.get().getSetting(SESSION_SETTING);

        if (session != null) {
            LoadingMessage loadingMessage = new LoadingMessage();
            loadingMessage.draw();
            tryLogin(session, loadingMessage);
        } else {
            this.loginPage = new LoginPage(this, null);
            tryToLoginIfLoggedInScheduler();
        }
    }

    private void tryLogin(final String session, final VLayout loadingMessage) {
        this.rm.getState(session, new AsyncCallback<String>() {
            public void onSuccess(String result) {
                if (result.startsWith("you are not connected")) {
                    if (loadingMessage != null) {
                        loadingMessage.destroy();
                    }
                    Settings.get().clearSetting(SESSION_SETTING);
                    RMController.this.loginPage = new LoginPage(RMController.this, null);
                    tryToLoginIfLoggedInScheduler();
                } else {
                    if (loadingMessage != null) {
                        loadingMessage.destroy();
                    }
                    login(session, Settings.get().getSetting(LOGIN_SETTING));
                    LogModel.getInstance().logMessage("Rebound session " + session);
                }
            }

            public void onFailure(Throwable caught) {
                if (loadingMessage != null) {
                    loadingMessage.destroy();
                }
                Settings.get().clearSetting(SESSION_SETTING);
                RMController.this.loginPage = new LoginPage(RMController.this, null);
                tryToLoginIfLoggedInScheduler();
            }
        });
    }

    private void tryToLoginIfLoggedInScheduler() {
        autoLoginTimer = new Timer() {
            @Override
            public void run() {
                String session = Settings.get().getSetting(SESSION_SETTING);
                if (session != null) {
                    tryLogin(session, null);
                }
            }
        };
        autoLoginTimer.scheduleRepeating(AUTO_LOGIN_TIMER_PERIOD_IN_MS);
    }

    private void stopTryingLoginIfLoggedInScheduler() {
        if (autoLoginTimer != null) {
            autoLoginTimer.cancel();
        }
    }

    @Override
    public void login(final String sessionId, final String login) {
        stopTryingLoginIfLoggedInScheduler();
        rm.portalAccess(sessionId, new AsyncCallback<String>() {
            @Override
            public void onFailure(Throwable caught) {
                String msg = JSONUtils.getJsonErrorMessage(caught);
                LogModel.getInstance().logImportantMessage("Failed to get REST server version: " + msg);
            }

            @Override
            public void onSuccess(String result) {
                if (result.contains("true")) {
                    rm.getVersion(new AsyncCallback<String>() {
                        public void onSuccess(String result) {
                            JSONObject obj = JSONParser.parseStrict(result).isObject();
                            String rmVer = obj.get("rm").isString().stringValue();
                            String restVer = obj.get("rest").isString().stringValue();
                            Config.get().set(RMConfig.RM_VERSION, rmVer);
                            Config.get().set(RMConfig.REST_VERSION, restVer);

                            __login(sessionId, login);
                        }

                        public void onFailure(Throwable caught) {
                            String msg = JSONUtils.getJsonErrorMessage(caught);
                            LogModel.getInstance().logImportantMessage("Failed to get REST server version: " + msg);
                        }
                    });
                } else {
                    RMController.this.loginPage = new LoginPage(RMController.this,
                                                                "You do not have rights to access Resource Manager portal");
                }
            }
        });
    }

    private void __login(String sessionId, String login) {
        LoginModel loginModel = LoginModel.getInstance();
        loginModel.setLoggedIn(true);
        loginModel.setLogin(login);
        loginModel.setSessionId(sessionId);

        if (this.loginPage != null) {
            this.loginPage.destroy();
            this.loginPage = null;
        }
        this.rmPage = new RMPage(this);
        this.fetchRMMonitoring();
        this.fetchNodesLimit();
        this.startTimer();

        Settings.get().setSetting(SESSION_SETTING, sessionId);
        if (login != null) {
            Settings.get().setSetting(LOGIN_SETTING, login);
        } else {
            Settings.get().clearSetting(LOGIN_SETTING);
        }

        String lstr = "";
        if (login != null) {
            lstr += " as " + login;
        }

        // this cookie is reset to a random int on every login:
        // if another session in another tab has a different localSessionNUm
        // than the one in the domain cookie, then we exit
        this.localSessionNum = "" + System.currentTimeMillis() + "_" + Random.nextInt();
        Cookies.setCookie(LOCAL_SESSION_COOKIE, this.localSessionNum);

        checkPortalsPermissions();
        checkRmMethodsPermissions();

        LogModel.getInstance().logMessage("Connected to " + Config.get().getRestUrl() + lstr + " (sessionId=" +
                                          loginModel.getSessionId() + ")");
    }

    @Override
    public void logout() {
        LoginModel loginModel = LoginModel.getInstance();
        if (!loginModel.isLoggedIn())
            return;

        Settings.get().clearSetting(SESSION_SETTING);
        rm.logout(loginModel.getSessionId(), new AsyncCallback<Void>() {

            public void onFailure(Throwable caught) {
            }

            public void onSuccess(Void result) {
            }

        });

        loginModel.setLoggedIn(false);
        teardown(null);
        tryToLoginIfLoggedInScheduler();
    }

    /**
     * Start the timer that will fetch new node states periodically
     */
    private void startTimer() {
        if (this.updater != null)
            throw new IllegalStateException("Updated is running");

        this.updater = new Timer() {
            @Override
            public void run() {

                if (!localSessionNum.equals(Cookies.getCookie(LOCAL_SESSION_COOKIE))) {
                    teardown("Duplicate session detected!<br>" +
                             "Another tab or window in this browser is accessing this page.");
                }
                fetchRMMonitoring();

            }
        };
        this.updater.schedule(RMConfig.get().getClientRefreshTime());

        this.statsUpdater = new Timer() {
            @Override
            public void run() {
                fetchStatHistory();
            }
        };
        this.statsUpdater.scheduleRepeating(RMConfig.get().getStatisticsRefreshTime());
    }

    private void fetchNodesLimit() {
        this.rm.getState(LoginModel.getInstance().getSessionId(), new AsyncCallback<String>() {
            public void onSuccess(String result) {
                // Parse json response to extract the current node limit
                JSONObject rmState = JSONParser.parseStrict(result).isObject();
                if (rmState == null) {
                    LogModel.getInstance().logMessage("Failed to parse json rmState: " + result);
                } else {
                    JSONValue maxNumberOfNodes = rmState.get("maxNumberOfNodes");
                    if (maxNumberOfNodes != null && maxNumberOfNodes.isNumber() != null) {
                        model.setMaxNumberOfNodes(Long.parseLong(maxNumberOfNodes.isNumber().toString()));
                    }
                }
            }

            public void onFailure(Throwable caught) {
                LogModel.getInstance()
                        .logMessage("Failed to access node limit through rmState: " + caught.getMessage());
            }
        });
    }

    /**
     * Perform the server call to fetch RRD history statistics
     */
    private void fetchStatHistory() {
        String range = "";

        // this 'sources' set order in which we request the fields,
        // it should be the same as in scheduling RMRest::dataSources
        // PS: "PendingTasksCount" corresponds to "Needed" nodes.
        String[] sources = new String[] { "AvailableNodesCount", "FreeNodesCount", "NeededNodesCount", "BusyNodesCount",
                                          "DeployingNodesCount", "ConfigNodesCount", "DownNodesCount", "LostNodesCount",
                                          "AverageActivity" };

        long updateFreq = Range.YEAR_1.getUpdateFrequency();
        boolean changedRange = false;
        for (String src : sources) {
            if (model.getStatHistory(src) != null &&
                !model.getStatHistory(src).range.equals(model.getRequestedStatHistoryRange(src))) {
                changedRange = true;
            }

            Range r = model.getRequestedStatHistoryRange(src);
            range += r.getChar();
            if (r.getUpdateFrequency() < updateFreq)
                updateFreq = r.getUpdateFrequency();
        }

        final long now = System.currentTimeMillis();
        final long dt = now - this.lastStatHistReq;

        // do not update stats every 5sec if the graphed range is large
        if (dt > updateFreq * 1000 || changedRange) {
            this.lastStatHistReq = now;

            this.statHistReq = rm.getStatHistory(LoginModel.getInstance().getSessionId(),
                                                 range,
                                                 new AsyncCallback<String>() {
                                                     @Override
                                                     public void onSuccess(String result) {

                                                         JSONValue val = RMController.parseJSON(result);
                                                         JSONObject obj = val.isObject();

                                                         HashMap<String, StatHistory> stats = new HashMap<String, StatHistory>();
                                                         for (String source : obj.keySet()) {
                                                             JSONArray arr = obj.get(source).isArray();

                                                             List<Double> values = new ArrayList<>();
                                                             for (int i = 0; i < arr.size(); i++) {
                                                                 JSONValue dval = arr.get(i);
                                                                 if (dval.isNumber() != null) {
                                                                     values.add(dval.isNumber().doubleValue());
                                                                 } else if (i < arr.size() - 1) {
                                                                     values.add(Double.NaN);
                                                                 }

                                                             }

                                                             if (source.equals("NeededNodesCount")) {
                                                                 final Double lastValue = values.get(values.size() - 1);
                                                                 if (0 <= lastValue && !lastValue.isNaN()) {
                                                                     model.setNeededNodes(lastValue.intValue());
                                                                 } else {
                                                                     model.setNeededNodes(0);
                                                                 }
                                                             }

                                                             StatHistory st = new StatHistory(source,
                                                                                              values,
                                                                                              model.getRequestedStatHistoryRange(source));
                                                             stats.put(source, st);
                                                         }
                                                         model.setStatHistory(stats);
                                                         LogModel.getInstance()
                                                                 .logMessage("Updated Statistics History in " +
                                                                             (System.currentTimeMillis() - now) + "ms");
                                                     }

                                                     @Override
                                                     public void onFailure(Throwable caught) {
                                                         if (JSONUtils.getJsonErrorCode(caught) == 401) {
                                                             teardown("You have been disconnected from the server.");
                                                         } else {
                                                             error("Failed to fetch Statistics History: " +
                                                                   JSONUtils.getJsonErrorMessage(caught));
                                                         }
                                                     }
                                                 });
        }

    }

    /**
     * Change the requested history range for a given set of sources,
     * store it in the model, perform statistic fetch
     *
     * @param r      range to set
     * @param source source names
     */
    public void setRuntimeRRDRange(Range r, String... source) {
        for (String src : source) {
            model.setRequestedStatHistoryRange(src, r);
        }

        if (statHistReq != null && statHistReq.isPending())
            this.statHistReq.cancel();
        fetchStatHistory();
    }

    /**
     * Perform the server call to fetch current nodes states,
     * store it on the model, notify listeners
     */
    private void fetchRMMonitoring() {
        final long t = System.currentTimeMillis();

        rm.getMonitoring(LoginModel.getInstance().getSessionId(), model.getMaxCounter(), new AsyncCallback<String>() {
            public void onSuccess(String result) {
                if (!LoginModel.getInstance().isLoggedIn()) {
                    return;
                }

                long counterBefore = model.getMaxCounter();
                updateModelBasedOnResponse(result);
                long counterAfter = model.getMaxCounter();
                if (counterBefore != counterAfter) {
                    updater.schedule(RMConfig.get().getClientBurstRefreshTime());
                } else {
                    updater.schedule(RMConfig.get().getClientRefreshTime());
                }
                LogModel.getInstance()
                        .logMessage("[ " + (System.currentTimeMillis() % 1000000) + " ]Processed RM/monitoring in " +
                                    (System.currentTimeMillis() - t) + "ms " + counterBefore + " -> " + counterAfter);
            }

            public void onFailure(Throwable caught) {
                model.setMaxCounter(-1);
                if (JSONUtils.getJsonErrorCode(caught) == 401) {
                    teardown("You have been disconnected from the server.");
                } else {
                    error("Failed to fetch RM State: " + JSONUtils.getJsonErrorMessage(caught));
                }
            }
        });
    }

    /**
     * Model is rendered in the CompactView based on the new and old (current) model.
     * That is why, we take old model and clone it. Then we process server response
     * and alternate new model by adding/removing node/nodesources.
     * @param json
     */
    private void updateModelBasedOnResponse(String json) {
        JSONObject obj = this.parseJSON(json).isObject();

        final long currentCounter = model.getMaxCounter();

        final Long latestCounter = Long.valueOf(obj.get("latestCounter").isNumber().toString());

        model.setMaxCounter(latestCounter);

        HashMap<String, NodeSource> newNodeSources = new HashMap<>();
        if (isRegularRequest(currentCounter, latestCounter)) {
            copyNodesSources(model.getNodeSources(), newNodeSources);
        }

        final List<NodeSource> nodeSourceList = processNodeSources(newNodeSources, obj);

        final List<Node> nodeList = new LinkedList<>();

        // process nodes
        JSONArray jsNodes = obj.get("nodesEvents").isArray();
        for (int i = 0; i < jsNodes.size(); i++) {
            try {
                JSONObject jsNode = jsNodes.get(i).isObject();

                final Node node = parseNode(jsNode);

                nodeList.add(node);

                final NodeSource nodeSource = newNodeSources.get(node.getSourceName());

                // if node source was not deleted
                if (nodeSource != null) {

                    String userAccessType = retrieveUserAccessType(nodeSource);
                    node.setUserAccessType(userAccessType);

                    if (!node.isRemoved()) {
                        addNodeToNodeSource(node, nodeSource);
                    } else {
                        removeNodeFromNodeSource(node, nodeSource);
                    }
                }

            } catch (Throwable t) {
                System.out.println("Failed to parse node : ");
                System.out.println(jsNodes.get(i).toString());
                t.printStackTrace();

                LogModel.getInstance().logCriticalMessage(t.getClass().getName() + ": " + t.getMessage() +
                                                          " for input: " + jsNodes.get(i).toString());
            }
        }

        model.setNodes(newNodeSources);
        model.nodesUpdate(newNodeSources);
        model.updateByDelta(nodeSourceList, nodeList);

        recalculatePhysicalVirtualHosts();

        recalculateStatistics();

    }

    private String retrieveUserAccessType(NodeSource nodeSource) {
        final String iHopeItNeverChange = "user access type [";
        String sourceDescription = nodeSource.getSourceDescription();
        if (sourceDescription.contains(iHopeItNeverChange)) {
            int begin = sourceDescription.indexOf(iHopeItNeverChange) + iHopeItNeverChange.length();
            int end = sourceDescription.indexOf("]", begin);
            return sourceDescription.substring(begin, end);
        } else {
            LogModel.getInstance()
                    .logMessage("NodeSource[" + nodeSource.getSourceName() + "] does not seem to have userAccessType.");
            return "";
        }
    }

    /**
     * Decides whether it was regular server request, or not.
     * 'Not regular' request means that something went strange, e.g. server was restarted,
     * or it is the very first request of this type.
     * It it is regular, than rm portal should treat server response as a delta.
     * If it is not regular, than rm portal should 'forget' what it knew, and start from the scratch
     * @param currentCounter is current counter that client is aware of
     * @param latestCounter is counter received from server
     * @return true if request is regular
     */
    private boolean isRegularRequest(long currentCounter, Long latestCounter) {
        return 0 <= currentCounter && currentCounter <= latestCounter;
    }

    /**
     * Add and remove NodeSources
     * @param newNodeSources
     * @param obj
     */
    private List<NodeSource> processNodeSources(HashMap<String, NodeSource> newNodeSources, JSONObject obj) {
        List<NodeSource> nodeSourceList = new LinkedList<>();
        JSONArray jsNodeSources = obj.get("nodeSource").isArray();
        for (int i = 0; i < jsNodeSources.size(); i++) {
            JSONObject jsNodeSource = jsNodeSources.get(i).isObject();

            NodeSource nodeSource = parseNodeSource(jsNodeSource);
            nodeSourceList.add(new NodeSource(nodeSource));
            if (nodeSource.isRemoved()) {
                newNodeSources.remove(nodeSource.getSourceName());
            } else {
                newNodeSources.put(nodeSource.getSourceName(), nodeSource);
            }
        }
        return nodeSourceList;
    }

    private void removeNodeFromNodeSource(Node node, NodeSource nodeSource) {
        if (node.isDeployingNode()) {
            nodeSource.getDeploying().remove(node.getNodeUrl());
        } else {
            Host host = nodeSource.getHosts().get(node.getHostName());

            if (host != null) {
                host.getNodes().remove(node.getNodeUrl());

                if (host.getNodes().isEmpty()) {
                    nodeSource.getHosts().remove(host);
                }
            }
        }
    }

    private void addNodeToNodeSource(Node node, NodeSource nodeSource) {
        // as deploying node
        if (node.isDeployingNode()) {

            nodeSource.getDeploying().put(node.getNodeUrl(), node);

        } else { // as already deployed node
            Host host = nodeSource.getHosts().get(node.getHostName());

            if (host == null) { // create host if there is no host
                host = new Host(node.getHostName(), node.getSourceName());
                nodeSource.getHosts().put(node.getHostName(), host);
            }

            host.getNodes().put(node.getNodeUrl(), node);

            if (node.isVirtual()) {
                host.setVirtual(true);
            }
        }
    }

    /**
     * @return clones node sources with all hosts and nodes
     */
    private void copyNodesSources(Map<String, NodeSource> oldNodeSources, HashMap<String, NodeSource> newNodeSources) {
        for (NodeSource nodeSource : oldNodeSources.values()) {
            NodeSource newNodeSource = new NodeSource(nodeSource);
            newNodeSources.put(newNodeSource.getSourceName(), newNodeSource);
        }
    }

    private void recalculatePhysicalVirtualHosts() {
        int numPhysical = 0;
        int numVirtual = 0;
        for (NodeSource nos : model.getNodeSources().values()) {
            for (Host h : nos.getHosts().values()) {
                if (h.isVirtual()) {
                    numVirtual++;
                } else {
                    numPhysical++;
                }
            }
        }

        model.setNumPhysicalHosts(numPhysical);
        model.setNumVirtualHosts(numVirtual);

    }

    private void recalculateStatistics() {
        model.setNumBusy(0);
        model.setNumConfiguring(0);
        model.setNumDeploying(0);
        model.setNumDown(0);
        model.setNumFree(0);
        model.setNumLost(0);
        model.setNumToBeRemoved(0);
        model.setNumLocked(0);
        model.setNumDeployedNodeSources(0);
        model.setNumUndeployedNodeSources(0);

        for (NodeSource nodeSource : model.getNodeSources().values()) {
            switch (nodeSource.getNodeSourceStatus()) {
                case NODES_DEPLOYED:
                    model.setNumDeployedNodeSources(model.getNumDeployedNodeSources() + 1);
                    break;
                case NODES_UNDEPLOYED:
                    model.setNumUndeployedNodeSources(model.getNumUndeployedNodeSources() + 1);
                    break;
            }

            for (Node node : nodeSource.getDeploying().values()) {
                recalculateStatistics(node);
            }

            for (Host host : nodeSource.getHosts().values()) {
                for (Node node : host.getNodes().values()) {
                    recalculateStatistics(node);
                }
            }
        }
    }

    private void recalculateStatistics(Node node) {
        switch (node.getNodeState()) {
            case BUSY:
                model.setNumBusy(model.getNumBusy() + 1);
                break;
            case CONFIGURING:
                model.setNumConfiguring(model.getNumConfiguring() + 1);
                break;
            case DEPLOYING:
                model.setNumDeploying(model.getNumDeploying() + 1);
                break;
            case DOWN:
                model.setNumDown(model.getNumDown() + 1);
                break;
            case FREE:
                model.setNumFree(model.getNumFree() + 1);
                break;
            case LOST:
                model.setNumLost(model.getNumLost() + 1);
                break;
            case TO_BE_REMOVED:
                model.setNumToBeRemoved(model.getNumToBeRemoved() + 1);
                break;
        }
        if (node.isLocked()) {
            model.setNumLocked(model.getNumLocked() + 1);
        }
    }

    private NodeSource parseNodeSource(JSONObject nsObj) {
        String sourceName = nsObj.get("sourceName").isString().stringValue();
        String sourceDescription = getJsonStringNullable(nsObj, "sourceDescription");
        LinkedHashMap<String, String> additionalInformation = getJsonMapNullable(nsObj, "additionalInformation");
        String nodeSourceAdmin = nsObj.get("nodeSourceAdmin").isString().stringValue();
        String nodeSourceStatus = getJsonStringNullable(nsObj, "nodeSourceStatus");
        String eventType = getJsonStringNullable(nsObj, "eventType");
        return new NodeSource(sourceName,
                              sourceDescription,
                              additionalInformation,
                              nodeSourceAdmin,
                              nodeSourceStatus,
                              eventType);
    }

    private Node parseNode(JSONObject nodeObj) {
        String hostName = nodeObj.get("hostName").isString().stringValue();
        String nss = nodeObj.get("nodeSource").isString().stringValue();

        String eventType = getJsonStringNullable(nodeObj, "eventType");

        String nodeUrl = nodeObj.get("nodeUrl").isString().stringValue();
        String nodeState = nodeObj.get("nodeState").isString().stringValue();
        String nodeInfo = nodeObj.get("nodeInfo").isString().stringValue();
        String timeStampFormatted = nodeObj.get("timeStampFormatted").isString().stringValue();
        long timeStamp = Math.round(nodeObj.get("timeStamp").isNumber().doubleValue());
        String nodeProvider = nodeObj.get("nodeProvider").isString().stringValue();

        String nodeOwner = getJsonStringNullable(nodeObj, "nodeOwner");
        String vmName = getJsonStringNullable(nodeObj, "vmname");
        String description = getJsonStringNullable(nodeObj, "nodeInfo");
        String defaultJMXUrl = getJsonStringNullable(nodeObj, "defaultJMXUrl");
        String proactiveJMXUrl = getJsonStringNullable(nodeObj, "proactiveJMXUrl");

        Map<String, String> usageInfo = Optional.ofNullable(nodeObj.get("usageInfo"))
                                                .map(JSONValue::isObject)
                                                .map(json -> {
                                                    Map<String, String> metaMap = new HashMap<>();
                                                    for (String key : json.keySet()) {
                                                        String value = json.get(key).isString().stringValue();
                                                        metaMap.put(key, value);
                                                    }
                                                    return metaMap;
                                                })
                                                .orElse(new HashMap<>());
        List<String> tokens = Optional.ofNullable(nodeObj.get("tokens")).map(JSONValue::isArray).map(arr -> {
            List<String> ts = new ArrayList<>(arr.size());
            for (int i = 0; i < arr.size(); ++i) {
                ts.add(arr.get(i).isString().stringValue());
            }
            return ts;
        }).orElse(Collections.emptyList());

        boolean isLocked = getJsonBooleanNullable(nodeObj, "locked", false);
        long lockTime = getJsonLongNullable(nodeObj, "lockTime", -1);
        String nodeLocker = getJsonStringNullable(nodeObj, "nodeLocker");

        return new Node(nodeUrl,
                        nodeState,
                        nodeInfo,
                        timeStamp,
                        timeStampFormatted,
                        nodeProvider,
                        nodeOwner,
                        nss,
                        hostName,
                        vmName,
                        description,
                        defaultJMXUrl,
                        proactiveJMXUrl,
                        isLocked,
                        lockTime,
                        nodeLocker,
                        eventType,
                        usageInfo,
                        tokens);
    }

    private LinkedHashMap<String, String> getJsonMapNullable(JSONObject jsonObject, String attributeName) {
        JSONObject mapAsJSONObject = jsonObject.get(attributeName).isObject();

        if (mapAsJSONObject == null) {
            return (LinkedHashMap<String, String>) Collections.EMPTY_MAP;
        }

        LinkedHashMap<String, String> result = new LinkedHashMap<>();
        Iterator<String> mapAsJSONObjectKeysIterator = mapAsJSONObject.keySet().iterator();
        while (mapAsJSONObjectKeysIterator.hasNext()) {
            String currentKey = mapAsJSONObjectKeysIterator.next();
            String currentValue = mapAsJSONObject.get(currentKey).isString().stringValue();
            result.put(currentKey, currentValue);
        }

        return result;
    }

    public void fetchLoggersSettings(ListGrid loggersGrid) {
        this.rm.getCurrentLoggers(LoginModel.getInstance().getSessionId(), new AsyncCallback<String>() {
            public void onSuccess(String loggersJson) {
                JSONObject loggersObject = JSONParser.parseStrict(loggersJson).isObject();
                if (loggersObject == null) {
                    LogModel.getInstance()
                            .logCriticalMessage("Failed to parse the JSON loggers object: " + loggersObject);
                } else {
                    Map<String, String> loggersMap = new HashMap<>();
                    loggersObject.keySet()
                                 .forEach(key -> loggersMap.put(key, loggersObject.get(key).isString().stringValue()));
                    model.setLoggersConfiguration(loggersMap);
                    updateLoggersGrid(loggersGrid);
                }
            }

            public void onFailure(Throwable caught) {
                LogModel.getInstance().logImportantMessage(
                                                           "Failed to get the map of (logger_name, level) from the server: " +
                                                           caught.getMessage());
                rmPage.disableLoggersMenuItem();
            }
        });
    }

    public void updateLoggersGrid(ListGrid loggersGrid) {
        int i = 0;
        Map<String, String> parsedLoggers = model.getLoggersConfiguration();
        ListGridRecord[] loggersRecords = new ListGridRecord[parsedLoggers.size()];

        for (Map.Entry mapentry : parsedLoggers.entrySet()) {
            ListGridRecord newLoggerRecord = new ListGridRecord();
            newLoggerRecord.setAttribute("logger", mapentry.getKey().toString());
            newLoggerRecord.setAttribute("level", mapentry.getValue().toString());

            loggersRecords[i] = newLoggerRecord;
            i++;
        }
        loggersGrid.setData(loggersRecords);

    }

    public void setLoggersSettings(ListGrid loggersGrid) {

        RecordList loggersData = new RecordList();

        for (int i = 0; i < loggersGrid.getTotalRows(); i++) {
            loggersData.add(loggersGrid.getEditedRecord(i));
        }
        Record[] records = loggersData.duplicate();

        Map<String, String> loggersMap = new HashMap<>();

        for (Record record : records) {
            loggersMap.put(record.getAttribute("logger"), record.getAttribute("level"));
        }

        this.rm.setLogLevelMultiple(LoginModel.getInstance().getSessionId(), loggersMap, new AsyncCallback<String>() {
            @Override
            public void onFailure(Throwable caught) {
                LogModel.getInstance().logImportantMessage("Error updating loggers on the server");
                JSONUtils.getJsonErrorMessage(caught);
            }

            @Override
            public void onSuccess(String result) {
                loggersGrid.redraw();
                fetchLoggersSettings(loggersGrid);
                LogModel.getInstance().logMessage("Successfully updating loggers configuration: " + loggersMap);
            }
        });
    }

    private String getJsonStringNullable(JSONObject jsonObject, String attributeName) {
        return getJsonStringNullable(jsonObject, attributeName, "");
    }

    private boolean getJsonBooleanNullable(JSONObject jsonObject, String attributeName, boolean defaultValue) {
        JSONBoolean result = jsonObject.get(attributeName).isBoolean();

        if (result == null) {
            return defaultValue;
        }

        return result.booleanValue();
    }

    private long getJsonLongNullable(JSONObject jsonObject, String attributeName, long defaultValue) {
        JSONNumber result = jsonObject.get(attributeName).isNumber();

        if (result == null) {
            return defaultValue;
        }

        return Long.parseLong(result.toString());
    }

    private String getJsonStringNullable(JSONObject jsonObject, String attributeName, String defaultValue) {
        JSONString result = jsonObject.get(attributeName).isString();

        if (result == null) {
            return defaultValue;
        }

        return result.stringValue();
    }

    /**
     * Fetch and store NS Infrastructure and Policy creation parameters
     * store it in the model
     *
     * @param success call this when it's done
     * @param failure call this if it fails
     */
    public void fetchSupportedInfrastructuresAndPolicies(final Runnable success, final Runnable failure) {
        rm.getInfrastructures(LoginModel.getInstance().getSessionId(), new AsyncCallback<String>() {

            public void onFailure(Throwable caught) {
                String msg = JSONUtils.getJsonErrorMessage(caught);
                SC.warn("Failed to fetch supported infrastructures:<br>" + msg);
                failure.run();
            }

            public void onSuccess(String result) {
                model.setSupportedInfrastructures(nodeSourceConfigurationParser.parsePluginDescriptors(result));

                rm.getPolicies(LoginModel.getInstance().getSessionId(), new AsyncCallback<String>() {

                    public void onFailure(Throwable caught) {
                        String msg = JSONUtils.getJsonErrorMessage(caught);
                        SC.warn("Failed to fetch supported policies:<br>" + msg);
                        failure.run();
                    }

                    public void onSuccess(String result) {
                        model.setSupportedPolicies(nodeSourceConfigurationParser.parsePluginDescriptors(result));

                        rm.getInfrasToPoliciesMapping(LoginModel.getInstance().getSessionId(),
                                                      new AsyncCallback<String>() {
                                                          @Override
                                                          public void onFailure(Throwable caught) {
                                                              String msg = JSONUtils.getJsonErrorMessage(caught);
                                                              SC.warn("Failed to fetch infra policies mapping:<br>" +
                                                                      msg);
                                                              failure.run();
                                                          }

                                                          @Override
                                                          public void onSuccess(String result) {
                                                              Map<String, List<String>> mapping = nodeSourceConfigurationParser.parseInfraPoliciesMapping(result);
                                                              model.setInfraPolicyMapping(mapping);

                                                              success.run();
                                                          }
                                                      });

                    }
                });
            }
        });
    }

    /**
     * Fetch and store in the model the current configuration of a node source
     *
     * @param success call this when it's done
     * @param failure call this if it fails
     */
    public void fetchNodeSourceConfiguration(String nodeSourceName, Runnable success, Runnable failure) {

        rm.getNodeSourceConfiguration(LoginModel.getInstance().getSessionId(),
                                      nodeSourceName,
                                      new AsyncCallback<String>() {

                                          public void onSuccess(String result) {
                                              try {
                                                  model.setEditedNodeSourceConfiguration(nodeSourceConfigurationParser.parseNodeSourceConfiguration(result));
                                                  success.run();
                                              } catch (Exception e) {
                                                  runFailure(e);
                                              }
                                          }

                                          public void onFailure(Throwable caught) {
                                              runFailure(caught);
                                          }

                                          private void runFailure(Throwable caught) {
                                              String msg = JSONUtils.getJsonErrorMessage(caught);
                                              SC.warn("Failed to fetch configuration of node source " + nodeSourceName +
                                                      ":<br>" + msg);
                                              failure.run();
                                          }
                                      });
    }

    /**
     * Unlock selected node/host/nodesource
     */
    public void unlockNodes() {
        unlockNodes(getSelectedNodesUrls());
    }

    /**
     * lock selected node/host/nodesource
     */
    public void lockNodes() {
        lockNodes(getSelectedNodesUrls());
    }

    public void checkPermissionOfContextMenuItems(ContextMenu contextMenu) {
        LoginModel loginModel = LoginModel.getInstance();

        /*
         * checkStatusOfRemoveItem(contextMenu);
         * if (!loginModel.userHasPermissionToLockNodes()) {
         * contextMenu.disableLockMenuItem(contextMenu);
         * }
         * if (!loginModel.userHasPermissionToUnLockNodes()) {
         * contextMenu.disableUnlockMenuItem(contextMenu);
         * }
         * 
         * 
         * if (!loginModel.userHasPermissionToUndeployNodeSource()) {
         * contextMenu.disableUndeployItem(contextMenu);
         * }
         * if (!loginModel.userHasPermissionToDeployNodeSource()) {
         * contextMenu.disableDeployItem(contextMenu);
         * }
         * checkStatusOfEditButton(contextMenu);
         * if (!loginModel.userHasPermissionToRemoveNode()) {
         * contextMenu.disableRemoveMenuItem(contextMenu);
         * }
         */

        if (!loginModel.userHasPermissionToGetNodeSourceConfiguration()) {
            contextMenu.disableExportInfrastructureItem(contextMenu);
            contextMenu.disableExportPolicyItem(contextMenu);
            contextMenu.disableExportNodeSourceItem(contextMenu);
        }

        /*
         * if (loginModel.userHasPermissionToLockNodes() ||
         * loginModel.userHasPermissionToUnLockNodes() ||
         * loginModel.userHasPermissionToRemoveNode()) {
         * checkNodePermission(contextMenu, true, null);
         * }
         * if ((loginModel.userHasPermissionToUndeployNodeSource() ||
         * loginModel.userHasPermissionToDeployNodeSource()) &&
         * model.getSelectedNodeSource() != null) {
         * checkNodePermission(contextMenu, false, null);
         * }
         */

        checkNodePermission(contextMenu, true, null);
        if (model.getSelectedNodeSource() != null) {
            checkNodePermission(contextMenu, false, null);
        }
    }

    private void checkStatusOfRemoveItem(ContextMenu contextMenu) {
        LoginModel loginModel = LoginModel.getInstance();
        if (contextMenu.getNodesource() == null) {
            if (!loginModel.userHasPermissionToRemoveNode()) {
                contextMenu.disableRemoveMenuItem(contextMenu);
            }
        } else {
            if (!loginModel.userHasPermissionToRemoveNodeSource()) {
                contextMenu.disableRemoveMenuItem(contextMenu);
            }
        }
    }

    /**
     * Checks the method permissions for edit/update button
     * @param contextMenu the current contextMenu
     */
    private void checkStatusOfEditButton(ContextMenu contextMenu) {
        LoginModel loginModel = LoginModel.getInstance();
        NodeSource selectedNodeSource = contextMenu.getNodesource();
        if (selectedNodeSource != null && selectedNodeSource.getNodeSourceStatus() == NodeSourceStatus.NODES_DEPLOYED) {
            if (!loginModel.userHasPermissionToUpdateDynamicParameters()) {
                contextMenu.disableEditItem(contextMenu);
            } else if (loginModel.userDoesNotHavePermissionToGetInfrasToPoliciesMapping() ||
                       loginModel.userDoesNotHavePermissionToGetSupportedNodeSourceInfras()) {
                contextMenu.disableEditItem(contextMenu);
            }
        } else if (selectedNodeSource != null &&
                   selectedNodeSource.getNodeSourceStatus() == NodeSourceStatus.NODES_UNDEPLOYED) {
            if (!loginModel.userHasPermissionToEditNodeSource()) {
                contextMenu.disableEditItem(contextMenu);
            } else if (loginModel.userDoesNotHavePermissionToGetInfrasToPoliciesMapping() ||
                       loginModel.userDoesNotHavePermissionToGetSupportedNodeSourceInfras()) {
                contextMenu.disableEditItem(contextMenu);
            }
        }
    }

    /**
     * Checks if the logged user has admin/provider permissions to the selected nodes
     * If map from LoginModel already contains the permission of the logged user for the selected node/nodeSource, the request will not be sent
     *
     * @param contextMenu the current contextMenu
     * @param provider if true, the request will check if the user has admin and provider permission, if false, it will check just for admin permission
    */
    public void checkNodePermission(ContextMenu contextMenu, boolean provider, String url) {
        LoginModel loginModel = LoginModel.getInstance();
        if (url == null) {
            if (model.getSelectedNodeSource() != null) {
                url = model.getSelectedNodeSource().getSourceName();
            } else if (model.getSelectedHost() != null) {
                url = model.getSelectedHost().getSourceName();
            } else if (model.getSelectedNode() != null) {
                url = model.getSelectedNode().getNodeUrl();
            }
        }

        if (provider && loginModel.userProviderPermissionWasReceivedForNode(url) && contextMenu != null) {
            contextMenu.disableProviderItems(contextMenu, url);
        } else if (!provider && loginModel.userAdminPermissionWasReceivedForNodeSource(url)) {
            if (contextMenu != null) {
                contextMenu.disableAdminItems(contextMenu, url);
            }
            boolean hadAdminPermissionForNodeSource = loginModel.userHasAdminPermissionForNodeSource(url);
            //    rmPage.setThreadDumpTabPageDisabled(!hadAdminPermissionForNodeSource ||
            //                                        !(loginModel.userHasPermissionToGetNodeThreadDump() ||
            //                                        loginModel.userHasPermissionToGetRmThreadDump()));
            rmPage.setScriptConsoleTabPageDisabled(!hadAdminPermissionForNodeSource ||
                                                   !loginModel.userHasPermissionToExecuteScript());
        } else {
            if (model.getSelectedNodeSource() != null || model.getSelectedHost() != null) {
                sendNodeSourcePermissionRequest(contextMenu, url, provider);
            } else if (model.getSelectedNode() != null) {
                sendNodePermissionRequest(contextMenu, url, provider);
            }
        }
    }

    /**
     * Send request in order to get the node permission for the current user
     * After sending the request, the cashed map from LoginModel will add the newly permissions per node received from the backend to avoid sending unnecessary requests
     *
     * @param contextMenu the current contextMenu
     * @param nodeUrl the node url
     * @param provider if true, the request will check if the user has admin and provider permission, if false, it will check just for admin permission
     */
    private void sendNodePermissionRequest(ContextMenu contextMenu, String nodeUrl, boolean provider) {
        LoginModel loginModel = LoginModel.getInstance();
        rm.checkNodePermission(loginModel.getSessionId(), nodeUrl, provider, new AsyncCallback<String>() {
            @Override
            public void onFailure(Throwable caught) {
                LogModel.getInstance().logImportantMessage("Failed to check nodes permission for " + nodeUrl +
                                                           JSONUtils.getJsonErrorMessage(caught));
            }

            @Override
            public void onSuccess(String result) {
                handlePermissionsReceived(result, provider, nodeUrl, contextMenu);

            }
        });
    }

    /**
     * Send request in order to get the node source permission for the current user
     * After sending the request, the cashed map from LoginModel will add the newly permissions per node source received from the backend to avoid sending unnecessary requests
     *
     * @param contextMenu the current contextMenu
     * @param url the node source name
     * @param provider if true, the request will check if the user has admin and provider permission, if false, it will check just for admin permission
     */
    private void sendNodeSourcePermissionRequest(ContextMenu contextMenu, String url, boolean provider) {
        LoginModel loginModel = LoginModel.getInstance();
        rm.checkNodeSourcePermission(loginModel.getSessionId(), url, provider, new AsyncCallback<String>() {
            @Override
            public void onFailure(Throwable caught) {
                LogModel.getInstance().logImportantMessage("Failed to check nodes permission for " + url +
                                                           JSONUtils.getJsonErrorMessage(caught));
            }

            @Override
            public void onSuccess(String result) {
                handlePermissionsReceived(result, provider, url, contextMenu);
            }
        });
    }

    private void handlePermissionsReceived(String result, boolean provider, String url, ContextMenu contextMenu) {
        LoginModel loginModel = LoginModel.getInstance();
        if (provider) {
            loginModel.addRMProviderPermissions(url, Boolean.parseBoolean(result));
            if (contextMenu != null) {
                contextMenu.disableProviderItems(contextMenu, url);
            }
        } else {
            loginModel.addRMAdminPermissions(url, Boolean.parseBoolean(result));
            if (contextMenu != null) {
                contextMenu.disableAdminItems(contextMenu, url);
            }
            boolean hadAdminPermissionForNodeSource = loginModel.userHasAdminPermissionForNodeSource(url);
            //    rmPage.setThreadDumpTabPageDisabled(!hadAdminPermissionForNodeSource ||
            //                                       !(loginModel.userHasPermissionToGetNodeThreadDump() ||
            //                                       loginModel.userHasPermissionToGetRmThreadDump()));
            rmPage.setScriptConsoleTabPageDisabled(!hadAdminPermissionForNodeSource ||
                                                   !loginModel.userHasPermissionToExecuteScript());
        }
    }

    public void checkPortalsPermissions() {
        LoginModel loginModel = LoginModel.getInstance();
        List<String> portals = loginModel.getPortalsPermissionsNames();
        List<String> notCashedPermissions = portals.stream()
                                                   .filter(portal -> !loginModel.sessionPermissionWasReceivedForPortal(portal))
                                                   .collect(Collectors.toList());
        if (notCashedPermissions.isEmpty()) {
            showHidePortalsShortcuts();
        }
        rm.portalsAccess(loginModel.getSessionId(), portals, new AsyncCallback<List<String>>() {
            @Override
            public void onFailure(Throwable caught) {
                LogModel.getInstance().logImportantMessage("Failed to check portals permissions: " +
                                                           JSONUtils.getJsonErrorMessage(caught));
            }

            @Override
            public void onSuccess(List<String> result) {
                LoginModel.addPortalsPermissions(result);
                showHidePortalsShortcuts();
            }
        });
    }

    /**
     * Check if the logged user has permissions to the methods from loginModel.getRmSessionPermissionMethods()
     * If the request has already been send for the logged user, the permissions will be loaded from the cashed list
     */
    public void checkRmMethodsPermissions() {
        LoginModel loginModel = LoginModel.getInstance();
        List<String> methods = loginModel.getRmSessionPermissionMethods();
        List<String> notCashedMethods = methods.stream()
                                               .filter(method -> !loginModel.sessionPermissionWasReceivedForMethod(method))
                                               .collect(Collectors.toList());
        if (notCashedMethods.isEmpty()) {
            setTabsStatus();
            return;
        }

        rm.checkMethodsPermissions(loginModel.getSessionId(), methods, new AsyncCallback<Map<String, Boolean>>() {
            @Override
            public void onFailure(Throwable caught) {
                LogModel.getInstance().logImportantMessage("Failed to check methods permissions: " +
                                                           JSONUtils.getJsonErrorMessage(caught));
            }

            @Override
            public void onSuccess(Map<String, Boolean> result) {
                LoginModel.addSessionPermissions(result);
                setTabsStatus();
            }
        });
    }

    private void setTabsStatus() {
        LoginModel loginModel = LoginModel.getInstance();
        rmPage.setThreadDumpTabPageDisabled(!loginModel.userHasPermissionToGetRmThreadDump());
        rmPage.setScriptConsoleTabPageDisabled(!loginModel.userHasPermissionToExecuteScript());
        if (loginModel.userDoesNotHavePermissionToDefineNodeSource()) {
            rmPage.disableNSButton();
        }
    }

    private void showHidePortalsShortcuts() {
        LoginModel loginModel = LoginModel.getInstance();
        rmPage.rebuildShortcutStrip(loginModel.userHasPermissionToAutomationDashboard(),
                                    loginModel.userHasPermissionToStudio(),
                                    loginModel.userHasPermissionToScheduler(),
                                    loginModel.userHasPermissionToRm());
    }

    private Set<String> getSelectedNodesUrls() {
        Set<String> urls = new HashSet<String>();
        if (model.getSelectedNode() != null) {
            urls.add(model.getSelectedNode().getNodeUrl());
        } else if (model.getSelectedHost() != null) {
            for (Node n : model.getSelectedHost().getNodes().values()) {
                urls.add(n.getNodeUrl());
            }
        } else {
            NodeSource selectedNodeSource = model.getSelectedNodeSource();

            if (selectedNodeSource != null) {
                for (Host h : selectedNodeSource.getHosts().values()) {
                    for (Node n : h.getNodes().values()) {
                        urls.add(n.getNodeUrl());
                    }
                }

                for (Node n : selectedNodeSource.getDeploying().values()) {
                    urls.add(n.getNodeUrl());
                }
            }
        }
        return urls;
    }

    private void lockNodes(final Set<String> nodeUrls) {
        // there's no real incentive to storing locked node states
        // here, let's just try to do what the user says, and report
        // the error if it's nonsense
        rm.lockNodes(LoginModel.getInstance().getSessionId(), nodeUrls, new AsyncCallback<String>() {
            @Override
            public void onFailure(Throwable caught) {
                LogModel.getInstance().logImportantMessage("Failed to lock " + nodeUrls.size() + " nodes: " +
                                                           JSONUtils.getJsonErrorMessage(caught));

            }

            @Override
            public void onSuccess(String result) {
                LogModel.getInstance().logMessage("Successfully locked " + nodeUrls.size() + " nodes");
            }
        });
    }

    private void unlockNodes(final Set<String> nodeUrls) {
        // there's no real incentive to storing locked node states
        // here, let's just try to do what the user says, and report
        // the error if it's nonsense
        rm.unlockNodes(LoginModel.getInstance().getSessionId(), nodeUrls, new AsyncCallback<String>() {
            @Override
            public void onFailure(Throwable caught) {
                LogModel.getInstance().logImportantMessage("Failed to unlock " + nodeUrls.size() + " nodes: " +
                                                           JSONUtils.getJsonErrorMessage(caught));

            }

            @Override
            public void onSuccess(String result) {
                LogModel.getInstance().logMessage("Successfully unlocked " + nodeUrls.size() + " nodes");
            }
        });
    }

    public void deployNodeSource() {
        if (model.getSelectedNodeSource() != null) {
            String nodeSourceName = model.getSelectedNodeSource().getSourceName();

            rm.deployNodeSource(LoginModel.getInstance().getSessionId(),
                                model.getSelectedNodeSource().getSourceName(),
                                new AsyncCallback<String>() {
                                    @Override
                                    public void onFailure(Throwable caught) {
                                        LogModel.getInstance()
                                                .logImportantMessage("Failed to deploy node source " + nodeSourceName +
                                                                     JSONUtils.getJsonErrorMessage(caught));

                                    }

                                    @Override
                                    public void onSuccess(String result) {
                                        LogModel.getInstance()
                                                .logMessage("Successfully deployed node source " + nodeSourceName);
                                    }
                                });
        }
    }

    public void undeployNodeSource() {
        if (model.getSelectedNodeSource() != null) {
            String nodeSourceName = model.getSelectedNodeSource().getSourceName();

            if (model.getSelectedNodeSource() != null) {
                confirmUndeployNodeSource("Confirm undeployment of <strong>" + "NodeSource " + nodeSourceName +
                                          "</strong>", new NodeRemovalCallback() {
                                              public void run(boolean force) {
                                                  rm.undeployNodeSource(LoginModel.getInstance().getSessionId(),
                                                                        model.getSelectedNodeSource().getSourceName(),
                                                                        force,
                                                                        new AsyncCallback<String>() {
                                                                            @Override
                                                                            public void onFailure(Throwable caught) {
                                                                                LogModel.getInstance()
                                                                                        .logImportantMessage("Failed to undeploy node source " +
                                                                                                             nodeSourceName +
                                                                                                             JSONUtils.getJsonErrorMessage(caught));

                                                                            }

                                                                            @Override
                                                                            public void onSuccess(String result) {
                                                                                LogModel.getInstance()
                                                                                        .logMessage("Successfully undeployed node source " +
                                                                                                    nodeSourceName);
                                                                            }
                                                                        });
                                              }
                                          });
            }

        }
    }

    public void editNodeSource(String nodeSourceName, NodeSourceStatus nodeSourceStatus) {
        if (nodeSourceStatus.equals(NodeSourceStatus.NODES_UNDEPLOYED)) {
            this.rmPage.showEditNodeSourceWindow(nodeSourceName);
        } else {
            this.rmPage.showEditDynamicParametersWindow(nodeSourceName);
        }
    }

    public void exportNodeSourceToFile(String nodeSourceName) {
        this.rm.getNodeSourceConfiguration(LoginModel.getInstance().getSessionId(),
                                           nodeSourceName,
                                           new ExportNodeSourceToFileHandler(nodeSourceName).exportFromNodeSourceConfiguration());
    }

    public void exportNodeSourceToCatalog(String nodeSourceName) {
        new ExportToCatalogConfirmWindow(nodeSourceName, CatalogKind.NODE_SOURCE, this).show();
    }

    public void exportInfrastructureToFile(String nodeSourceName) {
        this.rm.getNodeSourceConfiguration(LoginModel.getInstance().getSessionId(),
                                           nodeSourceName,
                                           new ExportInfrastructureToFileHandler(nodeSourceName).exportFromNodeSourceConfiguration());
    }

    public void exportInfrastructureToCatalog(String nodeSourceName) {
        new ExportToCatalogConfirmWindow(nodeSourceName, CatalogKind.INFRASTRUCTURE, this).show();
    }

    public void exportPolicyToFile(String nodeSourceName) {
        this.rm.getNodeSourceConfiguration(LoginModel.getInstance().getSessionId(),
                                           nodeSourceName,
                                           new ExportPolicyToFileHandler(nodeSourceName).exportFromNodeSourceConfiguration());
    }

    public void exportPolicyToCatalog(String nodeSourceName) {
        new ExportToCatalogConfirmWindow(nodeSourceName, CatalogKind.POLICY, this).show();
    }

    /**
     * Remove nodes according to the current selection:
     * if a host is selected, multiple nodes will be removed
     * if a nodesource is selected, multiple hosts will be removed
     */
    public void removeNodes() {
        String _msg;
        int _numNodes = 1;
        if (model.getSelectedNode() != null) {
            _msg = "Node " + model.getSelectedNode().getNodeUrl();
        } else if (model.getSelectedHost() != null) {
            _msg = "1 Node from Host " + model.getSelectedHost().getHostName() + " (ns: " +
                   model.getSelectedHost().getSourceName() + ")";
            _numNodes = model.getSelectedHost().getNodes().size();
        } else if (model.getSelectedNodeSource() != null) {
            _msg = "NodeSource " + model.getSelectedNodeSource().getSourceName();
        } else {
            return;
        }

        final String msg = _msg;
        final int numNodes = _numNodes;

        final AsyncCallback<String> callback = new AsyncCallback<String>() {
            @Override
            public void onFailure(Throwable caught) {
                String err = JSONUtils.getJsonErrorMessage(caught);
                LogModel.getInstance().logImportantMessage("Failed to remove " + msg + ": " + err);
            }

            @Override
            public void onSuccess(String result) {
                if (Boolean.parseBoolean(result)) {
                    LogModel.getInstance().logMessage("Successfully removed " + msg);
                } else {
                    LogModel.getInstance().logMessage(msg + " was not removed");
                }
            }
        };

        if (model.getSelectedNode() != null) {
            confirmRemoveNode("Confirm removal of <strong>" + msg + "</strong>", new NodeRemovalCallback() {
                public void run(boolean force) {
                    rm.removeNode(LoginModel.getInstance().getSessionId(),
                                  model.getSelectedNode().getNodeUrl(),
                                  force,
                                  callback);
                }
            });
        } else if (model.getSelectedHost() != null) {
            final Host h = model.getSelectedHost();
            confirmRemoveNode("Confirm removal of <strong>" + numNodes + " node" + ((numNodes > 1) ? "s" : "") +
                              "</strong> on <strong>host " + h.getHostName() + "</strong>", new NodeRemovalCallback() {
                                  public void run(boolean force) {
                                      for (Node n : h.getNodes().values()) {
                                          rm.removeNode(LoginModel.getInstance().getSessionId(),
                                                        n.getNodeUrl(),
                                                        force,
                                                        callback);
                                      }
                                  }
                              });
        } else if (model.getSelectedNodeSource() != null) {
            confirmRemoveNode("Confirm removal of <strong>" + msg + "</strong>", new NodeRemovalCallback() {
                public void run(boolean force) {
                    rm.removeNodesource(LoginModel.getInstance().getSessionId(),
                                        model.getSelectedNodeSource().getSourceName(),
                                        force,
                                        callback);
                }
            });
        }
    }

    public void setNodeTokens(String nodeUrl, List<String> tokens) {
        rm.setNodeTokens(LoginModel.getInstance().getSessionId(), nodeUrl, tokens, new AsyncCallback<Void>() {
            @Override
            public void onFailure(Throwable caught) {
                LogModel.getInstance().logCriticalMessage(caught.getMessage());
            }

            @Override
            public void onSuccess(Void result) {
            }
        });
    }

    private abstract class NodeRemovalCallback {
        public abstract void run(boolean force);
    }

    private void confirmRemove(String title, String message, final NodeRemovalCallback callback) {
        final Window win = new Window();
        win.setTitle(title);
        win.setShowMinimizeButton(false);
        win.setIsModal(true);
        win.setShowModalMask(true);
        win.setWidth(380);
        win.setHeight(160);
        win.setCanDragResize(false);
        win.setCanDragReposition(false);
        win.centerInPage();

        Label label = new Label(message);
        label.setHeight(40);

        // somehow the name of the checkbox is misleading compared to what it
        // means (see name vs title). Thus we have to negate the value of the
        // checkbox when to preserve meaning
        final CheckboxItem force = new CheckboxItem("force", "Wait task completion on busy nodes");
        force.setValue(true);
        final DynamicForm form = new DynamicForm();
        form.setColWidths(25, "*");
        form.setItems(force);

        Canvas fill = new Canvas();
        fill.setHeight100();

        HLayout buttons = new HLayout();
        buttons.setMembersMargin(5);
        buttons.setAlign(Alignment.RIGHT);
        buttons.setHeight(25);

        IButton ok = new IButton("OK", event -> {
            callback.run(!force.getValueAsBoolean());
            win.hide();
            win.destroy();
        });
        ok.setIcon(Images.instance.ok_16().getSafeUri().asString());
        IButton cancel = new IButton("Cancel", event -> {
            win.hide();
            win.destroy();
        });
        cancel.setIcon(Images.instance.cancel_16().getSafeUri().asString());
        buttons.setMembers(ok, cancel);

        VLayout layout = new VLayout();
        layout.setMembersMargin(5);
        layout.setMargin(5);
        layout.setMembers(label, form, fill, buttons);

        win.addItem(layout);
        win.show();
    }

    private void confirmRemoveNode(String message, final NodeRemovalCallback callback) {
        confirmRemove("Confirm node removal", message, callback);
    }

    private void confirmUndeployNodeSource(String message, final NodeRemovalCallback callback) {
        confirmRemove("Confirm node source undeployment", message, callback);
    }

    public RMServiceAsync getRMService() {
        return this.rm;
    }

    public RMPage getRmPage() {
        return this.rmPage;
    }

    /**
     * Override user settings, rewrite cookies, refresh corresponding ui elements
     *
     * @param refreshTime refresh time for update thread in ms
     */
    public void setUserSettings(String refreshTime) {

        boolean refreshChanged = !refreshTime.equals("" + RMConfig.get().getClass());
        RMConfig.get().set(RMConfig.CLIENT_REFRESH_TIME, refreshTime);
        Settings.get().setSetting(RMConfig.CLIENT_REFRESH_TIME, refreshTime);

        if (refreshChanged) {
            this.stopTimer();
            this.startTimer();
        }
    }

    /**
     * Change the currently selected node
     * notify listeners
     *
     * @param selection currently selected node
     */
    public void selectNode(Node selection) {
        this.model.setSelectedNode(selection.getNodeUrl());
    }

    /**
     * Change the currently selected node
     * notify listeners
     *
     * @param sel currently selected host
     */
    public void selectHost(Host sel) {
        this.model.setSelectedHost(sel.getId());
    }

    /**
     * Change the currently selected ns
     * notify listeners
     *
     * @param sel currently selected ns
     */
    public void selectNodeSource(NodeSource sel) {
        this.model.setSelectedNodeSource(sel.getSourceName());
    }

    /**
     * Issue an error message to the user
     *
     * @param reason error message to display
     */
    private void error(String reason) {
        LogModel.getInstance().logCriticalMessage(reason);
    }

    /**
     * Issue a warning message to the user
     *
     * @param reason warning message to display
     */
    private void warning(String reason) {
        LogModel.getInstance().logImportantMessage(reason);
    }

    /**
     * stop the timer that updates node states periodically
     */
    private void stopTimer() {
        if (this.updater == null)
            return;

        this.updater.cancel();
        this.updater = null;

        this.statsUpdater.cancel();
        this.statsUpdater = null;
    }

    /**
     * Shut down everything, get back to the login page
     *
     * @param message an error message, or null
     */
    private void teardown(String message) {
        this.stopTimer();

        if (this.rmPage == null)
            return;

        this.rmPage.destroy();
        this.rmPage = null;

        this.model = new RMModelImpl();
        this.loginPage = new LoginPage(this, message);
    }

    /**
     * @return a read only view of the clients local data, which stores everything
     * that was received from the server by the controller
     */
    @Override
    public RMModel getModel() {
        return this.model;
    }

    /**
     * @return the Event Dispatcher to use to register new event listeners
     */
    @Override
    public RMEventDispatcher getEventDispatcher() {
        return this.model;
    }

    public void onUncaughtException(Throwable e) {
        e.printStackTrace();
        warning(e.getMessage());
    }

    private String parseAllScriptResults(String jsonString) {
        JSONValue allScriptResultsJson = this.parseJSON(jsonString);
        JSONObject scriptResultJsonObject = allScriptResultsJson.isObject();
        return scriptResultJsonObject == null ? parseAllScriptResultsAsJsonArray(allScriptResultsJson)
                                              : parseScriptResultAsJsonObject(scriptResultJsonObject);
    }

    private String parseAllScriptResultsAsJsonArray(JSONValue allScriptResultsJson) {
        StringBuilder allScriptResultsOutput = new StringBuilder();
        JSONArray scriptResultJsonArray = allScriptResultsJson.isArray();

        JSONValue scriptResultJson;
        JSONObject scriptResultJsonObject;
        for (int i = 0; i < scriptResultJsonArray.size(); i++) {
            scriptResultJson = scriptResultJsonArray.get(i);
            scriptResultJsonObject = scriptResultJson.isObject();
            if (scriptResultJsonObject != null) {
                allScriptResultsOutput.append(parseScriptResultAsJsonObject(scriptResultJsonObject)).append("<br/>");
            }
        }

        return allScriptResultsOutput.toString();
    }

    private String parseScriptResultAsJsonObject(JSONObject scriptResultJsonObject) {
        StringBuilder scriptResultOutput = new StringBuilder();

        parseHostname(scriptResultJsonObject, scriptResultOutput);
        parseExceptionAndCause(scriptResultJsonObject, scriptResultOutput);
        parseOutput(scriptResultJsonObject, scriptResultOutput);

        return scriptResultOutput.toString();
    }

    private void parseHostname(JSONObject scriptResultJsonObject, StringBuilder scriptResultOutput) {
        JSONValue scriptHostnameJson = scriptResultJsonObject.get("hostname");
        if (scriptHostnameJson != null) {
            JSONString scriptHostnameJsonString = scriptHostnameJson.isString();
            if (scriptHostnameJsonString != null) {
                scriptResultOutput.append("<b>On host ")
                                  .append(scriptHostnameJsonString.stringValue())
                                  .append("</b><br/>");
            }
        }
    }

    private void parseExceptionAndCause(JSONObject scriptResultJsonObject, StringBuilder scriptResultOutput) {
        JSONValue exceptionJson = scriptResultJsonObject.get("exception");
        if (exceptionJson != null) {
            JSONObject exceptionJsonObject = exceptionJson.isObject();
            if (exceptionJsonObject != null && exceptionJsonObject.get("message").isString() != null) {
                scriptResultOutput.append("<b>Error: </b>")
                                  .append(exceptionJsonObject.get("message").isString().stringValue())
                                  .append("<br/>");
            }
            while (exceptionJsonObject != null && exceptionJsonObject.get("cause") != null &&
                   exceptionJsonObject.get("cause").isObject() != null) {
                exceptionJsonObject = exceptionJsonObject.get("cause").isObject();
                if (exceptionJsonObject.get("message").isString() != null) {
                    scriptResultOutput.append("<b>Caused by: </b>")
                                      .append(exceptionJsonObject.get("message").isString().stringValue())
                                      .append("<br/>");
                }
            }
        }
    }

    private void parseOutput(JSONObject scriptResultJsonObject, StringBuilder scriptResultOutput) {
        JSONValue outputJson = scriptResultJsonObject.get("output");
        if (outputJson != null) {
            JSONString outputJsonObject = outputJson.isString();
            if (outputJsonObject != null) {
                scriptResultOutput.append("<b>Output: </b><br/>")
                                  .append(outputJsonObject.stringValue())
                                  .append("<br/>");
            }
        }
    }

    public void executeScript(final String script, final String engine, final String nodeUrl,
            final Callback<String, String> syncCallBack) {
        rm.executeNodeScript(LoginModel.getInstance().getSessionId(),
                             script,
                             engine,
                             nodeUrl,
                             new AsyncCallback<String>() {
                                 public void onFailure(Throwable caught) {
                                     reportScriptExecutionFailure(caught, script, nodeUrl, syncCallBack);
                                 }

                                 public void onSuccess(String result) {
                                     syncCallBack.onSuccess(parseAllScriptResults(result));
                                 }
                             });
    }

    public void executeNodeSourceScript(final String script, final String engine, final String nodeSourceName,
            final Callback<String, String> syncCallBack) {
        rm.executeNodeSourceScript(LoginModel.getInstance().getSessionId(),
                                   script,
                                   engine,
                                   nodeSourceName,
                                   new AsyncCallback<String>() {
                                       public void onFailure(Throwable caught) {
                                           reportScriptExecutionFailure(caught, script, nodeSourceName, syncCallBack);
                                       }

                                       public void onSuccess(String result) {
                                           syncCallBack.onSuccess(parseAllScriptResults(result));
                                       }
                                   });
    }

    public void executeHostScript(final String script, final String engine, final String host,
            final Callback<String, String> syncCallBack) {
        rm.executeHostScript(LoginModel.getInstance().getSessionId(),
                             script,
                             engine,
                             host,
                             new AsyncCallback<String>() {
                                 public void onFailure(Throwable caught) {
                                     reportScriptExecutionFailure(caught, script, host, syncCallBack);
                                 }

                                 public void onSuccess(String result) {
                                     syncCallBack.onSuccess(parseAllScriptResults(result));
                                 }
                             });
    }

    private void reportScriptExecutionFailure(Throwable caught, String script, String scriptTarget,
            Callback<String, String> syncCallBack) {
        String msg = JSONUtils.getJsonErrorMessage(caught);
        LogModel.getInstance()
                .logImportantMessage("Failed to execute a script " + script + " on " + scriptTarget + ": " + msg);
        if (msg.contains("HTTP 500 Internal Server Error")) {
            msg = "You are not authorized to execute scripts on this node source. Please contact the administrator.";
        }
        syncCallBack.onFailure(msg);
    }

}
