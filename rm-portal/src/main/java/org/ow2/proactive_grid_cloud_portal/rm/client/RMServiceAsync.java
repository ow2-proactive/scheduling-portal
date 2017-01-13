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

import java.util.List;
import java.util.Map;
import java.util.Set;

import org.ow2.proactive_grid_cloud_portal.common.client.Model;
import org.ow2.proactive_grid_cloud_portal.common.shared.RestServerException;
import org.ow2.proactive_grid_cloud_portal.common.shared.ServiceException;

import com.google.gwt.http.client.Request;
import com.google.gwt.user.client.rpc.AsyncCallback;


/**
 * Async client side stub for server calls
 * 
 * 
 * @author mschnoor
 *
 */
public interface RMServiceAsync {

    /**
     * Default configuration is read by the server
     * @param propertyList async callback to return a list of configuration properties read by 
     *  	the server used to configure the client
     */
    void getProperties(AsyncCallback<Map<String, String>> propertyList);

    /**
     * Logout the current session
     * @param sessionId current session
     * @param callback
     */
    void logout(String sessionId, AsyncCallback<Void> callback);

    /**
     * Limited info about the current RM State : freeNodesNumber, totalAliveNodesNumber, totalNodesNumber
     * @param sessionId current session
     * @param callback async callback to return freeNodesNumber, totalAliveNodesNumber, 
     * 		totalNodesNumber in a JSON string
     */
    void getState(String sessionId, AsyncCallback<String> callback);

    /**
     * Detailed info about the nodes currently held by the RM
     * presented as two arrays of nodes and nodesources referencing each others	
     * @param sessionId current session
     * @param callback async callback to return a JSON object containing two arrays 
     * 		named nodesList and nodeSources, that contain all info about current
     * 		nodes and nodesources in the RM
     */
    void getMonitoring(String sessionId, AsyncCallback<String> callback);

    /**
     * List of all supported Infrastructure Managers, and their parameters
     * @param sessionId current session
     * @param callback async callback to return a JSON array containing all supported infrastructures
     */
    void getInfrastructures(String sessionId, AsyncCallback<String> callback);

    /**
     * List of all supported Policies, and their parameters
     * @param sessionId current session
     * @param callback async callback to return a JSON array containing all supported policies
     */
    void getPolicies(String sessionId, AsyncCallback<String> callback);

    /**
     * Creates a NodeSource 
     * @param sessionId current session
     * @param nodeSourceName name of the new NS
     * @param infrastructureType infrastructure manager full class name
     * @param infrastructureParameters IM String parameters, null value for files
     * @param infrastructureFileParamaters file parameters
     * @param policyType policy full class name
     * @param policyParameters String parameters, null value for files
     * @param policyFileParameters file parameters
     * @param callback
     * @throws ServiceException
     */
    void createNodeSource(String sessionId, String nodeSourceName, String infrastructureType,
            String[] infrastructureParameters, String[] infrastructureFileParameters, String policyType,
            String[] policyParameters, String[] policyFileParameters, AsyncCallback<String> callback);

    /**
     * Lock a set of nodes
     * @param sessionId current session
     * @param nodeUrls urls of nodes to lock
     * @param callback true upon success
     */
    void lockNodes(String sessionId, Set<String> nodeUrls, AsyncCallback<String> callback);

    /**
     * Unlock a set of nodes
     * @param sessionId current session
     * @param nodeUrls urls of nodes to unlock
     * @param callback true upon success
     */
    void unlockNodes(String sessionId, Set<String> nodeUrls, AsyncCallback<String> callback);

    /**
     * Release a node
     * @param sessionId current session
     * @param url unique url of the node
     * @param callback
     */
    void releaseNode(String sessionId, String url, AsyncCallback<String> callback);

    /**
     * Remove a node
     * @param sessionId current session
     * @param url unique url of the node
     * @param force do not wait for task completion
     * @param callback
     */
    void removeNode(String sessionId, String url, boolean force, AsyncCallback<String> callback);

    /**
     * Remove a node source
     * @param sessionId current session
     * @param url unique url of the node
     * @param preempt don't wait for current tasks if true
     * @param callback
     */
    void removeNodesource(String sessionId, String name, boolean preempt, AsyncCallback<String> callback);

    /**
     * returns the version string of the REST api
     */
    void getVersion(AsyncCallback<String> callback);

    /**
     * Query a list of attributes from a specific RM MBean
     * @param sessionId current session
     * @param name name of the JMX management bean
     * @param attrs attributes to fetch in the specified MBean
     * @param callback will return a JSON object with attribute names as key
     */
    void getMBeanInfo(String sessionId, String name, List<String> attrs, AsyncCallback<String> callback);

    /**
     * Retrieves attributes of the specified mbean.
     * 
     * @param sessionId current session
     * @param name of mbean
     * @param nodeJmxUrl mbean server url
     * @param attrs set of mbean attributes
     *
     * @return mbean attributes values
     */
    void getNodeMBeanInfo(String sessionId, String nodeJmxUrl, String objectName, List<String> attrs,
            AsyncCallback<String> callback);

    /**
     * Retrieves attributes of the specified mbeans.
     * 
     * @param sessionId current session
     * @param objectNames mbean names (@see ObjectName format)
     * @param nodeJmxUrl mbean server url
     * @param attrs set of mbean attributes
     * 
     * @return mbean attributes values
     */
    void getNodeMBeansInfo(String sessionId, String nodeJmxUrl, String objectNames, List<String> attrs,
            AsyncCallback<String> callback);

    void getNodeMBeanHistory(String sessionId, String nodeJmxUrl, String objectName, List<String> attrs,
            String timeRange, AsyncCallback callback) throws RestServerException, ServiceException;

    void getNodeMBeansHistory(String sessionId, String nodeJmxUrl, String objectName, List<String> attrs,
            String timeRange, AsyncCallback callback) throws RestServerException, ServiceException;

    /**
     * Statistic history for the following values:<pre>
     * 	{ "BusyNodesCount",
     *    "FreeNodesCount",
     *    "DownNodesCount",
     *    "AvailableNodesCount",
     *    "AverageActivity" }</pre>
     *    
     * @param sessionId current session 
     * @param range a String of 5 chars, one for each stat history source, indicating the time range to fetch
     *      for each source. Each char can be:<ul>
     *            <li>'a' 1 minute
     *            <li>'m' 10 minutes
     *            <li>'h' 1 hour
     *            <li>'H' 8 hours
     *            <li>'d' 1 day
     *            <li>'w' 1 week
     *            <li>'M' 1 month
     *            <li>'y' 1 year</ul>
     * @param callback will contain the server response, a JSON object containing a key for each source
     * @return
     */
    Request getStatHistory(String sessionId, String range, AsyncCallback<String> callback);

    /**
     * Executes a script on a node
     * 
     * @param sessionId current session
     * @param script to execute 
     * @param a script engine to use for the script execution 
     * @param nodeUrl a target for the script execution
     */
    void executeNodeScript(String sessionId, String script, String engine, String nodeUrl,
            AsyncCallback<String> callback);
}
