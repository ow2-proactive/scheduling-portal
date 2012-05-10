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
package org.ow2.proactive_grid_cloud_portal.rm.client;

import java.util.List;
import java.util.Map;
import java.util.Set;

import org.ow2.proactive_grid_cloud_portal.common.shared.RestServerException;
import org.ow2.proactive_grid_cloud_portal.common.shared.ServiceException;

import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;


/**
 * Client side stub for server calls 
 * 
 * 
 * 
 * @author mschnoor
 *
 */
@RemoteServiceRelativePath("rm")
public interface RMService extends RemoteService {

    /**
     * Default configuration is read by the server
     * @return a list of configuration properties read by 
     *  	the server used to configure the client
     */
    Map<String, String> getProperties();

    /**
     * Logout the current session
     * @param sessionId id of the current session
     * @throws ServiceException
     */
    void logout(String sessionId) throws ServiceException;

    /**
     * Limited info about the current RM State : freeNodesNumber, totalAliveNodesNumber, totalNodesNumber
     * @param sessionId the current session
     * @return freeNodesNumber, totalAliveNodesNumber, totalNodesNumber in a JSON string
     * @throws RestServerException 
     * @throws ServiceException
     */
    String getState(String sessionId) throws RestServerException, ServiceException;

    /**
     * Detailed info about the nodes currently held by the RM
     * presented as two arrays of nodes and nodesources referencing each others
     * @param sessionId current session
     * @return a JSON object containing two arrays named nodesList and nodeSources, that contain all info about current
     * 		nodes and nodesources in the RM
     * @throws RestServerException 
     * @throws ServiceException
     */
    String getMonitoring(String sessionId) throws RestServerException, ServiceException;

    /**
     * List of all supported Infrastructure Managers, and their parameters
     * @param sessionId current session
     * @return a JSON array containing all supported infrastructures
     * @throws ServiceException
     */
    String getInfrastructures(String sessionId) throws RestServerException, ServiceException;

    /**
     * List of all supported Policies, and their parameters
     * @param sessionId current session
     * @return a JSON array containing all supported policies
     * @throws RestServerException 
     * @throws ServiceException
     */
    String getPolicies(String sessionId) throws RestServerException, ServiceException;

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
     * @throws RestServerException 
     * @throws ServiceException
     */
    String createNodeSource(String sessionId, String nodeSourceName, String infrastructureType,
            String[] infrastructureParameters, String[] infrastructureFileParameters, String policyType,
            String[] policyParameters, String[] policyFileParameters) throws RestServerException,
            ServiceException;

    /**
     * lock a set of nodes
     * @param sessionId current session
     * @param nodeUrls nodes to lock
     * @return true upon success
     * @throws RestServerException 
     * @throws ServiceException
     */
    String lockNodes(String sessionId, Set<String> nodeUrls) throws RestServerException, ServiceException;

    /**
     * Unlock a set of nodes
     * @param sessionId current session
     * @param nodeUrls nodes to unlock
     * @return true upon success
     * @throws RestServerException 
     * @throws ServiceException
     */
    String unlockNodes(String sessionId, Set<String> nodeUrls) throws RestServerException, ServiceException;

    /**
     * Release a node
     * @param sessionId currend session
     * @param url complete unique url of the node
     * @return true when ok
     * @throws RestServerException 
     * @throws ServiceException
     */
    String releaseNode(String sessionId, String url) throws RestServerException, ServiceException;

    /**
     * Remove a node
     * @param sessionId currend session
     * @param url complete unique url of the node
     * @param force do not wait for task completion
     * @return true when ok
     * @throws RestServerException 
     * @throws ServiceException
     */
    String removeNode(String sessionId, String url, boolean force) throws RestServerException,
            ServiceException;

    /**
     * Remove a node
     * @param sessionId currend session
     * @param name complete unique name of the nodesource
     * @param preempt don't wait for tasks if true
     * @return true when ok
     * @throws RestServerException 
     * @throws ServiceException
     */
    String removeNodesource(String sessionId, String name, boolean preempt) throws RestServerException,
            ServiceException;

    /**
     * @return version string of the REST api
     */
    String getVersion() throws RestServerException, ServiceException;

    /**
     * Query a list of attributes from a specific RM MBean
     * @param sessionId current session
     * @param name of the JMX management bean
     * @param attrs attributes to fetch in the specified MBean
     * @return JSON object with attribute names as key
     * @throws RestServerException
     * @throws ServiceException
     */
    String getMBeanInfo(String sessionId, String name, List<String> attrs) throws RestServerException,
            ServiceException;

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
    String getNodeMBeanInfo(String sessionId, String nodeJmxUrl, String objectName, List<String> attrs)
            throws RestServerException, ServiceException;

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
    String getNodeMBeansInfo(String sessionId, String nodeJmxUrl, String objectNames, List<String> attrs)
            throws RestServerException, ServiceException;

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
     * @return will contain the server response, a JSON object containing a key for each source
     */
    String getStatHistory(String sessionId, String range) throws RestServerException, ServiceException;
}
