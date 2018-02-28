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
package org.ow2.proactive_grid_cloud_portal.scheduler.server;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Enumeration;
import java.util.HashMap;

import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.ow2.proactive_grid_cloud_portal.common.server.Service;
import org.ow2.proactive_grid_cloud_portal.common.shared.RestServerException;
import org.ow2.proactive_grid_cloud_portal.common.shared.ServiceException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;


/**
 * Custom job submission servlet
 * <p>
 * This servlet is used to submit a job and edit some of the variables definitions
 * the parameters are:
 * . 'sessionId' : used to submit the job
 * . 'job' : contains the XML job descriptor as a string
 * . 'var_<variable>' : where <variable> is the name of a variable
 * definition in the XML descriptor. This parameter can be used for each
 * variable
 * <p>
 * If you do not wish to edit variables, simply use {@link UploadServlet}
 *
 * @author mschnoor
 */
@SuppressWarnings("serial")
public class SubmitEditServlet extends HttpServlet {

    private static final Logger LOGGER = LoggerFactory.getLogger(SubmitEditServlet.class);

    public static final String ERROR = "SubmitEditServletError:";

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) {
        upload(request, response);
    }

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) {
        upload(request, response);
    }

    private void upload(HttpServletRequest request, HttpServletResponse response) {
        response.setContentType("text/html");

        String sessionId = null;
        String job = null;
        String startAt = null;
        boolean validate = false;
        boolean plan = false;
        HashMap<String, String> varMap = new HashMap<String, String>();
        File editedJob = null;
        File jobDesc = null;

        @SuppressWarnings("rawtypes")
        Enumeration e = request.getParameterNames();
        while (e.hasMoreElements()) {
            Object o = e.nextElement();
            String key = o.toString();
            String val = request.getParameter(key);

            if (key.equals("job")) {
                job = val;
            } else if (key.equals("sessionId")) {
                sessionId = val;
            } else if (key.startsWith("var_")) {
                String name = key.substring(4);
                varMap.put(name, val);
            } else if (key.equalsIgnoreCase("START_AT")) {
                startAt = val;
            } else if (key.equalsIgnoreCase("PLAN")) {
                plan = Boolean.parseBoolean(val);
            } else if (key.equals("validate")) {
                validate = Boolean.parseBoolean(val);
            }
        }

        LOGGER.info("sessionId=" + sessionId);
        LOGGER.info("startAt=" + startAt);
        LOGGER.info("varMap=" + varMap);
        LOGGER.info("job=" + job);

        try {
            if (job == null) {
                response.getWriter().write(ERROR + "Parameter 'job' is null");
                return;
            }
            if (sessionId == null) {
                response.getWriter().write(ERROR + "Parameter 'sessionId' is null");
                return;
            }

            try {
                jobDesc = File.createTempFile("portal_job_edit", "xml");
                jobDesc.deleteOnExit();
                IOUtils.write(job, new FileOutputStream(jobDesc));

                DocumentBuilderFactory docFactory = DocumentBuilderFactory.newInstance();
                DocumentBuilder docBuilder = docFactory.newDocumentBuilder();
                Document doc = docBuilder.parse(jobDesc);

                NodeList variablesNodeList = doc.getElementsByTagName("variable");

                /* edit the job variables using XML DOM */
                if (variablesNodeList != null) {
                    for (int i = 0; i < variablesNodeList.getLength(); i++) {
                        Node variableNode = variablesNodeList.item(i);
                        if (variableNode != null && !isTaskVariableElement(variableNode)) {
                            if (variableNode.getAttributes() != null) {

                                String name = null;
                                Node nodeVal = null;
                                for (int j = 0; j < variableNode.getAttributes().getLength(); j++) {
                                    Node attr = variableNode.getAttributes().item(j);

                                    if (attr.getNodeName().equals("name")) {
                                        name = attr.getNodeValue();
                                    }
                                    if (attr.getNodeName().equals("value")) {
                                        nodeVal = attr;
                                    }
                                }
                                String match = varMap.get(name);

                                if (match != null && nodeVal != null) {
                                    nodeVal.setNodeValue(match);
                                }
                            }
                        }
                    }
                }

                if (startAt != null && !startAt.isEmpty()) {
                    NodeList gis = doc.getElementsByTagName("genericInformation");
                    Node gi = null;

                    if (gis.getLength() > 0) {
                        // get the existing GI
                        gi = gis.item(0);
                    } else {
                        // create the new GI element to insert
                        gi = doc.createElement("genericInformation");
                    }

                    Element startAtEl = doc.createElement("info");
                    startAtEl.setAttribute("name", "START_AT");
                    startAtEl.setAttribute("value", startAt);
                    gi.appendChild(startAtEl);

                    // it will be inserted right before taskFlow
                    Node nextNode = doc.getElementsByTagName("taskFlow").item(0);
                    // consequence of not having a insertAfter() method
                    nextNode.getParentNode().insertBefore(gi, nextNode);

                }

                // write the document to a string
                try {
                    Transformer transformer = TransformerFactory.newInstance().newTransformer();
                    transformer.setOutputProperty(OutputKeys.INDENT, "yes");
                    editedJob = File.createTempFile("portal_edit_res", "xml");
                    editedJob.deleteOnExit();
                    StreamResult result = new StreamResult(editedJob);
                    DOMSource source = new DOMSource(doc);
                    transformer.transform(source, result);
                } catch (Exception e1) {
                    response.getWriter()
                            .write(ERROR + "Error while writing the job descriptor's DOM: " + e1.getMessage());
                }

            } catch (ParserConfigurationException e1) {
                response.getWriter().write(ERROR + "Error initializing DOM parser " + e1.getMessage());
            } catch (SAXException e1) {
                response.getWriter().write(ERROR + "Error parsing job descriptor: " + e1.getMessage());
            }

            // submission at last....
            try {
                LOGGER.info("editedJob that will be sent:");
                LOGGER.info(FileUtils.readFileToString(editedJob));

                String responseFromService = null;

                if (validate) {
                    responseFromService = ((SchedulerServiceImpl) Service.get()).validateXMLFile(sessionId, editedJob);
                } else if (plan) {
                    responseFromService = ((SchedulerServiceImpl) Service.get()).planXMLFile(sessionId, editedJob);
                } else {
                    responseFromService = ((SchedulerServiceImpl) Service.get()).submitXMLFile(sessionId, editedJob);
                }
                if (responseFromService == null || responseFromService.length() == 0) {
                    response.getWriter().write(ERROR + "Job submission returned without a value!");
                } else {
                    response.getWriter().write(responseFromService);
                }
            } catch (RestServerException e1) {
                String msg = e1.getMessage().replace("<", "&lt;").replace(">", "&gt;");
                response.getWriter().print(ERROR + msg);
            } catch (ServiceException e2) {
                String msg = e2.getMessage().replace("<", "&lt;").replace(">", "&gt;");
                response.getWriter().print(ERROR + msg);
            }

        } catch (IOException e1) {
            LOGGER.warn("Failed to write back to client", e1);
        } finally {
            if (jobDesc != null)
                jobDesc.delete();
            if (editedJob != null)
                editedJob.delete();
        }
    }

    private boolean isTaskVariableElement(Node node) {
        if (node.getParentNode() != null && node.getParentNode().getParentNode() != null) {
            Node grandparentNode = node.getParentNode().getParentNode();
            if (grandparentNode.getNodeName().equals("task")) {
                return true;
            }
        }
        return false;
    }

}
