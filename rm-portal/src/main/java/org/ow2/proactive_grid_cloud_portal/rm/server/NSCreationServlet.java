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
package org.ow2.proactive_grid_cloud_portal.rm.server;

import java.io.File;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.fileupload.FileItem;
import org.apache.commons.fileupload.disk.DiskFileItemFactory;
import org.apache.commons.fileupload.servlet.ServletFileUpload;
import org.apache.commons.io.IOUtils;
import org.codehaus.jettison.json.JSONException;
import org.codehaus.jettison.json.JSONObject;
import org.ow2.proactive_grid_cloud_portal.common.shared.RestServerException;
import org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.EditNodeSourceWindow;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


/**
 * NS Creation requires reading one or multiple files from the client,
 * which cannot be done client-side
 *
 * @author ActiveEon Team
 */
@SuppressWarnings("serial")
public class NSCreationServlet extends HttpServlet {

    private static final Logger LOGGER = LoggerFactory.getLogger(NSCreationServlet.class);

    public static final int MAX_UPLOAD_SIZE = 1048576; // in Bytes

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) {
        createNs(request, response);
    }

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) {
        createNs(request, response);
    }

    private void createNs(HttpServletRequest request, HttpServletResponse response) {

        String sessionId = "";
        String callbackName = "";
        String nsName = "";
        String nodesRecoverable = "";
        String infra = "";
        String policy = "";

        ArrayList<String> infraParams = new ArrayList<>();
        ArrayList<String> infraFileParams = new ArrayList<>();
        ArrayList<String> policyParams = new ArrayList<>();
        ArrayList<String> policyFileParams = new ArrayList<>();

        boolean readingInfraParams = false;
        boolean readingPolicyParams = false;

        boolean deployNodeSource = false;
        boolean nodeSourceEdited = false;

        try {
            DiskFileItemFactory factory = new DiskFileItemFactory();
            factory.setSizeThreshold(4096);
            factory.setRepository(new File(System.getProperty("java.io.tmpdir")));
            ServletFileUpload upload = new ServletFileUpload(factory);
            upload.setSizeMax(MAX_UPLOAD_SIZE);

            List<?> fileItems = upload.parseRequest(request);
            Iterator<?> i = fileItems.iterator();
            while (i.hasNext()) {
                FileItem formField = (FileItem) i.next();
                String formFieldName = formField.getFieldName();
                if (formField.isFormField()) {
                    String formFieldValue = formField.getString();
                    if (formFieldName.equals("sessionId")) {
                        sessionId = formFieldValue;
                    } else if (formFieldName.equals("nsCallback")) {
                        callbackName = formFieldValue;
                    } else if (formFieldName.equals("nsName")) {
                        nsName = formFieldValue;
                    } else if (formFieldName.equals("nodesRecoverable")) {
                        nodesRecoverable = formFieldValue;
                    } else if (formFieldName.equals("deploy")) {
                        if (formFieldValue.equals(Boolean.TRUE.toString())) {
                            deployNodeSource = true;
                        }
                    } else if (formFieldName.equals("nodeSourceEdited")) {
                        nodeSourceEdited = Boolean.valueOf(formFieldValue);
                    } else if (formFieldName.equals("infra")) {
                        infra = formFieldValue;
                        readingInfraParams = true;
                    } else if (formFieldName.equals("policy")) {
                        policy = formFieldValue;
                        readingPolicyParams = true;
                        readingInfraParams = false;
                    } else if (readingInfraParams) {
                        if (formFieldName.endsWith(EditNodeSourceWindow.EDIT_FORM_ITEM_SUFFIX)) {
                            infraFileParams.add(formFieldValue);
                        } else if (!formFieldName.endsWith(EditNodeSourceWindow.EDIT_OR_UPLOAD_FORM_ITEM_SUFFIX)) {
                            infraParams.add(formFieldValue);
                        }
                    } else if (readingPolicyParams) {
                        policyParams.add(formFieldValue);
                    } else {
                        LOGGER.warn("Unexpected param " + formFieldName);
                    }
                } else {
                    if (readingInfraParams) {
                        byte[] bytes = IOUtils.toByteArray(formField.getInputStream());
                        infraFileParams.add(new String(bytes));
                    } else if (readingPolicyParams) {
                        byte[] bytes = IOUtils.toByteArray(formField.getInputStream());
                        policyFileParams.add(new String(bytes));
                    } else {
                        LOGGER.warn("Unexpected param " + formFieldName);
                    }
                }
            }
            String failFast = null;
            if (nsName.length() == 0) {
                failFast = "You need to pick a name for the new Node Source";
            } else if (policy.length() == 0 || policy.equals("undefined")) {
                failFast = "No Policy selected";
            } else if (infra.length() == 0 || infra.equals("undefined")) {
                failFast = "No Infrastructure selected";
            }

            if (failFast != null) {
                throw new RestServerException(failFast);
            }

            String jsonResponsePayload;
            if (nodeSourceEdited) {
                jsonResponsePayload = ((RMServiceImpl) RMServiceImpl.get()).editNodeSource(sessionId,
                                                                                           nsName,
                                                                                           infra,
                                                                                           toArray(infraParams),
                                                                                           toArray(infraFileParams),
                                                                                           policy,
                                                                                           toArray(policyParams),
                                                                                           toArray(policyFileParams),
                                                                                           nodesRecoverable);
            } else {
                jsonResponsePayload = ((RMServiceImpl) RMServiceImpl.get()).defineNodeSource(sessionId,
                                                                                             nsName,
                                                                                             infra,
                                                                                             toArray(infraParams),
                                                                                             toArray(infraFileParams),
                                                                                             policy,
                                                                                             toArray(policyParams),
                                                                                             toArray(policyFileParams),
                                                                                             nodesRecoverable);
            }

            JSONObject jsonObject = new JSONObject(jsonResponsePayload);
            if (hasResult(jsonObject) && isResultSuccessful(jsonObject)) {
                jsonResponsePayload = createJsonPair(jsonObject);
            } else {
                deployNodeSource = false;
            }

            if (deployNodeSource) {
                jsonResponsePayload = ((RMServiceImpl) RMServiceImpl.get()).deployNodeSource(sessionId, nsName);

                jsonObject = new JSONObject(jsonResponsePayload);
                if (hasResult(jsonObject)) {
                    jsonResponsePayload = createJsonPair(jsonObject);
                }
            }

            write(response, createJavascriptPayload(callbackName, jsonResponsePayload));
        } catch (Throwable t) {
            write(response,
                  createJavascriptPayload(callbackName, createEscapedSimpleJsonPair("errorMessage", t.getMessage())));
        }
    }

    private String createJsonPair(JSONObject json) throws JSONException {
        String jsonResult;
        if (isResultSuccessful(json)) {
            jsonResult = createNonEscapedSimpleJsonPair("result", "true");
        } else {
            String errorMessage = json.get("errorMessage").toString();
            jsonResult = createEscapedSimpleJsonPair("errorMessage", errorMessage);
        }
        return jsonResult;
    }

    private boolean hasResult(JSONObject jsonObject) {
        return jsonObject != null && jsonObject.has("result");
    }

    private boolean isResultSuccessful(JSONObject jsonObject) throws JSONException {
        return jsonObject.getBoolean("result");
    }

    private void write(HttpServletResponse response, String s) {
        try {
            response.getWriter().write(s);
        } catch (Throwable t) {
            LOGGER.warn("Failed to write script back to client", t);
        }
    }

    private String createEscapedSimpleJsonPair(String key, String value) {
        return createSimpleJsonPair(key, value, true);
    }

    private String createNonEscapedSimpleJsonPair(String key, String value) {
        return createSimpleJsonPair(key, value, false);
    }

    private String createSimpleJsonPair(String key, String value, boolean escapeValue) {
        if (escapeValue) {
            value = "\"" + value + "\"";
        }

        return "{ \"" + key + "\" : " + value + " }";
    }

    private String createJavascriptPayload(String callbackName, String json) {
        return "<script type='text/javascript'>window.opener.focus(); window.opener." + callbackName + "(" + json +
               "); window.close();</script>";
    }

    private String[] toArray(ArrayList<String> list) {
        return list.toArray(new String[list.size()]);
    }

}
