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
package org.ow2.proactive_grid_cloud_portal.rm.server.nodesource;

import static org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.NodeSourceWindow.FIELD_SEPARATOR;
import static org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.NodeSourceWindow.FILE;
import static org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.NodeSourceWindow.ROW_SEPARATOR;
import static org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.edition.InlineItemModificationCreator.EDIT_FORM_ITEM_SUFFIX;
import static org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.edition.InlineItemModificationCreator.EDIT_OR_UPLOAD_FORM_ITEM_SUFFIX;

import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.fileupload.FileItem;
import org.apache.commons.io.IOUtils;
import org.codehaus.jettison.json.JSONException;
import org.codehaus.jettison.json.JSONObject;
import org.codehaus.jettison.json.JSONWriter;
import org.ow2.proactive_grid_cloud_portal.common.shared.RestServerException;
import org.ow2.proactive_grid_cloud_portal.rm.server.RMServiceImpl;
import org.ow2.proactive_grid_cloud_portal.rm.server.ServletRequestTransformer;
import org.ow2.proactive_grid_cloud_portal.rm.shared.NodeSourceAction;
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

        Map<String, String> infraParams = new HashMap<>();
        Map<String, String> infraFileParams = new HashMap<>();
        Map<String, String> policyParams = new HashMap<>();
        Map<String, String> policyFileParams = new HashMap<>();

        boolean readingInfraParams = false;
        boolean readingPolicyParams = false;

        boolean deployNodeSource = false;
        NodeSourceAction nodeSourceAction = NodeSourceAction.UNKNOWN;

        Map<String, Integer> infraParamOrder = new HashMap<>();
        Map<String, Integer> infraParamFileOrder = new HashMap<>();
        Map<String, Integer> policyParamOrder = new HashMap<>();
        Map<String, Integer> policyParamFileOrder = new HashMap<>();

        try {
            List<FileItem> formItems = new ServletRequestTransformer().getFormItems(request);

            for (FileItem formItem : formItems) {
                String formFieldName = formItem.getFieldName();
                if (formItem.isFormField()) {
                    String formFieldValue = formItem.getString();
                    if (formFieldName.equals("sessionId")) {
                        sessionId = formFieldValue;
                    } else if (formFieldName.equals("nsCallback")) {
                        callbackName = formFieldValue;
                    } else if (formFieldName.equals("nsName")) {
                        nsName = formFieldValue;
                    } else if (formFieldName.equals("nodesRecoverable")) {
                        nodesRecoverable = formFieldValue;
                    } else if (formFieldName.equals("deploy")) {
                        deployNodeSource = Boolean.parseBoolean(formFieldValue);
                    } else if (formFieldName.equals("nodeSourceAction")) {
                        nodeSourceAction = NodeSourceAction.getEnum(formFieldValue);
                    } else if (formFieldName.equals("infra")) {
                        infra = formFieldValue;
                        readingInfraParams = true;
                    } else if (formFieldName.equals("infraParamOrder")) {
                        populateOrderMap(infraParamOrder, formFieldValue);
                    } else if (formFieldName.equals("infraParamFileOrder")) {
                        populateOrderMap(infraParamFileOrder, formFieldValue);
                    } else if (formFieldName.equals("policy")) {
                        policy = formFieldValue;
                        readingPolicyParams = true;
                        readingInfraParams = false;
                    } else if (formFieldName.equals("policyParamOrder")) {
                        populateOrderMap(policyParamOrder, formFieldValue);
                    } else if (formFieldName.equals("policyParamFileOrder")) {
                        populateOrderMap(policyParamFileOrder, formFieldValue);
                    } else if (formFieldName.equals("hidden-infra") && !formFieldValue.isEmpty()) {
                        extractHiddenItems(infraParams, infraFileParams, formFieldValue);
                    } else if (formFieldName.equals("hidden-policy") & !formFieldValue.isEmpty()) {
                        extractHiddenItems(policyParams, policyFileParams, formFieldValue);
                    } else if (readingInfraParams) {
                        addToStringParamsOrToFileParams(infraParams, infraFileParams, formFieldName, formFieldValue);
                    } else if (readingPolicyParams) {
                        addToStringParamsOrToFileParams(policyParams, policyFileParams, formFieldName, formFieldValue);
                    } else {
                        LOGGER.warn("Unexpected parameter " + formFieldName);
                    }
                } else {
                    if (readingInfraParams) {
                        byte[] bytes = IOUtils.toByteArray(formItem.getInputStream());
                        infraFileParams.put(formFieldName, new String(bytes));
                    } else if (readingPolicyParams) {
                        byte[] bytes = IOUtils.toByteArray(formItem.getInputStream());
                        policyFileParams.put(formFieldName, new String(bytes));
                    } else {
                        LOGGER.warn("Unexpected parameter " + formFieldName);
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
                LOGGER.error("Cannot apply node source action: " + failFast);
                LOGGER.error("Request parameters: ");
                formItems.forEach(fileItem -> LOGGER.error(fileItem.getFieldName() + "=" + fileItem.getString()));
                throw new RestServerException(failFast);
            }

            String[] infraFileParamsArray = orderValues(removeSuffixEditFromKeys(infraFileParams), infraParamFileOrder);
            String[] infraParamsArray = orderValues(infraParams, infraParamOrder);
            String[] policyFileParamsArray = orderValues(removeSuffixEditFromKeys(policyFileParams),
                                                         policyParamFileOrder);
            String[] policyParamsArray = orderValues(policyParams, policyParamOrder);

            String jsonResponsePayload;
            switch (nodeSourceAction) {
                case EDIT:
                    jsonResponsePayload = ((RMServiceImpl) RMServiceImpl.get()).editNodeSource(sessionId,
                                                                                               nsName,
                                                                                               infra,
                                                                                               infraParamsArray,
                                                                                               infraFileParamsArray,
                                                                                               policy,
                                                                                               policyParamsArray,
                                                                                               policyFileParamsArray,
                                                                                               nodesRecoverable);
                    break;
                case CREATE:
                    jsonResponsePayload = ((RMServiceImpl) RMServiceImpl.get()).defineNodeSource(sessionId,
                                                                                                 nsName,
                                                                                                 infra,
                                                                                                 infraParamsArray,
                                                                                                 infraFileParamsArray,
                                                                                                 policy,
                                                                                                 policyParamsArray,
                                                                                                 policyFileParamsArray,
                                                                                                 nodesRecoverable);
                    break;
                case UPDATE:
                    jsonResponsePayload = ((RMServiceImpl) RMServiceImpl.get()).updateDynamicParameters(sessionId,
                                                                                                        nsName,
                                                                                                        infra,
                                                                                                        infraParamsArray,
                                                                                                        infraFileParamsArray,
                                                                                                        policy,
                                                                                                        policyParamsArray,
                                                                                                        policyFileParamsArray);
                    break;
                default:
                    jsonResponsePayload = new JSONWriter(new StringWriter()).object()
                                                                            .key("result")
                                                                            .value(false)
                                                                            .key("errorMessage")
                                                                            .value("Unknown node source action")
                                                                            .endObject()
                                                                            .toString();
                    break;
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

    private void extractHiddenItems(Map<String, String> infraParams, Map<String, String> infraFileParams,
            String formFieldValue) {
        for (String row : formFieldValue.split(ROW_SEPARATOR)) {
            String[] fields = row.split(FIELD_SEPARATOR);
            if (fields.length != 2 && fields.length != 3) {
                LOGGER.warn("Failed extract hidden value from: " + row);
                continue;
            }
            boolean isFile = fields[0].equals(FILE);
            String name = fields[1];
            String value = fields.length > 2 ? fields[2] : "";
            if (isFile) {
                infraFileParams.put(name, value);
            } else {
                infraParams.put(name, value);
            }
        }
    }

    private Map<String, String> removeSuffixEditFromKeys(Map<String, String> map) {
        Map<String, String> result = new HashMap<>(map.size());

        for (Map.Entry<String, String> entry : map.entrySet()) {
            if (entry.getKey().endsWith(EDIT_FORM_ITEM_SUFFIX)) {
                result.put(entry.getKey().substring(0, entry.getKey().length() - EDIT_FORM_ITEM_SUFFIX.length()),
                           entry.getValue());
            } else {
                result.put(entry.getKey(), entry.getValue());
            }
        }

        return result;
    }

    private void populateOrderMap(Map<String, Integer> orderMap, String value) {
        String[] split = value.split(";");
        if (split.length > 1) {
            for (int i = 0; i < split.length; ++i) {
                orderMap.put(split[i], i);
            }
        } else if (value != null && !value.trim().isEmpty()) {
            orderMap.put(value, 0);
        }
    }

    private String[] orderValues(Map<String, String> keyValue, Map<String, Integer> keyIndex) {
        return keyIndex.entrySet()
                       .stream()
                       .sorted(Comparator.comparing(Map.Entry::getValue))
                       .filter(e -> !e.getKey().contains("staticTextItem"))
                       .map(e -> keyValue.get(e.getKey()))
                       .toArray(String[]::new);
    }

    private void addToStringParamsOrToFileParams(Map<String, String> params, Map<String, String> fileParams,
            String formFieldName, String formFieldValue) {
        if (formFieldName.endsWith(EDIT_FORM_ITEM_SUFFIX)) {
            fileParams.put(formFieldName, formFieldValue);
        } else if (!formFieldName.endsWith(EDIT_OR_UPLOAD_FORM_ITEM_SUFFIX)) {
            params.put(formFieldName, formFieldValue);
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

        //writing the callback name in as an inlined script, so that the
        // browser, upon receiving it, will evaluate the JS and call the
        // function
        return "<script type='text/javascript'>window.opener.focus(); window.opener." + callbackName + "(" + json +
               "); window.close();</script>";
    }

    private String[] toArray(ArrayList<String> list) {
        return list.toArray(new String[list.size()]);
    }

}
