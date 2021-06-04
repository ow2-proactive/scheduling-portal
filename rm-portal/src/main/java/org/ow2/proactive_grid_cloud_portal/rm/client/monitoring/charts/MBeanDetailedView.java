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
package org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.charts;

import java.util.List;

import org.ow2.proactive_grid_cloud_portal.common.client.json.JSONUtils;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LogModel;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LoginModel;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMController;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMModel;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMServiceAsync;

import com.google.gwt.json.client.JSONArray;
import com.google.gwt.json.client.JSONObject;
import com.google.gwt.json.client.JSONValue;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.smartgwt.client.widgets.viewer.DetailViewer;
import com.smartgwt.client.widgets.viewer.DetailViewerField;
import com.smartgwt.client.widgets.viewer.DetailViewerRecord;


/**
 * Detailed view that retrieves information from MBean.
 */
public class MBeanDetailedView extends DetailViewer {

    private AsyncCallback<String> extraCallback;

    public MBeanDetailedView() {
    }

    public MBeanDetailedView(AsyncCallback<String> extraCallback) {
        this.extraCallback = extraCallback;
    }

    public MBeanDetailedView(RMController controller, String jmxServerUrl, String mbean, List<String> attrs) {
        load(controller, jmxServerUrl, mbean, attrs);
    }

    public void load(final RMController controller, String jmxServerUrl, String mbean, List<String> attrs) {
        DetailViewerField[] fields = new DetailViewerField[attrs.size()];

        for (int i = 0; i < fields.length; i++) {
            fields[i] = new DetailViewerField(attrs.get(i));
        }

        setFields(fields);

        final RMServiceAsync rm = controller.getRMService();
        final long t = System.currentTimeMillis();

        final LoginModel loginModel = LoginModel.getInstance();

        // loading runtime info
        rm.getNodeMBeanInfo(loginModel.getSessionId(), jmxServerUrl, mbean, attrs, new AsyncCallback<String>() {
            public void onSuccess(String result) {

                if (extraCallback != null) {
                    extraCallback.onSuccess(result);
                }

                if (!loginModel.isLoggedIn())
                    return;

                LogModel.getInstance()
                        .logMessage("Fetched JVM Runtime info in " + (System.currentTimeMillis() - t) + "ms");
                try {
                    JSONArray array = controller.parseJSON(result).isArray();
                    if (array != null) {
                        DetailViewerRecord dv = new DetailViewerRecord();
                        for (int i = 0; i < array.size(); i++) {
                            try {
                                JSONObject property = array.get(i).isObject();
                                String name = property.get("name").isString().stringValue();
                                JSONValue value = property.get("value");
                                String valueStr = "";

                                if (value.isString() != null) {
                                    valueStr = value.isString().stringValue();
                                } else if (value.isNumber() != null) {
                                    valueStr = value.isNumber().toString();
                                }

                                dv.setAttribute(name, valueStr);
                            } catch (Exception e) {
                                // ignore it
                            }
                        }
                        setData(new DetailViewerRecord[] { dv });
                    }
                } catch (Exception e) {
                    LogModel.getInstance().logMessage("Error when processing " + this.getClass().getName() +
                                                      " result : " + e.getMessage());
                }

            }

            public void onFailure(Throwable caught) {
                if (extraCallback != null) {
                    extraCallback.onFailure(caught);
                }

                if (JSONUtils.getJsonErrorCode(caught) == 401) {
                    LogModel.getInstance().logMessage("You have been disconnected from the server.");
                }
            }
        });
    }
}
