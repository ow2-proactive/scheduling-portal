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

import java.util.Arrays;

import org.ow2.proactive_grid_cloud_portal.common.client.Model;
import org.ow2.proactive_grid_cloud_portal.common.client.json.JSONUtils;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LogModel;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LoginModel;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMController;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMModel;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMServiceAsync;

import com.google.gwt.json.client.JSONArray;
import com.google.gwt.json.client.JSONNumber;
import com.google.gwt.json.client.JSONObject;
import com.google.gwt.user.client.rpc.AsyncCallback;


/**
 * Chart that retrieves information from several MBeans.
 */
public abstract class MBeansChart extends MBeanChart {

    public MBeansChart(RMController controller, String jmxServerUrl, String mbean, String[] attrs, String title) {
        super(controller, jmxServerUrl, mbean, attrs, title);
    }

    @Override
    public void reload() {
        final RMServiceAsync rm = controller.getRMService();
        final RMModel model = controller.getModel();
        final long t = System.currentTimeMillis();
        final boolean realTime = timeRange.equals(Model.StatHistory.Range.MINUTE_1);

        final LoginModel loginModel = LoginModel.getInstance();

        AsyncCallback<String> callback = new AsyncCallback<String>() {
            public void onSuccess(String result) {
                if (onFinish != null) {
                    onFinish.run();
                }
                if (!loginModel.isLoggedIn())
                    return;

                LogModel.getInstance().logMessage("Fetched " + mbeanName + ":" + Arrays.toString(attrs) + " in " +
                                                  (System.currentTimeMillis() - t) + "ms");

                if (realTime) {
                    processResult(result);
                } else {
                    processHistoryResult(result);
                }
            }

            public void onFailure(Throwable caught) {
                if (onFinish != null) {
                    onFinish.run();
                }
                if (JSONUtils.getJsonErrorCode(caught) == 401) {
                    LogModel.getInstance().logMessage("You have been disconnected from the server.");
                }
            }
        };

        if (realTime) {
            rm.getNodeMBeansInfo(loginModel.getSessionId(), jmxServerUrl, mbeanName, Arrays.asList(attrs), callback);
        } else {
            try {
                rm.getNodeMBeansHistory(loginModel.getSessionId(),
                                        jmxServerUrl,
                                        mbeanName,
                                        Arrays.asList(attrs),
                                        String.valueOf(timeRange.getChar()),
                                        callback);
            } catch (Exception e) {
                LogModel.getInstance().logCriticalMessage(e.getMessage());
            }
        }
    }

    protected int getJsonInternalSize(JSONObject json) {

        // assuming the response structure is the following
        // {mbean1:{theonlyattr:[values]}, mbean2:{theonlyattr:[values]}, ...}

        for (String mbean : json.keySet()) {
            JSONObject attrObject = json.get(mbean).isObject();
            if (attrObject != null) {
                return super.getJsonInternalSize(attrObject);
            }
        }

        return 0;
    }

    protected double[] getJsonSlice(JSONObject json, int i) {

        // assuming the response structure is the following
        // {mbean1:{theonlyattr:[values]}, mbean2:{theonlyattr:[values]}, ...}

        double[] res = new double[json.keySet().size()];

        int counter = 0;
        for (String mbean : json.keySet()) {
            JSONObject attrObject = json.get(mbean).isObject();

            double numValue = 0;
            if (attrObject != null) {
                for (String attr : attrObject.keySet()) {
                    JSONArray values = attrObject.get(attr).isArray();

                    if (values != null) {
                        JSONNumber num = values.get(i).isNumber();
                        if (num != null) {
                            numValue = num.doubleValue();
                        }
                    }
                    break;
                }
            }

            res[counter++] = numValue;
        }
        return res;
    }

}
