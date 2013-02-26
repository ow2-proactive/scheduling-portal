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
package org.ow2.proactive_grid_cloud_portal.common.client;

import java.util.Date;

import com.google.gwt.core.client.JavaScriptObject;
import com.google.gwt.dom.client.Document;
import com.google.gwt.dom.client.Element;
import com.google.gwt.dom.client.ScriptElement;


/**
 * Native JS Utils for stuff that is not already wrapped by [Smart]GWT.
 * 
 * 
 * @author mschnoor
 *
 */
public class JSUtil {

    public interface JSCallback {
        public void execute(JavaScriptObject obj);
    }

    private static int requestCounter = 0;

    /**
    * Register a native JS callback to the document
    * for asynchronous callback in SmartGWT forms that do not support it
    * 
    * Simply register the function, keep the name and recall it later
    * 	
    * @param callback
    * @return name of the callback
    */
    public static String register(JSCallback callback) {
        String callbackName = "callback" + (requestCounter++);
        createCallbackFunction(callback, callbackName);
        return callbackName;
    }

    private native static void createCallbackFunction(JSCallback obj, String callbackName)/*-{
    				tmpcallback = function(j) {
    					obj.@org.ow2.proactive_grid_cloud_portal.common.client.JSUtil.JSCallback::execute(Lcom/google/gwt/core/client/JavaScriptObject;)( j );
    				};
    				$wnd[callbackName] = tmpcallback;
    			}-*/;

    /**
     * load a new script in the document
     * 
     * @param path relative path to the JS
     */
    public static void addScript(String path) {
        Element head = Document.get().getElementsByTagName("head").getItem(0);
        ScriptElement script = Document.get().createScriptElement();
        script.setType("text/javascript");
        script.setLang("javascript");
        script.setSrc(path);
        head.appendChild(script);
    }

    /**
     * @return available screen width in pixels
     */
    public static int getScreenWidth() {
        return Integer.parseInt(getAvailWidth());
    }

    /**
     * @return available screen height in pixels
     */
    public static int getScreenHeight() {
        return Integer.parseInt(getAvailHeight());
    }

    private static native String getAvailHeight() /*-{
        return screen.availHeight + "";
    }-*/;

    private static native String getAvailWidth() /*-{
        return screen.availWidth + "";
    }-*/;

    public static String getTime(long time) {
        if(time < 0) {
            return "";
        }

        StringBuilder ret = new StringBuilder();
        ret.append(new Date(time).toString());
        ret.append(", ");

        long seconds = (System.currentTimeMillis() - time) / 1000;
        long day, hou, min, sec;

        day = seconds / (3600 * 24);
        seconds -= day * (3600 * 24);
        hou = seconds / 3600;
        seconds -= hou * 3600;
        min = seconds / 60;
        sec = seconds % 60;

        if (day > 0) {
            ret.append(day).append("d ");
            ret.append(hou).append("h ");
        } else if (hou > 0) {
            ret.append(hou).append("h");
            ret.append(min).append("mn ");
        } else if (min > 0) {
            ret.append(min).append("mn ");
        } else {
            ret.append(sec).append("s ");
        }

        ret.append("ago");

        return ret.toString();
    }
}
