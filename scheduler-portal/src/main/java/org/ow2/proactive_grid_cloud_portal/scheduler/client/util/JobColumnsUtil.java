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
package org.ow2.proactive_grid_cloud_portal.scheduler.client.util;

import java.util.Date;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.google.gwt.i18n.client.DateTimeFormat;
import com.google.gwt.json.client.JSONBoolean;
import com.google.gwt.json.client.JSONObject;
import com.google.gwt.json.client.JSONString;


/**
 * @author ActiveEon Team
 * @since Mar 6, 2017
 */
public class JobColumnsUtil {

    /**
     * Character separating type and key
     */
    private static final String FIELDS_SEPARATOR = "-";

    /**
     * Generic information type
     */
    private static final String GENERIC_INFORMATION_TYPE = "generic-information";

    /**
     * Input date format
     */
    private static final String DATE_FORMAT_OUTPUT = "yyyy-MM-dd HH:mm:ss";

    /**
     * Output date format
     */
    private static final String DATE_FORMAT_INPUT = "yyyy-MM-dd'T'HH:mm:ssZZZ";

    /**
     * Logger
     */
    private static final Logger LOGGER = Logger.getLogger(JobColumnsUtil.class.getName());

    /**
     * START_AT key
     */
    public static final String START_AT = "START_AT";

    /**
     * Get the generic information key from the column name
     * @param columnName the name of the column
     * @return the generic informaton key
     */
    public static String getGenericInformationKey(String columnName) {
        String startType = GENERIC_INFORMATION_TYPE + FIELDS_SEPARATOR;
        if (columnName.startsWith(startType))
            return columnName.replace(startType, "");
        return null;
    }

    /**
     * Get the title of an extra column from the JSON definition 
     * @param extraColumnProperties the JSON properties of the column
     * @param columnIndex the index of the column
     * @return the value of the title property
     */
    public static String getTitle(JSONObject extraColumnProperties, int columnIndex) {
        JSONString title = extraColumnProperties.get("title").isString();
        if (title != null)
            return title.stringValue();
        else {
            LOGGER.log(Level.SEVERE, "Could not read the field \"title\" of column number " + columnIndex);
            return "";
        }
    }

    /**
     * Get the name of an extra column from the JSON definition 
     * @param extraColumnProperties the JSON properties of the column
     * @param columnIndex the index of the column
     * @return the concatenation of the type and key of the property
     */
    public static String getName(JSONObject extraColumnProperties, int columnIndex) {

        JSONObject information = extraColumnProperties.get("information").isObject();
        if (information != null) {
            String type = getType(information, columnIndex);
            String key = getKey(information, columnIndex);
            return getNameFromTypeAndKey(type, key);
        } else {
            LOGGER.log(Level.SEVERE, "Could not read the field \"information\" of column number " + columnIndex);
            return "";
        }
    }

    /**
     * Concatenate the type and the key of a column separated by a given character
     * @param type the type of the column
     * @param key the key of the type
     * @return the cocatenation of both fields
     */
    private static String getNameFromTypeAndKey(String type, String key) {
        return type + FIELDS_SEPARATOR + key;
    }

    /**
     * Get the type of an extra column from the JSON definition 
     * @param information the JSON \"information\" property of the column
     * @param columnIndex the index of the column
     * @return the value of the type property
     */
    private static String getType(JSONObject information, int columnIndex) {
        JSONString typeJson = information.get("type").isString();
        if (typeJson != null)
            return typeJson.stringValue();
        else {
            LOGGER.log(Level.SEVERE,
                       "Could not read the field \"information\"->\"type\" of column number " + columnIndex);
            return "";
        }
    }

    /**
     * Get the key of an extra column from the JSON definition 
     * @param information the JSON \"information\" property of the column
     * @param columnIndex the index of the column
     * @return the value of the key property
     */
    private static String getKey(JSONObject information, int columnIndex) {
        JSONString keyJson = information.get("key").isString();
        if (keyJson != null)
            return keyJson.stringValue();
        else {
            LOGGER.log(Level.SEVERE,
                       "Could not read the field \"information\"->\"key\" of column number " + columnIndex);
            return "";
        }
    }

    /**
     * Get the hidden status of an extra column from the JSON definition 
     * @param extraColumnProperties the JSON properties of the column
     * @param columnIndex the index of the column
     * @return the value of the hide property
     */
    public static boolean getHide(JSONObject extraColumnProperties, int columnIndex) {

        JSONBoolean hidden = extraColumnProperties.get("hide").isBoolean();
        if (hidden != null)
            return hidden.booleanValue();
        else {
            LOGGER.log(Level.SEVERE, "Could not read the field \"hide\" of column number " + columnIndex);
            return false;
        }
    }

    /**
     * Format date String from time zone format to custom format
     * @param timeZoneDate date to format
     * @return the date with custom format
     */
    public static String getFormattedDateString(String timeZoneDate) {
        try {
            Date date = DateTimeFormat.getFormat(DATE_FORMAT_INPUT).parse(timeZoneDate);
            return DateTimeFormat.getFormat(DATE_FORMAT_OUTPUT).format(date);
        } catch (IllegalArgumentException e) {
            LOGGER.log(Level.SEVERE, "Error in date format " + timeZoneDate);
            return timeZoneDate;
        }
    }

}
