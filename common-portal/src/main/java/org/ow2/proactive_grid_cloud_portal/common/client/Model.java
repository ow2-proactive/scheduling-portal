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
package org.ow2.proactive_grid_cloud_portal.common.client;

import java.util.List;
import java.util.Map;

import org.ow2.proactive_grid_cloud_portal.common.client.Model.StatHistory.Range;

import com.google.gwt.i18n.client.DateTimeFormat.PredefinedFormat;


/**
 * Locally stores data to share among client-side views
 * 
 * 
 * 
 * @author mschnoor
 *
 */
public interface Model {

    /**
     * Contains one single statistic history source,
     * stored with values, name and time range
     */
    public static class StatHistory {
        /**
         * time range for the statistic values
         */
        public enum Range {
            MINUTE_1("1 minute", 'a', 60, 5, PredefinedFormat.HOUR24_MINUTE_SECOND), //
            MINUTE_5("5 minutes", 'n', 60 * 5, 10, PredefinedFormat.HOUR24_MINUTE),
            MINUTE_10("10 minutes", 'm', 60 * 10, 20, PredefinedFormat.HOUR24_MINUTE), //
            MINUTE_30("30 minutes", 't', 60 * 30, 20, PredefinedFormat.HOUR24_MINUTE), //
            HOUR_1("1 hour", 'h', 60 * 60, 60, PredefinedFormat.HOUR24_MINUTE), //
            HOUR_2("2 hour", 'j', 60 * 60 * 2, 60, PredefinedFormat.HOUR24_MINUTE), //
            HOUR_4("4 hour", 'k', 60 * 60 * 4, 60, PredefinedFormat.HOUR24_MINUTE), //
            HOUR_8("8 hours", 'H', 60 * 60 * 8, 60 * 10, PredefinedFormat.HOUR24_MINUTE), //
            DAY_1("1 day", 'd', 60 * 60 * 24, 60 * 30, PredefinedFormat.HOUR24_MINUTE), //
            WEEK_1("1 week", 'w', 60 * 60 * 24 * 7, 60 * 60 * 3, PredefinedFormat.MONTH_NUM_DAY), //
            MONTH_1("1 month", 'M', 60 * 60 * 24 * 28, 60 * 60 * 8, PredefinedFormat.MONTH_NUM_DAY), //
            YEAR_1("1 year", 'y', 60 * 60 * 24 * 365, 60 * 60 * 24, PredefinedFormat.MONTH);

            private char charValue;

            private String stringValue;

            private long duration;

            private long updateFreq;

            private PredefinedFormat format;

            Range(String str, char c, long duration, long updateFreq, PredefinedFormat format) {
                this.stringValue = str;
                this.charValue = c;
                this.duration = duration;
                this.updateFreq = updateFreq;
                this.format = format;
            }

            public String getString() {
                return this.stringValue;
            }

            public char getChar() {
                return this.charValue;
            }

            public long getDuration() {
                return this.duration;
            }

            public long getUpdateFrequency() {
                return this.updateFreq;
            }

            public PredefinedFormat getFormat() {
                return this.format;
            }

            public static Range create(char c) {
                for (Range r : Range.values()) {
                    if (r.charValue == c)
                        return r;
                }
                return Range.MINUTE_1;
            }
        }

        public List<Double> values;

        public String source;

        public Range range;

        public StatHistory(String source, List<Double> values, Range range) {
            this.values = values;
            this.source = source;
            this.range = range;
        }
    }

    /**
     * @param source name of the statistic value to fetch
     * @return the statistic values of the requested source, or null. Values may contain Double.NaN
     */
    public abstract StatHistory getStatHistory(String source);

    /**
     * @return complete statistic values for all sources
     */
    public abstract Map<String, StatHistory> getStatHistory();

    /**
     * @param source source name of the statistic value to fetch
     * @return the Range that will be requested in future server communications for this statistics source
     */
    public abstract Range getRequestedStatHistoryRange(String source);

    //    /**
    //     * @param message issue a log message
    //     */
    //    public abstract void logMessage(String message);
    //
    //    /**
    //     * @param error issue an important message
    //     */
    //    public abstract void logImportantMessage(String message);
    //
    //    /**
    //     * @param error issue a critical message
    //     */
    //    public abstract void logCriticalMessage(String message);

}
