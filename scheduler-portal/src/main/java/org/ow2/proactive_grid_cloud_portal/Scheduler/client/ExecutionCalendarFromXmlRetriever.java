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
package org.ow2.proactive_grid_cloud_portal.scheduler.client;

import java.io.InputStream;
import java.rmi.ServerException;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;

import org.w3c.dom.Document;
import org.xml.sax.InputSource;


public class ExecutionCalendarFromXmlRetriever {

    private static final String EXECUTION_CALENDAR_TAG = "EXECUTION_CALENDARS";

    private static DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();

    public String getExecutionCalendarTagValueFrom(InputStream xmlContent) throws Exception {
        try {
            DocumentBuilder documentBuilder = dbf.newDocumentBuilder();
            Document xmlDocument = documentBuilder.parse(new InputSource(xmlContent));
            return (String) getExecutionCalendarTagTextExpression(xmlDocument).evaluate(xmlDocument,
                                                                                        XPathConstants.STRING);
        } catch (org.xml.sax.SAXParseException parseException) {
            throw new Exception("A parsing error occurred while extracting the execution calendar generic information.",
                                parseException);
        } catch (Exception ex) {
            throw new ServerException(ExecutionCalendarFromXmlRetriever.class.getName() + " : " + ex.getMessage(), ex);
        }
    }

    private XPathExpression getExecutionCalendarTagTextExpression(Document xmlDoc) throws XPathExpressionException {
        XPathFactory xPathFactory = XPathFactory.newInstance();
        XPath xPath = xPathFactory.newXPath();
        return xPath.compile("/job/genericInformation/info[@name='" + EXECUTION_CALENDAR_TAG + "']/@value");
    }

}
