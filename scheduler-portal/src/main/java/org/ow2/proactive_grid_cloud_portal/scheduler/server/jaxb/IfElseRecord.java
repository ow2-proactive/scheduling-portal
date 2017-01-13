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
package org.ow2.proactive_grid_cloud_portal.scheduler.server.jaxb;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for ifElseRecord complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="ifElseRecord">
 *   &lt;complexContent>
 *     &lt;extension base="{}flowControlRecord">
 *       &lt;sequence>
 *         &lt;element name="source" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="targetIf" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="connectorIf" minOccurs="0">
 *           &lt;complexType>
 *             &lt;complexContent>
 *               &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *                 &lt;sequence>
 *                   &lt;element name="point" type="{}positionRecord" maxOccurs="unbounded" minOccurs="0"/>
 *                 &lt;/sequence>
 *               &lt;/restriction>
 *             &lt;/complexContent>
 *           &lt;/complexType>
 *         &lt;/element>
 *         &lt;element name="targetElse" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="connectorElse" minOccurs="0">
 *           &lt;complexType>
 *             &lt;complexContent>
 *               &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *                 &lt;sequence>
 *                   &lt;element name="point" type="{}positionRecord" maxOccurs="unbounded" minOccurs="0"/>
 *                 &lt;/sequence>
 *               &lt;/restriction>
 *             &lt;/complexContent>
 *           &lt;/complexType>
 *         &lt;/element>
 *       &lt;/sequence>
 *     &lt;/extension>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "ifElseRecord", propOrder = { "source", "targetIf", "connectorIf", "targetElse", "connectorElse" })
public class IfElseRecord extends FlowControlRecord {

    protected String source;

    protected String targetIf;

    protected IfElseRecord.ConnectorIf connectorIf;

    protected String targetElse;

    protected IfElseRecord.ConnectorElse connectorElse;

    /**
     * Gets the value of the source property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getSource() {
        return source;
    }

    /**
     * Sets the value of the source property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setSource(String value) {
        this.source = value;
    }

    /**
     * Gets the value of the targetIf property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getTargetIf() {
        return targetIf;
    }

    /**
     * Sets the value of the targetIf property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setTargetIf(String value) {
        this.targetIf = value;
    }

    /**
     * Gets the value of the connectorIf property.
     * 
     * @return
     *     possible object is
     *     {@link IfElseRecord.ConnectorIf }
     *     
     */
    public IfElseRecord.ConnectorIf getConnectorIf() {
        return connectorIf;
    }

    /**
     * Sets the value of the connectorIf property.
     * 
     * @param value
     *     allowed object is
     *     {@link IfElseRecord.ConnectorIf }
     *     
     */
    public void setConnectorIf(IfElseRecord.ConnectorIf value) {
        this.connectorIf = value;
    }

    /**
     * Gets the value of the targetElse property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getTargetElse() {
        return targetElse;
    }

    /**
     * Sets the value of the targetElse property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setTargetElse(String value) {
        this.targetElse = value;
    }

    /**
     * Gets the value of the connectorElse property.
     * 
     * @return
     *     possible object is
     *     {@link IfElseRecord.ConnectorElse }
     *     
     */
    public IfElseRecord.ConnectorElse getConnectorElse() {
        return connectorElse;
    }

    /**
     * Sets the value of the connectorElse property.
     * 
     * @param value
     *     allowed object is
     *     {@link IfElseRecord.ConnectorElse }
     *     
     */
    public void setConnectorElse(IfElseRecord.ConnectorElse value) {
        this.connectorElse = value;
    }

    /**
     * <p>Java class for anonymous complex type.
     * 
     * <p>The following schema fragment specifies the expected content contained within this class.
     * 
     * <pre>
     * &lt;complexType>
     *   &lt;complexContent>
     *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
     *       &lt;sequence>
     *         &lt;element name="point" type="{}positionRecord" maxOccurs="unbounded" minOccurs="0"/>
     *       &lt;/sequence>
     *     &lt;/restriction>
     *   &lt;/complexContent>
     * &lt;/complexType>
     * </pre>
     * 
     * 
     */
    @XmlAccessorType(XmlAccessType.FIELD)
    @XmlType(name = "", propOrder = { "point" })
    public static class ConnectorElse {

        protected List<PositionRecord> point;

        /**
         * Gets the value of the point property.
         * 
         * <p>
         * This accessor method returns a reference to the live list,
         * not a snapshot. Therefore any modification you make to the
         * returned list will be present inside the JAXB object.
         * This is why there is not a <CODE>set</CODE> method for the point property.
         * 
         * <p>
         * For example, to add a new item, do as follows:
         * <pre>
         *    getPoint().add(newItem);
         * </pre>
         * 
         * 
         * <p>
         * Objects of the following type(s) are allowed in the list
         * {@link PositionRecord }
         * 
         * 
         */
        public List<PositionRecord> getPoint() {
            if (point == null) {
                point = new ArrayList<PositionRecord>();
            }
            return this.point;
        }

    }

    /**
     * <p>Java class for anonymous complex type.
     * 
     * <p>The following schema fragment specifies the expected content contained within this class.
     * 
     * <pre>
     * &lt;complexType>
     *   &lt;complexContent>
     *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
     *       &lt;sequence>
     *         &lt;element name="point" type="{}positionRecord" maxOccurs="unbounded" minOccurs="0"/>
     *       &lt;/sequence>
     *     &lt;/restriction>
     *   &lt;/complexContent>
     * &lt;/complexType>
     * </pre>
     * 
     * 
     */
    @XmlAccessorType(XmlAccessType.FIELD)
    @XmlType(name = "", propOrder = { "point" })
    public static class ConnectorIf {

        protected List<PositionRecord> point;

        /**
         * Gets the value of the point property.
         * 
         * <p>
         * This accessor method returns a reference to the live list,
         * not a snapshot. Therefore any modification you make to the
         * returned list will be present inside the JAXB object.
         * This is why there is not a <CODE>set</CODE> method for the point property.
         * 
         * <p>
         * For example, to add a new item, do as follows:
         * <pre>
         *    getPoint().add(newItem);
         * </pre>
         * 
         * 
         * <p>
         * Objects of the following type(s) are allowed in the list
         * {@link PositionRecord }
         * 
         * 
         */
        public List<PositionRecord> getPoint() {
            if (point == null) {
                point = new ArrayList<PositionRecord>();
            }
            return this.point;
        }

    }

}
