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
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for taskRecord complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="taskRecord">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="name" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="position" type="{}positionRecord" minOccurs="0"/>
 *         &lt;element name="size" type="{}sizeRecord" minOccurs="0"/>
 *         &lt;element name="connections" minOccurs="0">
 *           &lt;complexType>
 *             &lt;complexContent>
 *               &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *                 &lt;sequence>
 *                   &lt;element name="connection" type="{}connectionRecord" maxOccurs="unbounded" minOccurs="0"/>
 *                 &lt;/sequence>
 *               &lt;/restriction>
 *             &lt;/complexContent>
 *           &lt;/complexType>
 *         &lt;/element>
 *         &lt;choice minOccurs="0">
 *           &lt;element name="IfElse" type="{}ifElseRecord"/>
 *           &lt;element name="Loop" type="{}loopRecord"/>
 *           &lt;element name="Replicate" type="{}replicatedRecord"/>
 *         &lt;/choice>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "taskRecord", propOrder = { "name", "position", "size", "connections", "ifElse", "loop", "replicate" })
public class TaskRecord {

    protected String name;

    protected PositionRecord position;

    protected SizeRecord size;

    protected TaskRecord.Connections connections;

    @XmlElement(name = "IfElse")
    protected IfElseRecord ifElse;

    @XmlElement(name = "Loop")
    protected LoopRecord loop;

    @XmlElement(name = "Replicate")
    protected ReplicatedRecord replicate;

    /**
     * Gets the value of the name property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getName() {
        return name;
    }

    /**
     * Sets the value of the name property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setName(String value) {
        this.name = value;
    }

    /**
     * Gets the value of the position property.
     * 
     * @return
     *     possible object is
     *     {@link PositionRecord }
     *     
     */
    public PositionRecord getPosition() {
        return position;
    }

    /**
     * Sets the value of the position property.
     * 
     * @param value
     *     allowed object is
     *     {@link PositionRecord }
     *     
     */
    public void setPosition(PositionRecord value) {
        this.position = value;
    }

    /**
     * Gets the value of the size property.
     * 
     * @return
     *     possible object is
     *     {@link SizeRecord }
     *     
     */
    public SizeRecord getSize() {
        return size;
    }

    /**
     * Sets the value of the size property.
     * 
     * @param value
     *     allowed object is
     *     {@link SizeRecord }
     *     
     */
    public void setSize(SizeRecord value) {
        this.size = value;
    }

    /**
     * Gets the value of the connections property.
     * 
     * @return
     *     possible object is
     *     {@link TaskRecord.Connections }
     *     
     */
    public TaskRecord.Connections getConnections() {
        return connections;
    }

    /**
     * Sets the value of the connections property.
     * 
     * @param value
     *     allowed object is
     *     {@link TaskRecord.Connections }
     *     
     */
    public void setConnections(TaskRecord.Connections value) {
        this.connections = value;
    }

    /**
     * Gets the value of the ifElse property.
     * 
     * @return
     *     possible object is
     *     {@link IfElseRecord }
     *     
     */
    public IfElseRecord getIfElse() {
        return ifElse;
    }

    /**
     * Sets the value of the ifElse property.
     * 
     * @param value
     *     allowed object is
     *     {@link IfElseRecord }
     *     
     */
    public void setIfElse(IfElseRecord value) {
        this.ifElse = value;
    }

    /**
     * Gets the value of the loop property.
     * 
     * @return
     *     possible object is
     *     {@link LoopRecord }
     *     
     */
    public LoopRecord getLoop() {
        return loop;
    }

    /**
     * Sets the value of the loop property.
     * 
     * @param value
     *     allowed object is
     *     {@link LoopRecord }
     *     
     */
    public void setLoop(LoopRecord value) {
        this.loop = value;
    }

    /**
     * Gets the value of the replicate property.
     * 
     * @return
     *     possible object is
     *     {@link ReplicatedRecord }
     *     
     */
    public ReplicatedRecord getReplicate() {
        return replicate;
    }

    /**
     * Sets the value of the replicate property.
     * 
     * @param value
     *     allowed object is
     *     {@link ReplicatedRecord }
     *     
     */
    public void setReplicate(ReplicatedRecord value) {
        this.replicate = value;
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
     *         &lt;element name="connection" type="{}connectionRecord" maxOccurs="unbounded" minOccurs="0"/>
     *       &lt;/sequence>
     *     &lt;/restriction>
     *   &lt;/complexContent>
     * &lt;/complexType>
     * </pre>
     * 
     * 
     */
    @XmlAccessorType(XmlAccessType.FIELD)
    @XmlType(name = "", propOrder = { "connection" })
    public static class Connections {

        protected List<ConnectionRecord> connection;

        /**
         * Gets the value of the connection property.
         * 
         * <p>
         * This accessor method returns a reference to the live list,
         * not a snapshot. Therefore any modification you make to the
         * returned list will be present inside the JAXB object.
         * This is why there is not a <CODE>set</CODE> method for the connection property.
         * 
         * <p>
         * For example, to add a new item, do as follows:
         * <pre>
         *    getConnection().add(newItem);
         * </pre>
         * 
         * 
         * <p>
         * Objects of the following type(s) are allowed in the list
         * {@link ConnectionRecord }
         * 
         * 
         */
        public List<ConnectionRecord> getConnection() {
            if (connection == null) {
                connection = new ArrayList<ConnectionRecord>();
            }
            return this.connection;
        }

    }

}
