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

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the org.ow2.proactive_grid_cloud_portal.server.jaxb package. 
 * <p>An ObjectFactory allows you to programatically 
 * construct new instances of the Java representation 
 * for XML content. The Java representation of XML 
 * content can consist of schema derived interfaces 
 * and classes representing the binding of schema 
 * type definitions, element declarations and model 
 * groups.  Factory methods for each of these are 
 * provided in this class.
 * 
 */
@XmlRegistry
public class ObjectFactory {

    private final static QName _MapRecord_QNAME = new QName("", "mapRecord");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: org.ow2.proactive_grid_cloud_portal.server.jaxb
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link ContinuationRecord.ListPoint }
     * 
     */
    public ContinuationRecord.ListPoint createContinuationRecordListPoint() {
        return new ContinuationRecord.ListPoint();
    }

    /**
     * Create an instance of {@link ReplicatedRecord.Connector }
     * 
     */
    public ReplicatedRecord.Connector createReplicatedRecordConnector() {
        return new ReplicatedRecord.Connector();
    }

    /**
     * Create an instance of {@link LoopRecord.Connector }
     * 
     */
    public LoopRecord.Connector createLoopRecordConnector() {
        return new LoopRecord.Connector();
    }

    /**
     * Create an instance of {@link MapRecord }
     * 
     */
    public MapRecord createMapRecord() {
        return new MapRecord();
    }

    /**
     * Create an instance of {@link PositionRecord }
     * 
     */
    public PositionRecord createPositionRecord() {
        return new PositionRecord();
    }

    /**
     * Create an instance of {@link ConnectionRecord }
     * 
     */
    public ConnectionRecord createConnectionRecord() {
        return new ConnectionRecord();
    }

    /**
     * Create an instance of {@link TaskRecord.Connections }
     * 
     */
    public TaskRecord.Connections createTaskRecordConnections() {
        return new TaskRecord.Connections();
    }

    /**
     * Create an instance of {@link IfElseRecord.ConnectorElse }
     * 
     */
    public IfElseRecord.ConnectorElse createIfElseRecordConnectorElse() {
        return new IfElseRecord.ConnectorElse();
    }

    /**
     * Create an instance of {@link ConnectionRecord.Connector }
     * 
     */
    public ConnectionRecord.Connector createConnectionRecordConnector() {
        return new ConnectionRecord.Connector();
    }

    /**
     * Create an instance of {@link ContinuationRecord }
     * 
     */
    public ContinuationRecord createContinuationRecord() {
        return new ContinuationRecord();
    }

    /**
     * Create an instance of {@link LoopRecord }
     * 
     */
    public LoopRecord createLoopRecord() {
        return new LoopRecord();
    }

    /**
     * Create an instance of {@link IfElseRecord }
     * 
     */
    public IfElseRecord createIfElseRecord() {
        return new IfElseRecord();
    }

    /**
     * Create an instance of {@link MapRecord.Map }
     * 
     */
    public MapRecord.Map createMapRecordMap() {
        return new MapRecord.Map();
    }

    /**
     * Create an instance of {@link IfElseRecord.ConnectorIf }
     * 
     */
    public IfElseRecord.ConnectorIf createIfElseRecordConnectorIf() {
        return new IfElseRecord.ConnectorIf();
    }

    /**
     * Create an instance of {@link ReplicatedRecord }
     * 
     */
    public ReplicatedRecord createReplicatedRecord() {
        return new ReplicatedRecord();
    }

    /**
     * Create an instance of {@link SizeRecord }
     * 
     */
    public SizeRecord createSizeRecord() {
        return new SizeRecord();
    }

    /**
     * Create an instance of {@link TaskRecord }
     * 
     */
    public TaskRecord createTaskRecord() {
        return new TaskRecord();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link MapRecord }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "", name = "mapRecord")
    public JAXBElement<MapRecord> createMapRecord(MapRecord value) {
        return new JAXBElement<MapRecord>(_MapRecord_QNAME, MapRecord.class, null, value);
    }

}
