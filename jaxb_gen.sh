#!/bin/bash


if [ $# != 1 ] ; then
	echo "Usage: ./$0 <studio_src>"
	exit 1
fi


cd $(dirname $0)

STUDIO=$1
SCHEMA="schema1.xsd"
CP="com.activeeon.workfloweditor.diagram.plugin/bin/"
CLASS="com.activeeon.workfloweditor.diagram.plugin/src/com/activeeon/workfloweditor/monitor/MapRecord.java"
PACKAGE="org.ow2.proactive_grid_cloud_portal.scheduler.server.jaxb"

schemagen -cp $STUDIO/$CP $STUDIO/$CLASS
xjc -d ../src/ $SCHEMA -p $PACKAGE

rm $SCHEMA
