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
package org.ow2.proactive_grid_cloud_portal.rm.client;

public class NodeSourceIconProvider {

    public static String getIconUri(String infrastructureType, NodeSourceStatus nodeSourceStatus) {
        if (nodeSourceStatus.equals(NodeSourceStatus.NODES_DEPLOYED)) {
            switch (infrastructureType) {
                case "AzureInfrastructure":
                    return RMImages.instance.azureVm().getSafeUri().asString();
                case "AzureVMScaleSetInfrastructure":
                    return RMImages.instance.azureScaleSet().getSafeUri().asString();
                case "SSHInfrastructure":
                case "SSHInfrastructureV2":
                    return RMImages.instance.sshv2().getSafeUri().asString();
                case "AWSEC2Infrastructure":
                    return RMImages.instance.awsEc2Vm().getSafeUri().asString();
                case "NativeSchedulerInfrastructure":
                    return RMImages.instance.nativeScheduler().getSafeUri().asString();
                case "AwsAutoScalingInfrastructure":
                    return RMImages.instance.awsEc2Autoscaling().getSafeUri().asString();
                case "CLIInfrastructure":
                    return RMImages.instance.CLI().getSafeUri().asString();
                case "GenericBatchJobInfrastructure":
                    return RMImages.instance.genericBatch().getSafeUri().asString();
                case "VMWareInfrastructure":
                    return RMImages.instance.vmWare().getSafeUri().asString();
                case "LocalInfrastructure":
                    return RMImages.instance.local().getSafeUri().asString();
                case "KubernetesInfrastructure":
                    return RMImages.instance.kubernetes().getSafeUri().asString();
                case "OpenstackInfrastructure":
                    return RMImages.instance.openStack().getSafeUri().asString();
                case "LSF":
                    return RMImages.instance.ibmLsf().getSafeUri().asString();
                case "GCEInfrastructure":
                    return RMImages.instance.gce().getSafeUri().asString();
                default:
                    return RMImages.instance.nodesource_deployed().getSafeUri().asString();
            }
        } else {
            switch (infrastructureType) {
                case "AzureInfrastructure":
                    return RMImages.instance.azureVmUndeployed().getSafeUri().asString();
                case "AzureVMScaleSetInfrastructure":
                    return RMImages.instance.azureScaleSetUndeployed().getSafeUri().asString();
                case "SSHInfrastructure":
                case "SSHInfrastructureV2":
                    return RMImages.instance.sshv2Undeployed().getSafeUri().asString();
                case "AWSEC2Infrastructure":
                    return RMImages.instance.awsEc2Vmundeployed().getSafeUri().asString();
                case "NativeSchedulerInfrastructure":
                    return RMImages.instance.nativeSchedulerUndeployed().getSafeUri().asString();
                case "AwsAutoScalingInfrastructure":
                    return RMImages.instance.awsEc2AutoscalingUndeployed().getSafeUri().asString();
                case "CLIInfrastructure":
                    return RMImages.instance.CLIUndeployed().getSafeUri().asString();
                case "GenericBatchJobInfrastructure":
                    return RMImages.instance.genericBatchUndeployed().getSafeUri().asString();
                case "VMWareInfrastructure":
                    return RMImages.instance.vmWareUndeployed().getSafeUri().asString();
                case "LocalInfrastructure":
                    return RMImages.instance.localUndeployed().getSafeUri().asString();
                case "KubernetesInfrastructure":
                    return RMImages.instance.kubernetesUndeployed().getSafeUri().asString();
                case "OpenstackInfrastructure":
                    return RMImages.instance.openStackUndeployed().getSafeUri().asString();
                case "LSF":
                    return RMImages.instance.ibmLsfUndeployed().getSafeUri().asString();
                case "GCEInfrastructure":
                    return RMImages.instance.gceUndeployed().getSafeUri().asString();
                default:
                    return RMImages.instance.nodesource_undeployed().getSafeUri().asString();
            }
        }
    }
}
