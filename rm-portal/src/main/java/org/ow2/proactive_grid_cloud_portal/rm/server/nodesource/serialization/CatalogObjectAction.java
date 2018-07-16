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
package org.ow2.proactive_grid_cloud_portal.rm.server.nodesource.serialization;

import java.io.File;
import java.nio.file.Files;


public class CatalogObjectAction implements AutoCloseable {

    private String sessionId;

    private String bucketName;

    private String catalogObjectName;

    private File catalogObjectJsonFile;

    private String kind;

    private String commitMessage;

    private String objectContentType;

    private boolean revised;

    public String getSessionId() {
        return sessionId;
    }

    public void setSessionId(String sessionId) {
        this.sessionId = sessionId;
    }

    public String getBucketName() {
        return bucketName;
    }

    public void setBucketName(String bucketName) {
        this.bucketName = bucketName;
    }

    public String getCatalogObjectNameBase() {
        return catalogObjectName;
    }

    public void setCatalogObjectName(String catalogObjectName) {
        this.catalogObjectName = catalogObjectName;
    }

    public File getCatalogObjectJsonFile() {
        return catalogObjectJsonFile;
    }

    public void setCatalogObjectJsonFile(File catalogObjectJsonFile) {
        this.catalogObjectJsonFile = catalogObjectJsonFile;
    }

    public String getKind() {
        return kind;
    }

    public void setKind(String kind) {
        this.kind = kind;
    }

    public String getCommitMessage() {
        return commitMessage;
    }

    public void setCommitMessage(String commitMessage) {
        this.commitMessage = commitMessage;
    }

    public String getObjectContentType() {
        return objectContentType;
    }

    public void setObjectContentType(String objectContentType) {
        this.objectContentType = objectContentType;
    }

    public boolean isRevised() {
        return revised;
    }

    public void setRevised(boolean revised) {
        this.revised = revised;
    }

    @Override
    public void close() throws Exception {
        if (this.catalogObjectJsonFile != null) {
            Files.delete(this.catalogObjectJsonFile.toPath());
        }
    }

}
