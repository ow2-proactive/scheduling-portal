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
package org.ow2.proactive_grid_cloud_portal.rm.shared;

public class CatalogConstants {

    private CatalogConstants() {
        // this class is not meant to be instantiated
    }

    public static final String URL_CATALOG = "http://localhost:8080/catalog";

    public static final String NODE_SOURCE_KIND = "NodeSource";

    public static final String NODE_SOURCE_CONTENT_TYPE = "application/json";

    public static final String INITIAL_COMMIT_MESSAGE = "(Initial commit)";

    public static final String EXPORT_FAILED_MESSAGE = "Export node source failed";

    public static final String SESSION_ID_PARAM = "sessionId";

    public static final String BUCKET_NAME_PARAM = "bucketName";

    public static final String NAME_PARAM = "name";

    public static final String FILE_CONTENT_PARAM = "fileContent";

    public static final String KIND_PARAM = "kind";

    public static final String COMMIT_MESSAGE_PARAM = "commitMessage";

    public static final String OBJECT_CONTENT_TYPE_PARAM = "objectContentType";

    public static final String REVISED_PARAM = "revised";

}
