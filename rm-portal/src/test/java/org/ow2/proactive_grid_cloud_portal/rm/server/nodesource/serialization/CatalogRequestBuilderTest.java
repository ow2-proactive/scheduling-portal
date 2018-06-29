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

import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.when;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;

import org.apache.http.client.methods.HttpPost;
import org.hamcrest.CoreMatchers;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.ow2.proactive_grid_cloud_portal.rm.shared.CatalogConstants;


@RunWith(MockitoJUnitRunner.class)
public class CatalogRequestBuilderTest {

    @Mock
    private CatalogObjectAction catalogObjectAction;

    private CatalogRequestBuilder catalogRequestBuilder;

    @Before
    public void setup() throws IOException {
        this.catalogRequestBuilder = new CatalogRequestBuilder(catalogObjectAction);
        File jsonFile = Files.createTempFile("nodesource", ".json").toFile();
        when(catalogObjectAction.getSessionId()).thenReturn("A4D5ca7vCAC582113WQCjmnW28");
        when(catalogObjectAction.getNodeSourceName()).thenReturn("myNodeSource");
        when(catalogObjectAction.getNodeSourceJsonFile()).thenReturn(jsonFile);
        when(catalogObjectAction.getCommitMessage()).thenReturn("commit message");
        when(catalogObjectAction.getKind()).thenReturn("NodeSource");
        when(catalogObjectAction.getObjectContentType()).thenReturn("application/json");
    }

    @Test
    public void testParametersAreSetForRevisedNodeSourceRequest() throws IOException {
        when(this.catalogObjectAction.isRevised()).thenReturn(true);
        HttpPost httpPost = this.catalogRequestBuilder.buildCatalogRequest("http://localhost:8080/catalog");
        String entityStringContent = getMultipartEntityString(httpPost);
        checkThatRequestContainsRegularParameters(entityStringContent);
        assertThat("Request contains the " + CatalogConstants.KIND_PARAM + " parameter whereas it shouldn't",
                   CoreMatchers.not(entityStringContent.contains(CatalogConstants.KIND_PARAM)));
        assertThat("Request contains the " + CatalogConstants.OBJECT_CONTENT_TYPE_PARAM +
                   " parameter whereas it shouldn't",
                   CoreMatchers.not(entityStringContent.contains(CatalogConstants.OBJECT_CONTENT_TYPE_PARAM)));
    }

    @Test
    public void testParametersAreSetForNewNodeSourceRequest() throws IOException {
        when(this.catalogObjectAction.isRevised()).thenReturn(false);
        HttpPost httpPost = this.catalogRequestBuilder.buildCatalogRequest("http://localhost:8080/catalog");
        String entityStringContent = getMultipartEntityString(httpPost);
        checkThatRequestContainsRegularParameters(entityStringContent);
        assertThat("Request does not contain the " + CatalogConstants.KIND_PARAM + " parameter",
                   entityStringContent.contains(CatalogConstants.KIND_PARAM));
        assertThat("Request does not contain the " + CatalogConstants.OBJECT_CONTENT_TYPE_PARAM + " parameter",
                   entityStringContent.contains(CatalogConstants.OBJECT_CONTENT_TYPE_PARAM));
    }

    private void checkThatRequestContainsRegularParameters(String entityStringContent) {
        assertThat("Request does not contain the " + CatalogConstants.FILE_CONTENT_PARAM + " parameter",
                   entityStringContent.contains(CatalogConstants.FILE_CONTENT_PARAM));
        assertThat("Request does not contain the " + CatalogConstants.COMMIT_MESSAGE_PARAM + " parameter",
                   entityStringContent.contains(CatalogConstants.COMMIT_MESSAGE_PARAM));
    }

    private String getMultipartEntityString(HttpPost httpPost) throws IOException {
        ByteArrayOutputStream out = new ByteArrayOutputStream((int) httpPost.getEntity().getContentLength());
        httpPost.getEntity().writeTo(out);
        return out.toString();
    }

}
