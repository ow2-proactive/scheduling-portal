package org.ow2.proactive_grid_cloud_portal.rm.server.serialization;

import org.apache.http.client.methods.HttpPost;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.ow2.proactive_grid_cloud_portal.rm.shared.CatalogConstants;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.when;

@RunWith(MockitoJUnitRunner.class)
public class CatalogRequestBuilderTest {

    @Mock
    private CatalogObjectAction catalogObjectAction;

    private File jsonFile;

    private CatalogRequestBuilder catalogRequestBuilder;

    @Before
    public void setup() throws IOException {
        this.catalogRequestBuilder = new CatalogRequestBuilder(catalogObjectAction);
        this.jsonFile = Files.createTempFile("nodesource", ".json").toFile();
        when(catalogObjectAction.getSessionId()).thenReturn("A4D5ca7vCAC582113WQCjmnW28");
        when(catalogObjectAction.getNodeSourceName()).thenReturn("myNodeSource");
        when(catalogObjectAction.getNodeSourceJsonFile()).thenReturn(this.jsonFile);
        when(catalogObjectAction.getCommitMessage()).thenReturn("commit message");
        when(catalogObjectAction.getKind()).thenReturn("NodeSource");
        when(catalogObjectAction.getObjectContentType()).thenReturn("application/json");
    }

    @Test
    public void testParametersAreSetForRevisedNodeSourceRequest() throws IOException {
        when(this.catalogObjectAction.isRevised()).thenReturn(true);
        HttpPost httpPost = this.catalogRequestBuilder.buildCatalogRequest("http://localhost:8080/catalog");
        String string = getMultipartEntityString(httpPost);
        checkThatRequestContainsRegularParameters(string);
        assertThat("Request contains the " + CatalogConstants.KIND_PARAM + " parameter whereas it shouldn't", !string.contains(CatalogConstants.KIND_PARAM));
        assertThat("Request contains the " + CatalogConstants.OBJECT_CONTENT_TYPE_PARAM + " parameter whereas it shouldn't", !string.contains(CatalogConstants.OBJECT_CONTENT_TYPE_PARAM));
    }

    @Test
    public void testParametersAreSetForNewNodeSourceRequest() throws IOException {
        when(this.catalogObjectAction.isRevised()).thenReturn(false);
        HttpPost httpPost = this.catalogRequestBuilder.buildCatalogRequest("http://localhost:8080/catalog");
        String string = getMultipartEntityString(httpPost);
        checkThatRequestContainsRegularParameters(string);
        assertThat("Request does not contain the " + CatalogConstants.KIND_PARAM + " parameter", string.contains(CatalogConstants.KIND_PARAM));
        assertThat("Request does not contain the " + CatalogConstants.OBJECT_CONTENT_TYPE_PARAM + " parameter", string.contains(CatalogConstants.OBJECT_CONTENT_TYPE_PARAM));
    }

    private void checkThatRequestContainsRegularParameters(String string) {
        assertThat("Request does not contain the " + CatalogConstants.FILE_CONTENT_PARAM + " parameter", string.contains(CatalogConstants.FILE_CONTENT_PARAM));
        assertThat("Request does not contain the " + CatalogConstants.COMMIT_MESSAGE_PARAM + " parameter", string.contains(CatalogConstants.COMMIT_MESSAGE_PARAM));
    }

    private String getMultipartEntityString(HttpPost httpPost) throws IOException {
        ByteArrayOutputStream out = new ByteArrayOutputStream((int) httpPost.getEntity().getContentLength());
        httpPost.getEntity().writeTo(out);
        return out.toString();
    }

}
