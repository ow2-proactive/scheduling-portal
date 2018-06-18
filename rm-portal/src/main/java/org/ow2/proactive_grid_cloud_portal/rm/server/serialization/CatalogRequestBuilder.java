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
package org.ow2.proactive_grid_cloud_portal.rm.server.serialization;

import static org.ow2.proactive_grid_cloud_portal.rm.server.serialization.CatalogRequestConstants.*;

import java.io.IOException;
import java.security.KeyManagementException;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.util.UUID;

import javax.servlet.http.HttpServletResponse;

import org.apache.http.client.methods.HttpPost;
import org.apache.http.conn.ssl.SSLConnectionSocketFactory;
import org.apache.http.conn.ssl.SSLContextBuilder;
import org.apache.http.conn.ssl.TrustSelfSignedStrategy;
import org.apache.http.entity.mime.MultipartEntityBuilder;
import org.apache.http.entity.mime.content.FileBody;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.impl.client.HttpClients;


public class CatalogRequestBuilder {

    private final CatalogObject catalogObject;

    public CatalogRequestBuilder(CatalogObject catalogObject) {
        this.catalogObject = catalogObject;
    }

    public String build() {
        return new StringBuilder(URL_CATALOG).append("/buckets/")
                                             .append(this.catalogObject.getBucketName())
                                             .append("/resources?")
                                             .append(paramPair(NAME_PARAM, this.catalogObject.getNodeSourceName()))
                                             .append("&")
                                             .append(paramPair(KIND_PARAM, this.catalogObject.getKind()))
                                             .append("&")
                                             .append(paramPair(COMMIT_MESSAGE_PARAM,
                                                               this.catalogObject.getCommitMessage()))
                                             .append("&")
                                             .append(paramPair(OBJECT_CONTENT_TYPE_PARAM,
                                                               this.catalogObject.getObjectContentType()))
                                             .toString();
    }

    public void postNodeSourceRequestToCatalog(String sessionId, String fullUri, HttpServletResponse response)
            throws NoSuchAlgorithmException, KeyStoreException, KeyManagementException, IOException {
        HttpPost postNodeSource = buildCatalogRequest(sessionId, fullUri);
        getHttpClientBuilder().build().execute(postNodeSource);
    }

    private String paramPair(String param, String value) {
        return param + "=" + value;
    }

    private HttpPost buildCatalogRequest(String sessionId, String fullUri) {
        String boundary = "---------------" + UUID.randomUUID().toString();
        HttpPost post = new HttpPost(fullUri);
        post.addHeader("Accept", "application/json");
        post.addHeader("Content-Type",
                       org.apache.http.entity.ContentType.MULTIPART_FORM_DATA.getMimeType() + ";boundary=" + boundary);
        post.addHeader("sessionId", sessionId);

        MultipartEntityBuilder builder = MultipartEntityBuilder.create();
        builder.setBoundary(boundary);
        builder.setMode(org.apache.http.entity.mime.HttpMultipartMode.BROWSER_COMPATIBLE);
        builder.addPart("file", new FileBody(this.catalogObject.getNodeSourceJsonFile()));
        post.setEntity(builder.build());
        return post;
    }

    private HttpClientBuilder getHttpClientBuilder()
            throws KeyStoreException, NoSuchAlgorithmException, KeyManagementException {
        SSLContextBuilder sslContextBuilder = new SSLContextBuilder();
        sslContextBuilder.loadTrustMaterial(null, new TrustSelfSignedStrategy());
        SSLConnectionSocketFactory sslSocketFactory = new SSLConnectionSocketFactory(sslContextBuilder.build(),
                                                                                     SSLConnectionSocketFactory.ALLOW_ALL_HOSTNAME_VERIFIER);
        return HttpClients.custom().setSSLSocketFactory(sslSocketFactory);
    }

}
