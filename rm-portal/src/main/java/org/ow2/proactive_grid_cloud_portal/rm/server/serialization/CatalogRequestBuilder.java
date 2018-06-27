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

import static org.ow2.proactive_grid_cloud_portal.rm.shared.CatalogConstants.*;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.security.KeyManagementException;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.util.Arrays;
import java.util.UUID;
import java.util.stream.Collectors;

import javax.servlet.http.HttpServletResponse;

import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.conn.ssl.SSLConnectionSocketFactory;
import org.apache.http.conn.ssl.SSLContextBuilder;
import org.apache.http.conn.ssl.TrustSelfSignedStrategy;
import org.apache.http.entity.mime.HttpMultipartMode;
import org.apache.http.entity.mime.MultipartEntityBuilder;
import org.apache.http.entity.mime.content.FileBody;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.impl.client.HttpClients;


public class CatalogRequestBuilder {

    private final CatalogObjectAction catalogObjectAction;

    public CatalogRequestBuilder(CatalogObjectAction catalogObjectAction) {
        this.catalogObjectAction = catalogObjectAction;
    }

    public String build() {
        String urlEncodedCommitMessage = getUrlEncodedCommitMessageOrFail();
        StringBuilder builder = new StringBuilder(URL_CATALOG).append("/buckets/")
                                                              .append(this.catalogObjectAction.getBucketName());
        if (this.catalogObjectAction.isRevised()) {
            builder.append("/resources/").append(this.catalogObjectAction.getNodeSourceName()).append("/revisions?");
            appendParameters(builder, paramPair(COMMIT_MESSAGE_PARAM, urlEncodedCommitMessage));
        } else {
            builder.append("/resources?");
            appendParameters(builder,
                             paramPair(NAME_PARAM, this.catalogObjectAction.getNodeSourceName()),
                             paramPair(KIND_PARAM, this.catalogObjectAction.getKind()),
                             paramPair(COMMIT_MESSAGE_PARAM, urlEncodedCommitMessage),
                             paramPair(OBJECT_CONTENT_TYPE_PARAM, this.catalogObjectAction.getObjectContentType()));
        }
        return builder.toString();
    }

    private String getUrlEncodedCommitMessageOrFail() {
        try {
            return URLEncoder.encode(this.catalogObjectAction.getCommitMessage(), "UTF-8");
        } catch (UnsupportedEncodingException e) {
            throw new IllegalArgumentException("Commit message cannot be encoded properly", e);
        }
    }

    private void appendParameters(StringBuilder builder, String... paramPairs) {
        builder.append(Arrays.stream(paramPairs).collect(Collectors.joining("&")));
    }

    public CloseableHttpResponse postNodeSourceRequestToCatalog(String sessionId, String fullUri)
            throws NoSuchAlgorithmException, KeyStoreException, KeyManagementException, IOException {
        HttpPost postNodeSource = buildCatalogRequest(sessionId, fullUri);
        return getHttpClientBuilder().build().execute(postNodeSource);
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
        builder.setMode(HttpMultipartMode.BROWSER_COMPATIBLE);
        builder.addPart("file", new FileBody(this.catalogObjectAction.getNodeSourceJsonFile()));
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
