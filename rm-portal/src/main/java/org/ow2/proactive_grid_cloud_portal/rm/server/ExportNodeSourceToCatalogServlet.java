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
package org.ow2.proactive_grid_cloud_portal.rm.server;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.security.KeyManagementException;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.util.Iterator;
import java.util.List;
import java.util.UUID;
import java.util.jar.JarFile;

import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.apache.commons.fileupload.FileItem;
import org.apache.commons.fileupload.FileUploadException;
import org.apache.commons.fileupload.disk.DiskFileItemFactory;
import org.apache.commons.fileupload.servlet.ServletFileUpload;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.apache.http.HttpEntity;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.conn.ssl.SSLConnectionSocketFactory;
import org.apache.http.conn.ssl.SSLContextBuilder;
import org.apache.http.conn.ssl.TrustSelfSignedStrategy;
import org.apache.http.entity.mime.MultipartEntityBuilder;
import org.apache.http.entity.mime.content.FileBody;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.impl.client.HttpClients;
import org.codehaus.jettison.json.JSONException;
import org.codehaus.jettison.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.w3c.dom.Document;

import com.google.gwt.user.server.Base64Utils;


/**
 * Servlet invoked when a node source is exported to the catalog. The request
 * content is expected to be in JSON format, containing the configuration of a
 * node source.
 */
public class ExportNodeSourceToCatalogServlet extends HttpServlet {

    public static final String SERVLET_MAPPING = "exportnodesourcetocatalog";

    private static final Logger LOGGER = LoggerFactory.getLogger(ExportNodeSourceToCatalogServlet.class);

    private static final String URL_CATALOG = "http://localhost:8080/catalog";

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) {
        upload(request, response);
    }

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) {
        upload(request, response);
    }

    private void upload(HttpServletRequest request, HttpServletResponse response) {
        response.setContentType("text/html");
        File job = null;

        try {
            DiskFileItemFactory factory = new DiskFileItemFactory();
            factory.setSizeThreshold(4096);
            factory.setRepository(new File(System.getProperty("java.io.tmpdir")));

            ServletFileUpload upload = new ServletFileUpload(factory);
            upload.setSizeMax(1000000);

            List<?> fileItems = upload.parseRequest(request);
            Iterator<?> i = fileItems.iterator();

            String sessionId = null;
            String bucketName = null;
            String workflowName = null;

            while (i.hasNext()) {
                FileItem fi = (FileItem) i.next();
                if (fi.isFormField()) {
                    if (fi.getFieldName().equals("sessionId")) {
                        sessionId = fi.getString();
                    } else if (fi.getFieldName().equals("bucketName")) {
                        bucketName = fi.getString();
                    } else if (fi.getFieldName().equals("workflowName")) {
                        workflowName = fi.getString();
                    }
                } else {
                    job = File.createTempFile("job_upload", ".xml");
                    fi.write(job);
                }
                fi.delete();
            }

            LOGGER.warn("sessionId=" + sessionId);
            LOGGER.warn("bucketName=" + bucketName);
            LOGGER.warn("workflowName=" + workflowName);
            LOGGER.warn("job=" + job);

            /*
             * if (bucketName != null && workflowName != null) {
             * fetchFromCatalogAndWriteResponse(bucketName, workflowName, sessionId, response);
             * } else {
             * writeResponse(job, response);
             * }
             */

            createAndExecutePostRequest(sessionId);

        } catch (Exception e) {
            try {
                String msg = e.getMessage().replace("<", "&lt;").replace(">", "&gt;");
                response.getWriter().write(msg);
            } catch (IOException ignored) {
            }
        } finally {
            if (job != null)
                job.delete();
        }
    }

    private void createAndExecutePostRequest(String sessionId) {
        File object_file = new File("/home/justine/Downloads/LocalNodes20-configuration.json");
        String boundary = "---------------" + UUID.randomUUID().toString();
        String query_push_obj_query = URL_CATALOG + "/buckets/" + "node-sources" + "/resources?name=" + "toto" +
                                      "&kind=" + "NodeSource" + "&commitMessage=" + "test" + "&objectContentType=" +
                                      "application/json";
        HttpPost post = new HttpPost(query_push_obj_query);
        post.addHeader("Accept", "application/json");
        post.addHeader("Content-Type",
                       org.apache.http.entity.ContentType.MULTIPART_FORM_DATA.getMimeType() + ";boundary=" + boundary);
        post.addHeader("sessionId", sessionId);

        MultipartEntityBuilder builder = MultipartEntityBuilder.create();
        builder.setBoundary(boundary);
        builder.setMode(org.apache.http.entity.mime.HttpMultipartMode.BROWSER_COMPATIBLE);
        builder.addPart("file", new FileBody(object_file));
        post.setEntity(builder.build());

        try {
            CloseableHttpResponse result = getHttpClientBuilder().build().execute(post);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private HttpClientBuilder getHttpClientBuilder() {
        try {
            SSLContextBuilder builder = new SSLContextBuilder();
            builder.loadTrustMaterial(null, new TrustSelfSignedStrategy());
            SSLConnectionSocketFactory sslsf = new SSLConnectionSocketFactory(builder.build(),
                                                                              SSLConnectionSocketFactory.ALLOW_ALL_HOSTNAME_VERIFIER);
            return HttpClients.custom().setSSLSocketFactory(sslsf);
        } catch (NoSuchAlgorithmException e) {
            e.printStackTrace();
        } catch (KeyStoreException e) {
            e.printStackTrace();
        } catch (KeyManagementException e) {
            e.printStackTrace();
        }
        return null;
    }

}
