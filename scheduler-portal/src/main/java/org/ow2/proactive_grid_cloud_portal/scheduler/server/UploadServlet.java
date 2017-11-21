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
package org.ow2.proactive_grid_cloud_portal.scheduler.server;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.util.Iterator;
import java.util.List;
import java.util.jar.JarFile;

import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.apache.commons.fileupload.FileItem;
import org.apache.commons.fileupload.disk.DiskFileItemFactory;
import org.apache.commons.fileupload.servlet.ServletFileUpload;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.apache.http.HttpEntity;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.w3c.dom.Document;

import com.google.gwt.user.server.Base64Utils;


/**
 * Default job submission servlet.
 * <p>
 * Form must be multipart and contain:
 * . one file, name ignored, must be a valid XML job descriptor
 * . one form field named 'sessionId' used to connect to the server
 * . one optional form field named 'edit'. if edit == "1",
 * the servlet does not submit the job, but simply writes the following string as application/json response:
 * { "jobEdit" : "<DESC_64>" }
 * where <DESC_64> is a base64 encoded version of the job sent as parameter.
 * This will allow client to edit the descriptor, as javascript runtimes are not allow to open local files.
 */
@SuppressWarnings("serial")
public class UploadServlet extends HttpServlet {

    private static final Logger LOGGER = LoggerFactory.getLogger(UploadServlet.class);

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
            String bucketId = null;
            String workflowName = null;

            while (i.hasNext()) {
                FileItem fi = (FileItem) i.next();
                if (fi.isFormField()) {
                    if (fi.getFieldName().equals("sessionId")) {
                        sessionId = fi.getString();
                    } else if (fi.getFieldName().equals("bucketId")) {
                        bucketId = fi.getString();
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
            LOGGER.warn("bucketId=" + bucketId);
            LOGGER.warn("workflowName=" + workflowName);
            LOGGER.warn("job=" + job);

            if (bucketId != null && workflowName != null) {
                fetchFromCatalogAndWriteResponse(bucketId, workflowName, response);
            } else {
                writeResponse(job, response);
            }

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

    private void fetchFromCatalogAndWriteResponse(String bucketId, String workflowName, HttpServletResponse response) {
        String encodedWorkflowName;
        try {
            encodedWorkflowName = URLEncoder.encode(workflowName, "UTF-8");
        } catch (UnsupportedEncodingException e) {
            encodedWorkflowName = workflowName;
        }
        String url = URL_CATALOG + "/buckets/" + bucketId + "/resources/" + encodedWorkflowName + "/raw";
        LOGGER.info("Sending request to catalog: " + url);
        CloseableHttpClient httpclient = HttpClients.createDefault();
        HttpGet httpget = new HttpGet(url);
        CloseableHttpResponse httpResponse = null;
        try {
            httpResponse = httpclient.execute(httpget);
            HttpEntity responseBody = httpResponse.getEntity();
            File job = File.createTempFile("job_upload", ".xml");
            FileUtils.copyInputStreamToFile(responseBody.getContent(), job);
            LOGGER.info(job.toString());
            writeResponse(job, response);
        } catch (Exception e) {
            LOGGER.error("Error when sending the request to catalog", e);
        } finally {
            try {
                if (httpResponse != null) {
                    httpResponse.close();
                }

            } catch (IOException e) {
                LOGGER.error("Error when closing the response", e);
            }
        }
    }

    private void writeResponse(File job, HttpServletResponse response) {
        try {
            boolean isJar = isJarFile(job);

            if (!isJar) {
                // this _loosely_ checks that the file we got is an XML file
                try {
                    DocumentBuilderFactory docFactory = DocumentBuilderFactory.newInstance();
                    DocumentBuilder docBuilder = docFactory.newDocumentBuilder();
                    Document doc = docBuilder.parse(job);
                } catch (Throwable e) {
                    response.getWriter().write("Job descriptor must be valid XML<br>" + e.getMessage());
                    return;
                }
            }
            String ret = IOUtils.toString(new FileInputStream(job), "UTF-8");
            response.getWriter().write("{ \"jobEdit\" : \"" + Base64Utils.toBase64(ret.getBytes()) + "\" }");
        } catch (IOException e) {
            LOGGER.error("Error when reading the job content", e);
            String msg = e.getMessage().replace("<", "&lt;").replace(">", "&gt;");
            try {
                response.getWriter().write(msg);
            } catch (IOException ignored) {
                // to get the error back client-side
            }
        } finally {
            if (job != null)
                job.delete();
        }
    }

    private boolean isJarFile(File job) {
        boolean isJar = false;
        try {
            new JarFile(job);
            isJar = true;
        } catch (IOException e) {
            // not a jar;
        }
        return isJar;
    }
}
