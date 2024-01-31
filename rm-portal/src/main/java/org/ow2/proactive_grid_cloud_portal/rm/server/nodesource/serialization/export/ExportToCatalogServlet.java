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
package org.ow2.proactive_grid_cloud_portal.rm.server.nodesource.serialization.export;

import static org.ow2.proactive_grid_cloud_portal.rm.shared.CatalogConstants.*;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;

import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.fileupload.FileItem;
import org.ow2.proactive.catalog.client.ApiClient;
import org.ow2.proactive.catalog.client.api.CatalogObjectControllerApi;
import org.ow2.proactive.catalog.client.api.CatalogObjectRevisionControllerApi;
import org.ow2.proactive_grid_cloud_portal.rm.server.ServletRequestTransformer;
import org.ow2.proactive_grid_cloud_portal.rm.server.nodesource.serialization.CatalogObjectAction;
import org.ow2.proactive_grid_cloud_portal.rm.server.nodesource.serialization.CatalogUrlRmServerBuilder;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


/**
 * Servlet invoked when a node source, an infrastructure, or a policy is
 * exported to the catalog. The request content is expected to be in JSON
 * format, containing the configuration of a node source.
 */
public class ExportToCatalogServlet extends HttpServlet {

    private static final Logger LOGGER = LoggerFactory.getLogger(ExportToCatalogServlet.class);

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) {
        upload(request, response);
    }

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) {
        LOGGER.error("Unsupported request: " + request.getRequestURI());
    }

    private void upload(HttpServletRequest request, HttpServletResponse response) {
        response.setContentType("text/html");
        try (CatalogObjectAction catalogObjectAction = buildCatalogObjetAction(request)) {
            ApiClient apiClient = new ApiClient();
            apiClient.setBasePath(new CatalogUrlRmServerBuilder().getCatalogUrl());
            if (catalogObjectAction.isRevised()) {
                CatalogObjectRevisionControllerApi catalogObjectController = new CatalogObjectRevisionControllerApi(apiClient);
                catalogObjectController.create2(catalogObjectAction.getSessionId(),
                                                catalogObjectAction.getBucketName(),
                                                catalogObjectAction.getCatalogObjectName(),
                                                catalogObjectAction.getCommitMessage(),
                                                catalogObjectAction.getCatalogObjectJsonFile(),
                                                catalogObjectAction.getProjectName(),
                                                null);
                LOGGER.info("Post new revision of catalog object " + catalogObjectAction.getCatalogObjectName() +
                            " in bucket " + catalogObjectAction.getBucketName());
            } else {
                CatalogObjectControllerApi catalogObjectController = new CatalogObjectControllerApi(apiClient);
                catalogObjectController.create1(catalogObjectAction.getSessionId(),
                                                catalogObjectAction.getBucketName(),
                                                catalogObjectAction.getKind(),
                                                catalogObjectAction.getCommitMessage(),
                                                catalogObjectAction.getObjectContentType(),
                                                catalogObjectAction.getCatalogObjectJsonFile(),
                                                catalogObjectAction.getCatalogObjectName(),
                                                catalogObjectAction.getProjectName(),
                                                null);
                LOGGER.info("Post new catalog object " + catalogObjectAction.getCatalogObjectName() + " in bucket " +
                            catalogObjectAction.getBucketName());
            }
        } catch (Exception e) {
            logErrorAndWriteResponseToClient(e, response);
        }
    }

    private CatalogObjectAction buildCatalogObjetAction(HttpServletRequest request) throws Exception {
        CatalogObjectAction catalogObjectAction = new CatalogObjectAction();
        for (FileItem formItem : new ServletRequestTransformer().getFormItems(request)) {
            if (formItem.isFormField()) {
                String fieldValue = formItem.getString();
                switch (formItem.getFieldName()) {
                    case SESSION_ID_PARAM:
                        catalogObjectAction.setSessionId(fieldValue);
                        break;
                    case BUCKET_NAME_PARAM:
                        catalogObjectAction.setBucketName(fieldValue);
                        break;
                    case NAME_PARAM:
                        catalogObjectAction.setCatalogObjectName(fieldValue);
                        break;
                    case PROJECT_NAME_PARAM:
                        catalogObjectAction.setProjectName(fieldValue);
                        break;
                    case FILE_CONTENT_PARAM:
                        File catalogObjectJsonFile = File.createTempFile("catalog-object-configuration", ".json");
                        try (PrintWriter writer = new PrintWriter(catalogObjectJsonFile)) {
                            writer.write(fieldValue);
                        }
                        formItem.write(catalogObjectJsonFile);
                        catalogObjectAction.setCatalogObjectJsonFile(catalogObjectJsonFile);
                        break;
                    case KIND_PARAM:
                        catalogObjectAction.setKind(fieldValue);
                        break;
                    case COMMIT_MESSAGE_PARAM:
                        catalogObjectAction.setCommitMessage(fieldValue);
                        break;
                    case OBJECT_CONTENT_TYPE_PARAM:
                        catalogObjectAction.setObjectContentType(fieldValue);
                        break;
                    case REVISED_PARAM:
                        catalogObjectAction.setRevised(Boolean.parseBoolean(fieldValue));
                        break;
                    default:
                }
            }
            formItem.delete();
        }
        return catalogObjectAction;
    }

    private void logErrorAndWriteResponseToClient(Exception e, HttpServletResponse response) {
        try {
            String errorMessage = e.getMessage().replace("<", "&lt;").replace(">", "&gt;");
            LOGGER.warn(EXPORT_FAILED_MESSAGE, e);
            response.getWriter().write(EXPORT_FAILED_MESSAGE + ": " + errorMessage);
        } catch (IOException ioe) {
            LOGGER.warn("Failed to return catalog object export error to client", ioe);
        }
    }

}
