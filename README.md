# ProActive Workflows & Scheduling

[![Build Status](http://jenkins.activeeon.com/buildStatus/icon?job=scheduling-portal)](http://jenkins.activeeon.com/job/scheduling-portal/)


## Description

This project is a Web front-end for the ProActive Scheduler and Resource Manager
REST API. It does not require the REST API to be built, but will not be able to
properly run without it.

Information about the REST API, the Scheduler and Resource Manager software can
be found at the following locations:

  - Project: http://proactive.inria.fr/
  - SCM: http://gitorious.ow2.org/ow2-proactive
  - Mailing: http://mail.ow2.org/wws/arc/proactive

This project is Open Source and distributed under the terms of the GNU AGPLv3,
a copy of which was included along with this file.


## Building

Building binaries requires to follow what is explained in the README
(https://github.com/ow2-proactive/scheduling#building-from-sources). It will
generate an archive containing binaries (JAR files). However it will not embed
Web portails. The procedure to also embed Web portals is a bit complex. That's
why you can download releases directly from activeeon.com.

Below are explanations to build and include Web portals with a customized version
of Scheduling assuming that the absolute path to the folder where scheduling is
installed is SCHEDULING_HOME:

  - Studio Web Portal: it is pretty easy to embed. You just need to checkout the
  project (https://github.com/ow2-proactive/studio). Supposing the absolute path
  to this folder is STUDIO_HOME, you need to create a symbolic link in
  SCHEDULING_HOME/dist/war/studio that points to STUDIO_HOME/app:

    `$> ln -s STUDIO_HOME/app SCHEDULING_HOME/dist/war/studio`

  - Scheduler and RM portals: Checkout the project scheduling-portal
  (https://github.com/ow2-proactive/scheduling-portal). Execute './gradlew build' (linux/mac) or 'gradlew build' (windows).
  The command will produce two wars that you need to symlink to SCHEDULING_HOME/dist/war:

    `$> ln -s SCHEDULING_PORTAL_HOME/rm-portal/build/libs/rm-portal-6.3.0.war SCHEDULING_HOME/dist/war/rm.war`

    `$> ln -s SCHEDULING_PORTAL_HOME/scheduler-portal/build/libs/scheduler-portal-6.3.0.war SCHEDULING_HOME/dist/war/scheduler.war`

Once you restart the scheduler, you should have the Web interfaces deployed.


## Deploying

Deploying either the Scheduler or Resource Manager web portal requires an
Application Server. For instance, this document will describe the procedure
for Apache Tomcat 6 which can be downloaded here: http://tomcat.apache.org/ .
Other versions of Apache Tomcat, and other Application Servers such as Jetty
(http://jetty.codehaus.org/jetty/) should work similarly.

Follow these steps:

  1. Stop Tomcat if it was running using /bin/shutdown.[sh|bat]
  2. Copy both .war files from dist/ to the webapps/ directory in the Tomcat
  installation directory 3. Unpack both .war files, ie for the rm.war file:

    Unix:

    `$> cd webapps`

    `$> unzip rm.war -d rm`

    Windows:

    `$> cd webapps`

    `$> mkdir rm`

    `$> cd rm`

    `$> "%JAVA_HOME%"\bin\jar -xf ..\rm.war`

  3. Edit the configuration file of each application to specify the URL
    of the REST server that the application will connect to.
      - For the Scheduler, the file is /webapps/scheduler/scheduler.conf,
        and the configuration key "sched.rest.url".
      - For the RM, the file is /webapps/rm/rm.conf, and the configuration
        key "rm.rest.url".

    ie. for the rm: "rm.rest.url = http://my.example.com:8080/rm_rest/"
    This step requires that you run the REST API server somewhere. This
    can be a remote server, but the REST API server may as well run in the
    same application server as the Web Portal.

  4. Start the Tomcat server using /bin/startup.[sh|bat]
  5. Check the logs in /logs/catalina.out. If the logs show a security exception,
   ie.:

    `java.security.AccessControlException: access denied`

  Kill tomcat, and restart it using

  `$> /bin/startup.[sh|bat] -security`

  to use a security manager.


## Architecture

This section describes briefly the project's architecture.
If you do not wish to understand the internals of the project, read or edit its
sourcecode, then this section is of no use to you.

Here is how the applications tiers interact with each others:

      .------.      .-----------.      |Comm layer + Platform |
      |  RM  +------. Scheduler |      +-----------+----------+
      `--+---'      `-----+-----'      |           | Java     |
         |                |            |           |          |
         |                |            |Java RPC   |          |
    .----+----.     .-----+------.     |           |          |
    | RM REST |     | Sched REST |     |           |          |
    `----+----'     `-----+------'     |           | Tomcat   |
         |                |            |           |          |
         |                |            |Java RPC   |          |
    .----+-----.   .------+-------.    |           |          |
    | RM Portal|   | Sched Portal |    |           |          |
    `----+-----'   `------+-------'    |           | Tomcat   |
         |................|            |           |          |
                 |                     |           |          |
           .-----+-------.             |HTTP(S)    |          |
           | Web Browser |             |           |          |
           `-------------'             |           | Any web  |
                                       |           | browser  |

In the above diagram:

  - The end user uses a Web Browser to connect to the Portal. The Portal
    displays information retrieved from the REST API through an HTTP connection.
  - The REST API retrieves information from the Scheduler or RM server using
    native ProActive Java RPC communications, and stores it locally.
    This has two effects:

    - the REST server acts as a caching layer, preventing the scheduler from
    suffering from the load of too many connected clients.
    - clients can connect through the REST API without using Java or
    ProActive and only through a simple HTTP client.

  - The Scheduler handles the job execution workflow. It is the central piece
    of the application.
  - The Resource Manager aggregates physical resources and provides them
    to the Scheduler so that it may execute tasks.


The RM and Scheduler applications are very close. They are built upon the same
architecture, use the same technologies, and even share some of the same code.

This simplified architecture diagram applies to both client-side applications:

                .------------.
                |AsyncService|
                `-----^------'
                      |network comm
                      |
                .-----v----.                .---------.
                |Controller+---------------->ModelImpl|
                `-----+----'       writes   `--+------'
         +------------+-------+                |Implements
         |!logged    XOR      |logged          |
    .----v----.            .--v-.              |  .-----.
    |LoginPage|            |Page|              +-->Model|
    `---------'            `--+-'              |  `-----'
                     +--------+-----+          |
                     |  includes    |          |  .---------------.
                  .--v--.        .--v--.       +-->EventDispatcher|
                  |View1|        |View2|          `---+-----------'
                  `--+--'        `--+--'              |
                     |              |                 |
                     |implements    |                 |
                .----v----.     .---v-----.           |
                |Listener1|     |Listener2|           |
                `----^----'     `---^-----'           |
                     +--------------+-----------------+
                                            onEvent

This client side application is written in Java and compiled to Javascript
using GWT which will allow execution in a web browser environment.
The server side application (AsyncService on the diagram) runs in native Java
in an application server, and communicates with client-initiated Ajax calls.
The server consists in a collection of servlets that communicate with the REST
API using an HTTP client.


## Troubleshooting

If you have questions regarding this document or the project, you can use the
ProActive mailing list.

If you have found a bug, you can check the project's bug tracker to either
search for open entries, or submit a new reproducible issue.

  - Mailing: http://mail.ow2.org/wws/arc/proactive
  - Tracker: https://bugs.activeeon.com/browse/PORTAL
