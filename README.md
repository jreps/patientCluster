patientCluster
======================

Introduction
============
An R package for clustering cohort patients by medical history or creating data-driven medical topics (clusters of concept_ids corresponding to the same medical topic).

There is also a javascript D3 interactive visulisation available for viewing the results.


Features
========
- Aids the creation of topics (sets of similar concept ids)
- Takes a cohort as input and feature definitions (set of individual concept_ids or topics).
- Extracts the necessary data from a database in OMOP Common Data Model format.
- Performs kmeans, generalised low rank models or concensus clustering 
- Includes functions for evaluating clusters and exporting into JSON format

Screenshots
===========
<table border = "">
<tr valign="top">
<td width = 100%>
  <img src="https://github.com/jreps/patientCluster/blob/master/extras/patclustExample.png" alt="Javascript Cluster Screenshot" title="Javascript Cluster Screenshot" />
</td>
</tr><tr>
<td>Example Javascript Plot</td>
</tr>
</table>

Technology
==========
patientCluster is an R package, with some functions implemented using h2o (http://h2o-release.s3.amazonaws.com/h2o/rel-lambert/5/docs-website/Ruser/Rinstall.html).

System Requirements
===================
Requires R (version ? or higher). Installation on Windows requires [RTools](http://cran.r-project.org/bin/windows/Rtools/). Libraries used in patientCluster require Java.

Dependencies
============
 * h2o
 * DatabaseConnector
 * SqlRender

Getting Started
===============
1. On Windows, make sure [RTools](http://cran.r-project.org/bin/windows/Rtools/) is installed.
2. The DatabaseConnector, h2o and SqlRender packages require Java. Java can be downloaded from
<a href="http://www.java.com" target="_blank">http://www.java.com</a>.
3. Install h2o as describe here: http://h2o-release.s3.amazonaws.com/h2o/rel-lambert/5/docs-website/Ruser/Rinstall.html
4. In R, use the following commands to download and install patientCluster:

  ```r
  install.packages("devtools")
  library(devtools)
  install_github("ohdsi/SqlRender") 
  install_github("ohdsi/DatabaseConnector") 
  install_github("ohdsi/patientCluster") 
  
  library("patientCluster")
  library("h2o")
  h2o.init(nthreads = -1, max_mem_size = '16g')
  ```

Getting Involved
================
* Vignette: [Cluster examples](https://github.com/jreps/patientCluster/blob/master/extras/Examples%20patientCluster.pdf)
* Package manual: [patientCluster.pdf](https://github.com/jreps/patientCluster/blob/master/man/patientCluster.pdf) 
* Developer questions/comments/feedback: <a href="http://forums.ohdsi.org/c/developers">OHDSI Forum</a>
* We use the <a href="../../issues">GitHub issue tracker</a> for all bugs/issues/enhancements
 
License
=======
patientCluster is licensed under Apache License 2.0

Development
===========
patientCluster is being developed in R Studio.

Beta

