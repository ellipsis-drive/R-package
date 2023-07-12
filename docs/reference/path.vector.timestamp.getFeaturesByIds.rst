=================================================================================================================
Get features of a vector timestamp by the id of the features — path.vector.timestamp.getFeaturesByIds • EDPackage
=================================================================================================================

.. raw:: html

   <div class="container template-reference-topic">

.. raw:: html

   <div class="navbar navbar-default navbar-fixed-top"
   role="navigation">

.. raw:: html

   <div class="container">

.. raw:: html

   <div class="navbar-header">

Toggle navigation
`EDPackage <../index.html>`__\ 1.0.0

.. raw:: html

   </div>

.. raw:: html

   <div id="navbar" class="navbar-collapse collapse">

-  `Reference <../reference/index.html>`__

.. raw:: html

   </div>

.. raw:: html

   </div>

.. raw:: html

   </div>

.. raw:: html

   <div class="row">

.. raw:: html

   <div class="col-md-9 contents">

.. raw:: html

   <div class="page-header">

.. rubric:: Get features of a vector timestamp by the id of the features
   :name: get-features-of-a-vector-timestamp-by-the-id-of-the-features

.. raw:: html

   <div class="hidden name">

``path.vector.timestamp.getFeaturesByIds.Rd``

.. raw:: html

   </div>

.. raw:: html

   </div>

.. raw:: html

   <div class="ref-description">

Get features of a vector timestamp by the id of the features

.. raw:: html

   </div>

.. raw:: html

   <div id="ref-usage">

.. raw:: html

   <div class="sourceCode">

.. code:: r

   path.vector.timestamp.getFeaturesByIds(
     pathId,
     timestampId,
     featureIds,
     token = NULL,
     showProgress = FALSE
   )

.. raw:: html

   </div>

.. raw:: html

   </div>

.. raw:: html

   <div id="arguments">

.. rubric:: Arguments
   :name: arguments

pathId
   Mandatory (uuid)

timestampId
   Mandatory (uuid)

featureIds
   Mandatory (array of uuids)

token
   Mandatory (string)

showProgress
   Mandatory (logical)

.. raw:: html

   </div>

.. raw:: html

   <div id="value">

.. rubric:: Value
   :name: value

a simple features 'sf' object containing the features as a geometry

.. raw:: html

   </div>

.. raw:: html

   </div>

.. raw:: html

   <div id="pkgdown-sidebar" class="col-md-3 hidden-xs hidden-sm">

.. rubric:: Contents
   :name: contents

.. raw:: html

   </div>

.. raw:: html

   </div>

.. raw:: html

   <div class="copyright">

Developed by berendstarkenburg@gmail.com.

.. raw:: html

   </div>

.. raw:: html

   <div class="pkgdown">

Site built with `pkgdown <https://pkgdown.r-lib.org/>`__ 2.0.7.

.. raw:: html

   </div>

.. raw:: html

   </div>
