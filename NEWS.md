TriMatch 1.0.0
=========

* Fix documentation issue with cross referencing functions per CRAN checks.

TriMatch 0.9.9
=========

* Fixed issue where as.data.frame.list would cause a conflict with the base function of the same 
  name. A new function, data.frame.to.list, wraps the non-exported version of as.data.frame.list.

TriMatch 0.9.8
=========

* Added tmatch parameter to the multibalance.plot function that will limit the balance estimates to 
  only the units that have been matched.
* Added method parameter to trips function and implemented random forests as an alternative method
  for calculating propensity scores.

TriMatch 0.9.7
=========

* Update for new version of ggplot2

TriMatch 0.9.6
=========

* Fixed bug with the triangle plot. Exported utility functions to be visible.

TriMatch 0.9.5
=========

* Updates for version 2.0 of ggplot2.

TriMatch 0.9.4
=========

* Fixes issues to pass current CRAN checks.

TriMatch 0.9.3
=========

* Added new one-to-one-to-one demo to show how to post process the resutls of trimatch.

TriMatch 0.9.2
=========

* Fix a bug in OneToN function that would return duplicate treatment 2 units when
  M1=1 and M2=1.

TriMatch 0.9.1
=========

* Fixes to pass new warnings from R CMD check.

TriMatch 0.9
=========

* Initial version of the TriMtach package release on July 5, 2013.
* Project is hosted on Github. More information at http://jbryer.github.com/TriMatch.
* See the package vignette for an introduction as well as the demos.
