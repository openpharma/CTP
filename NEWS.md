# CTP 3.0.1

## Minor change

* Package `rmarkdown` added to suggest field in the `DESCRIPTION` file.

# CTP 3.0

## Bug fixes

* In some cases of hypothesis structures, the testing set of some elementary was not complete and some adjusted p-values were not correct. However, the hypothesis trees as constructed by the function `IntersectHypotheses`were correct. This bug is fixed now.

## Major changes

* In `AnalyseCTP()`, the tests for linear and generalised linear models are now calculated by means of the functions `test()` and `contrast()` from the package `emmeans`.

* The new exported function `TestingSet()` provides the testing set for a specific elementary hypothesis.

## Minor changes

* Adjusted p-values are only calculated and displayed for the *elementary* hypotheses. 

* In `summary(ctp.struct)`, a message from `dplyr` is eliminated.

* In the vignette **Closed Testing Procedure**, new examples are given.

* Updated documentation.

