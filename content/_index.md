---
title: "EES 5891-03: Bayesian Statistical Methods"
description: "An introduction to Bayesian statistics, with a focus on both practical application of Bayesian regression methods to data as well as philosophical background on statistical inference and interpretation of statistical analyses. Bayes's theorem and tools for applying it, including quadratic approximations, Hamiltonian Monte Carlo sampling, and Integrated Nested Laplace Approximation (INLA). Advanced methods, including multilevel regression methods and geospatial analysis."
---


<center style="font-size:150%;line-height:150%;">
{{% md %}}
|             |     |                                                                     |
|:-----------:|:--:|:-------------------------------------------------------------------:|
| Instructor | <span style="margin-left:20px;">&nbsp;</span> | [Jonathan Gilligan](http://www.jonathangilligan.org) {{% email "jonathan.gilligan@vanderbilt.edu" %}} {{% twitter_link "jg_environ" %}} {{% github_link "jonathan-g" %}} |
| Classroom   |     | {{% CLASSROOM %}}   |   
| Class Times      |     | **{{% CLASS_TIMES %}}** |
| <hr style="border-top-color:black;"/> | <hr style="border-top-color:black;"/> | <hr style="border-top-color:black;"/> |
| Office Hours |   | <span style="font-size:75%;line-height:50%;"> {{% OFFICE_HOURS %}}<br/>{{% OFFICE_LOCATION %}}</span> |
{{% /md %}}
</center>

## Overview

The class will begin with an introduction to Bayesian statistics and then focus 
on practical application of regression methods to data. We will use R together 
with the 
[Stan software package](https://mc-stan.org) for Hamiltonian Monte Carlo methods
and the [R-INLA software package](https://www.r-inla.org/) 
for Integrated Nested Laplace Approximation (INLA) analysis 
(https://www.r-inla.org/). 
The course will combine practical applications of Bayesian methods to real 
(often messy) data with more philosophical discussions of Bayesian approaches 
to statistics and how to interpret results of statistical analyses. 
We will focus on regression methods, including hierarchical or 
multilevel regression modeling methods, which can be very powerful when you 
have data that has a nested structure (e.g., cities and counties within states 
or species within genera). Students will do projects applying Bayesian methods 
to their own data sets.

## Textbooks

We will use two required textbooks and two optional ones:

### Required Books

* {{% LONG_RETHINKING %}}
* {{% LONG_INLA %}} (there is a [free e-book version]({{% INLA_URL %}}) 
  available at <{{% INLA_URL %}}>).

## Optional Books

* {{% LONG_DBDA %}}
* {{% LONG_WICKHAM %}} (there is a [free e-book version]({{% WICKHAM_URL %}})
  available at <{{% WICKHAM_URL %}}>)
