---
title: "Course Description"
date: "2021-08-26T00:00:00"
---
#### <a href="/files/EES_5891-03_Syllabus.pdf" target="_blank"><i class="fa fa-file-pdf-o" style="margin-right:0.25em;"></i> **Printable syllabus**</a>

# Contents

* [Overview](#desc)
  * [Catalog description](#catalog-desc)
  * [Prerequisites](#prereqs)
  * [Narrative description](#narrative-desc)
  * [Goals for the course](#goals)
* [Course Structure](#structure)
  * [Reading material](#reading)
  * [Graded assignments](#graded-work)
    * [Homework](#homework)
    * [Research project](#project)
    * [Basis of semester grade](#grades)


# **Overview of the Course** {#desc}

## Basic Info: {#basic-info}

#### Professor {#prof}

{{< PROF_INFO >}}

{{< IF_TA >}}
#### Graduate Teaching Assistant

{{< TA_INFO >}}
{{< /IF_TA >}}

#### Schedule {#schedule}

{{% CLASS_MEETINGS %}}

## Catalog Description {#catalog-desc}

**EES 5891-03 Bayesian Statistical Methods**
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

### Prerequisites {#prereqs}

You should be comfortable with differential and integral calculus and have 
some previous experience with standard statistics. 

This course will be very mathematical and will make extensive use of the R
software system, but I do not assume that you already know R or advanced 
mathematics beyond calculus.

## Narrative Description {#narrative-desc}

Bayesian statistics is a branch of statistics that has been around for almost
300 years, but for most of that time, it was very difficult to apply to 
practical problems because the mathematical equations were too difficult to
solve. In the last 30 years, as computers have become much faster and more 
powerful, new computational methods have emerged that make Bayesian statistics
practical for research and applications. 

Bayesian analysis is widely used across a wide variety of research as well as
practical applications. It is used to analyze results from high-energy 
particle physics experiments to discover new subatomic particles. 
There are many other applications in a wide variety of domains.
It's used by geologists to improve estimates of mineral distributions and
radon hazards. 
It's used by biologists to identify and categorize variations in the genomes of 
humans and other species. 
It's used extensively in medicine to analyze the results of clinical trials, 
to determine the pharmacokinetics of drug metabolism, and to assess the 
predictive value of tests for diseases such as cancer or COVID infection.
It's used in political science and sociology to improve the 
accuracy of public opinion surveys and to understand patterns of voting.
It's widely used in marketing to identify consumer preferences and 
improve the effectiveness of advertising. 
If you use Google, Amazon, 
Netflix, Stitchfix, or practically any large online platform for shopping or
entertainment, advanced Bayesian methods form the basis of their 
recommendations. 
Bayesian analysis has also been applied effectively to law and criminology to 
assess the value of evidence in proving guilt or innocence. 
It has been applied to public health to estimate the prevalence of dieseases 
and tomake more effective treatment decisions when medical tests are uncertain. 
It is widely used in meteorology to make weather forecasts and in 
climate science to combine data from many different sources and come up with 
quantitative predictions and detailed understanding of their associated 
uncertainties. 
Bayesian methods are also widely used in computational applications, such as 
image analysis and reconstruction, computational text analysis, and natural 
language processing. 
One of the earliest practical applications of Bayesian textual analysis, 
in 1964, identified the anonymous authors of the Federalist Papers.
More recent applications of Bayesian textual analysis are used to separate
desired email from spam.

Bayesian statistical methods are valuable because they provide a systematic
way to combine what you already know about a problem with new data from 
experiments or observations, and the results of Bayesian analyses are more 
straightforward to interpret than conventional statistics.

This course will provide a general introduction to Bayesian statistics and 
will combine practical instruction in how to do Bayesian data analysis and 
philosophical discussions about how to think about the assumptions that go into
a Bayesian analysis and how to interpret the results that it produces.

You do not need to have any prior knowledge of computer programming, but I do 
expect that you are familiar with basic statistics and calculus 
(both derivatives and integrals).

## Goals for the Course {#goals}

By the end of the semester, you will:

* Understand Bayes's theorem and how to apply it.
* Understand problems with the traditional statistical emphasis on
  null-hypothesis significance testing (NHST), why Bayesian approaches to
  NHST don't solve these problems, and how Bayeian statistics offers
  superior alternatives to NHST.
* Understand how think about statistical models, how to choose an
  appropriate model for your problems, and understand the tradeoffs between
  different kinds of models.
* Be able to design and conduct a comprehensive Bayesian analysis of data
  from start to finish.
* Understand how to choose appropriate priors for your Bayesian analyses
  and how to test whether your choice of priors is sound.
* Understand how to set up, perform, assess the validity of, and
  interpret the results of Bayesian regression analysis.
* Understand why Markov Chain Monte Carlo (MCMC) sampling is used in Bayesian
  analysis, what the limits of MCMC are, and how to test your MCMC analyses
  for validity.
* Understand and be able to perform analyses using more complex
  statistical models, such as interaction models, generalized linear models,
  models of discrete (categorical and count) data.
* Understand what multilevel or hierarchical models are, when to use them,
  and how to interpret the results of a multilevel analysis.
* Understand the Integrated Nested Laplace Approximation (INLA),
  why you might use INLA instead of MCMC analysis, and what the limits of INLA
  analysis are.
* Understand several types of Bayesian geospatial analysis, including
  Matern covariance models and conditional autoregressive (CAR) models.



# **Structure of the Course:** {#structure}

I divide the semester into three parts:

* **Introduction to Bayes's Theorem and its Applications:**
  The first part of the course introduces the basic concepts of Bayesian 
  statistics, using simplified approximations to calculate difficult 
  equations. This section will focus on linear regression methods.

* **Monte Carlo Methods:** 
  Next, we study Monte Carlo methods, which help us solve more difficult 
  problems that our earlier approximations are not powerful enough for.
  This section will introduce statistical models of discrete data
  (counts, categories, etc.), and generalized linear models. It will conclude
  with multilevel statistical models, which can be very powerful methods for
  working with large and complex data sets.

* **Geospatial Modeling:**
  Finally, we will learn a different approach, called the Integrated 
  Nested Laplace Approximation (INLA), which is very well suited for 
  analyzing geospatial data that may be too difficult to analyze uding 
  Monte Carlo methods. 


## Reading Material {#reading}

There are two required textbooks and two optional textbooks:

### Textbooks

1. {{% LONG_RETHINKING %}}

   This will be the main textbook for most of the semester.

   There is a [companion web site]({{% RETHINKING_COMPANION_URL %}}) to the book, 
   and the author has posted 
   [videos of his lectures]({{% RETHINKING_LECTURES %}})
   on YouTube.
  
   McElreath uses basic R, which is fine, but many people have learned to use
   a more modern dialect of R called the "tidyverse," which is described
   at length in our companion book, [{{% MEDIUM_WICKHAM %}}]({{% WICKHAM_URL %}}).
   For people who are used to the Tidyverse, there is an online e-book
   * {{% LONG_TIDY_RETHINKING %}},


   that translates McElreath's R code into the _tidyverse_, dialect.
   If you want to use this, you will read the text in McElreath's book, 
   but use the code from Kurz's e-book.

2. {{% LONG_INLA %}}

   We will only use this book for a few weeks in the third part of the semester,
   when we are studying geospatial methods.

   The book is fairly expensive, but there is a free 
   [online e-book version]({{% INLA_URL %}})
   of this book that you can use, if you don't want to buy it.
  
   We will use this book in the third part of the semester, when we study 
   Bayesian geospatial methods. This book covers a method called the 
   Integrated Nested Laplace Approximation, which is very powerful for 
   statistical models that are too computationally intensive for Monte Carlo
   methods.
  
#### Optional Textbooks
  
There are two optional textbooks that you don't need to buy, but which may 
be very useful as companions to the assigned textbooks. 
  
3. {{% LONG_DBDA %}}

   This is an excellent introduction to Bayesian data analysis for beginners.
   It is gentler than {{% MEDIUM_RETHINKING %}}, and would be better suited for
   an undergraduate course, but I decided not to use it as the main textbook 
   for this class because it focuses more on the statistical methods and does 
   not give as much application of them to real scientific problems.
  
   The author writes very clearly and this book may be helpful if you find
   some of the material in {{% MEDIUM_RETHINKING %}} confusing.
   I have asked the Science and Engineering Library to put a copy on reserve so
   you will be able to access it without buying a copy.

4. {{% LONG_WICKHAM %}}

   This book is the best practical introduction I have found for getting started
   in R and getting things done in data analysis. The author is the chief data
   scientist as the RStudio company and wrote a huge number of widely used free
   packages to extend and enrich R. This book follows his philosophy of how to 
   organize data sensibly for analyzing and presenting it.

## Additional Resources

This course only scratches the surface of what is possible with Bayesian 
statistics. I have prepared a handout with a lot of additional recommendations
for reading about these powerful methods and how to use them.

## Class Web Site

In addition to {{% BRIGHTSPACE %}}, I have set up a companion web site for this
course at
<https://ees5891.jgilligan.org>,
where I post the reading and homework assignments,
my slides from class, and other useful material. That web site will be the
central place to keep up with material for the course during the semester.
This web site will direct you to Brightspace if there is anything you need to
find there.


## Computer Software {#software}

For this class, we will work in R, and I strongly recommend that you install
the free version of RStudio Desktop for working with R. All the software we will
use this semester is free and can be downloaded and installed on Windows,
Mac, and Linux systems. You can find details at the 
[tools page]({{% TOOLS_PAGE_URL %}}) of the 
{{% CLASS_WEB_SITE %}}

# **Assignments** {#assignments}
 
### Overview of reading assignments {#reading-asgts}

I will give out detailed reading that give specific pages to read for each class 
and notes on important things you should understand.
**I expect you to complete the reading before you come to class on the day 
for which the reading is assigned**, 
so you can participate in discussions of the 
assigned material and ask questions if there are things you don't understand.

## **Graded Work** {#graded-work}

### Homework {#homework}

Homework must be turned in at the beginning of class on the day it's due.

### Project {#project}

In the second half of the semester, you will do a research project, in which you
will choose a data set that's interesting to you and apply Bayesian methods to 
analyze it. You will present the results of your project in class during the
last week of the semester and turn in a written report about your project.

### Tests and Examinations {#tests}

There will not be any tests or examinations in this course. Your grade will be 
based on class participation, homework, modeling projects, and in-class 
presentations.

## **Basis for Grading** {#grades}

|                            |       |      |
|:--------------------------:|------:|-----:|
| Class participation        |  5%   |      |
| Homework                   | 45%   |      |
| Research Project           | 50%   |      |


# **Final Note:**

I have made every effort to plan a busy, exciting, and instructive semester.
I may find during the term that I need to revise the syllabus to give more time
to some subjects or to pass more quickly over others rather than covering them
in depth.
Thus, while I will attempt to follow this syllabus as closely as I can,
you should realize that it is subject to change during the semester.
