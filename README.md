# Ada CSS Library and tools

[![Build Status](https://img.shields.io/jenkins/s/https/jenkins.vacs.fr/Ada-CSS.svg)](https://jenkins.vacs.fr/job/Ada-CSS/)
[![Test Status](https://img.shields.io/jenkins/t/https/jenkins.vacs.fr/Ada-CSS.svg)](https://jenkins.vacs.fr/job/Ada-CSS/)
[![License](http://img.shields.io/badge/license-APACHE2-blue.svg)](LICENSE)
![Commits](https://img.shields.io/github/commits-since/stcarrez/ada-css/0.2.0.svg)

The Ada CSS library is an Ada 05 implementation of the
[CSS Object Model (CSSOM)](https://www.w3.org/TR/2016/WD-cssom-1-20160317/).
It provides a CSS parser that allows to read stylesheets and have them
represented in memory.  The library provides operations to look at the
CSS rules.

The motivation to have such library is to provide tools that allows to
check the syntax, analyze, optimize and make various reports on CSS files.

To use Ada CSS library, configure as follows:
```
   ./configure
   make
```
For the installation, use the following command:
```
   make install
```
