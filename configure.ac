dnl Autoconf configure script for Ada CSS Library
dnl Copyright (C) 2017, 2018, 2020, 2023 Stephane Carrez
dnl Written by Stephane Carrez (Stephane.Carrez@gmail.com)
dnl
dnl  Licensed under the Apache License, Version 2.0 (the "License");
dnl  you may not use this file except in compliance with the License.
dnl  You may obtain a copy of the License at
dnl
dnl     http://www.apache.org/licenses/LICENSE-2.0
dnl
dnl  Unless required by applicable law or agreed to in writing, software
dnl  distributed under the License is distributed on an "AS IS" BASIS,
dnl  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
dnl  See the License for the specific language governing permissions and
dnl  limitations under the License.
dnl 
dnl Process this file with autoconf to produce a configure script.

AC_INIT(src/css.ads)

# Current release settings
CSS_MAJOR_VERSION=0
CSS_MINOR_VERSION=4
CSS_MICRO_VERSION=0
CSS_VERSION=$CSS_MAJOR_VERSION.$CSS_MINOR_VERSION.$CSS_MICRO_VERSION

# Set the version number of Ada Server Faces
AC_SUBST(CSS_VERSION)
AC_SUBST(CSS_MAJOR_VERSION)
AC_SUBST(CSS_MINOR_VERSION)
AC_SUBST(CSS_MICRO_VERSION)

AM_GNAT_LIBRARY_PROJECT(cssada)
AM_GNAT_FIND_ADA_UTIL([Ada CSS])

AC_OUTPUT(
Makefile.conf cssada.gpr cssada_tests.gpr cssada_tools.gpr
)
