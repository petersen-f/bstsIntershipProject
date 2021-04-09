---
jupyter:
  jupytext:
    formats: ipynb,md
    text_representation:
      extension: .md
      format_name: markdown
      format_version: '1.3'
      jupytext_version: 1.10.1
  kernelspec:
    display_name: R
    language: R
    name: ir
---

# Documentation of the development of the bsts JASP module

The following document will document the steps that were necessary to develope a JASP module that mirrors the functionality of the bsts package in R. The aim is to make it easier to understand how to develop a module from scratch.

The first thing I did was reading up upon the [development workflow](https://github.com/jasp-stats/jasp-desktop/blob/development/Docs/development/jasp-module-workflow.md) and the [Guide to adding a module in JASP](https://github.com/jasp-stats/jasp-desktop/blob/development/Docs/development/jasp-adding-module.md). The documentation here does not serve as a replacement but rather as an illustration of which specific steps to take for a final module. 

The development workflow documentation provides us with a link to a nice template which I subsequently downloaded. After extracting the template folder from the .zip file I renamed it from `jaspModuleTemplate` to `jaspBsts` for good measure.

#### **Description.qml**

The [guide](https://github.com/jasp-stats/jasp-desktop/blob/development/Docs/development/jasp-adding-module.md) explains that the first step to developing a module is editing the Description.qml file which is located in the `/inst` folder of the template. It basically tells JASP how to display the display your module in the JASP menu and which analysis function it entails that the user can select from.

As our goal is to implement the main functions of the bsts package a single Analysis function will be enough for now. 

The first part of the qml file describes how the module will be named which in our case is 'BSTS'. 
```
import QtQuick		2.12
import JASP.Module	1.0

Description
{
	name		: "jaspBsts"
	title		: qsTr("BSTS")
	description	: qsTr("This module offers a Bayesian take on linear Gaussian state space models suitable for   time series analysis.")
	icon      : "bsts.svg"
	version		: "0.1"
	author		: "JASP Team"
	maintainer	: "JASP Team <info@jasp-stats.org>"
	website		: "https://jasp-stats.org"
	license		: "GPL (>= 2)"
```    

```R

```
