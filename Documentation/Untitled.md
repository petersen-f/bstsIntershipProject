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

The first part of the qml file describes the title and name of the module as well as a small description including the name of the file for the icon.  
  


```
import QtQuick		2.12
import JASP.Module	1.0

Description
{
	name		: "jaspBsts"
	title		: qsTr("BSTS")
	description	: qsTr("This module offers a Bayesian take on linear Gaussian state space models suitable for time series analysis.")
	icon        : "bsts.svg"
	version		: "0.1"
	author		: "JASP Team"
	maintainer	: "JASP Team <info@jasp-stats.org>"
	website		: "https://jasp-stats.org"
	license		: "GPL (>= 2)"
```


One **important** thing to note is that the `name:` argument needs to match the one mentioned in the `Description` file that exist one level above. If that doesn't happen the module will not (re)load as the folder name that is produced doesn't match it's actual destination. 

After having specified all these things we need to include an Analysis section in the Description section. And while it is possible to include seperators and multiple functions we only need a single analysis for now. More specifically we have to specify how it is called and the R funtion and qml file that includes the interface and actual analysis. If our corresponding qml file doesn't have the `func` + .qml as a name we have to specify its name manually via the `qml:` argument.


```
	Analysis
	{
	    title: "Bayesian State Space Models"
	    func:   "BayesianStateSpace"
	}
} # ending brakets for the Description object
```


#### **BayesianStateSpace.qml**

After double checking that the module is correctly display in JASP and can be selected let's start building a mock interface!
For this we need to go into the `/inst/qml` folder, create a text file and replace the ending with .qml. As I didn't specify a specific qml file name in my Description.qml file, it needs to match the `func` name plus an .qml ending: `BayesianStateSpace.qml
`

