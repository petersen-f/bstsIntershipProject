import QtQuick		2.12
import JASP.Module	1.0

Description
{
	name		: "jaspBsts"
	title		: qsTr("BSTS")
	description	: qsTr("This module offers a Bayesian take on linear Gaussian state space models suitable for   time series analysis.")
	icon      : "bsts.png"
	version		: "0.1"
	author		: "JASP Team"
	maintainer	: "JASP Team <info@jasp-stats.org>"
	website		: "https://jasp-stats.org"
	license		: "GPL (>= 2)"

	Analysis
	{
	    title: "Bayesian State Space Models"
	    func: "bayesianStateSpace"
		qml: 'bayesianStateSpace.qml'
	}
}
