import QtQuick
import JASP.Module

Description
{

	title		: qsTr("Predictive Analytics")
	description	: qsTr("This module offers time series predictions and combines them with quality control concepts. That way one can predict whether a process will exeed a specified boundary in the future.")
	icon: 			"icon.png"
	hasWrappers: 	false
	
	Analysis
	{
	    title: "Predictive Analytics"
	    func: "predictiveAnalytics"
		qml: 'predictiveAnalytics.qml'
	}

	//Analysis
	//{
	//    title: "Multivariate Binomial Control"
	//    func: "multiVarControl"
	//	qml: 'multiVarControl.qml'
	//}
}
