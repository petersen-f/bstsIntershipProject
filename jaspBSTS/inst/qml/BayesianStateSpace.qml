import QtQuick 2.11
import QtQuick.Layouts 1.3
import JASP.Controls 1.0
import JASP.Widgets 1.0

Form
{

	VariablesForm
	{
		AvailableVariablesList	{ name: "allVariablesList" }
		AssignedVariablesList	{ name: "dependent";	title: qsTr("Dependent Variable");		suggestedColumns: ["scale"];	singleVariable: true	}
		AssignedVariablesList	{ name: "covariates";	title: qsTr("Covariates");				    suggestedColumns: ["scale"];	allowedColumns: ["scale"]}
	}




	Section
	{
		title: qsTr("Model Components")
		//columns: 2
		CheckBox
		{
			name: "checkboxAr"
			label: qsTr("Add Autoregressive Process")
			checked: false
			id: checkAr
			Layout.columnSpan: 2

			columns: 2

			RadioButtonGroup
			{
				columns: 2
				enabled: checkAr.checked
				name: "lagSelectionMethod"
				title: qsTr("Lag Selection Method")
				RadioButton
				{
					value: "manualAR"; label: qsTr("Manually"); checked: true
					//columns: 1
					IntegerField
					{
						name: "noLags"
						label: qsTr("No. of lags")
						fieldWidth: 40
						defaultValue: 1
					 	}
				}
				RadioButton
				{
					value: "autoAR"; label: qsTr("Automatic"); checked: true
					columns: 1
					DoubleField { name: "maxNoLags";	label: qsTr("Maximal lags");	fieldWidth: 40; defaultValue: 1;}
				}
			}

			CheckBox
			{
				name: 'arSdPrior'
				enabled: checkAr.checked
				label: qsTr(' Custom Stand. Dev. Prior') //not sure about the name as it is actually an inverse Gamma prior

				DoubleField { name:'arSigmaGuess';		label: "σ guess";	fieldWidth: 40;}
				DoubleField { name:'arSigmaWeight';		label: "Weight";	fieldWidth: 40;}
			}




		}

		CheckBox
		{
			name: "checkboxLocalLevel"
			label: qsTr("Add Local Level Component")
			id: checkLocalLevel
			checked: false
			Layout.columnSpan: 2
			CheckBox
			{
				name: 'localLevelSdPrior'
				enabled: checkLocalLevel.checked
				label: qsTr(' Custom random walk SD prior') //not sure about the name as it is actually an inverse Gamma prior

				DoubleField { name:'localLevelSigmaGuess';		label: "σ guess";	fieldWidth: 40;}
				DoubleField { name:'localLevelSigmaWeight';		label: "Weight";	fieldWidth: 40;}
			}
		}
		//Local Linear Trend
		// abbreviated as Llt for priors
		CheckBox
		{
			name: "checkboxLocalLinearTrend"
			label: qsTr("Add Local Linear Trend Component")
			id: checkLocalLinearTrend
			checked: false
			columns: 2
			//Layout.columnSpan: 2
			CheckBox
			{
				name: 'lltLevelPrior'
				enabled: checkLocalLinearTrend.checked
				label: qsTr(' Custom level SD prior')

				DoubleField { name:'lltLevelSigmaGuess';		label: "σ guess";	fieldWidth: 40;}
				DoubleField { name:'lltLevelSigmaWeight';		label: "Weight";	fieldWidth: 40;}
			}
			CheckBox
			{
				name: 'lltSlopePrior'
				enabled: checkLocalLinearTrend.checked
				label: qsTr(' Custom slope SD prior')

				DoubleField { name:'lltSlopeSigmaGuess';		label: "σ guess";	fieldWidth: 40;}
				DoubleField { name:'lltSlopeSigmaWeight';		label: "Weight";	fieldWidth: 40;}
			}
		}
		//Dynamic Regression Component
		CheckBox
		{
			name: "checkboxDynReg"
			label: qsTr("Add Dynamic Regression Component")
			checked: false
			id: checkDynReg
			Layout.columnSpan: 2

			columns: 2
		}
	}
	Section

	{
		title: qsTr('Plots')

	}
	Section
	{
		title: qsTr('Prediction')
	}
}
