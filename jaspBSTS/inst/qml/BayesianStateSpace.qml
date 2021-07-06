import QtQuick 2.11
import QtQuick.Layouts 1.3
import JASP.Controls 1.0
import JASP.Widgets 1.0

Form
{

	VariablesForm
	{

		AvailableVariablesList	{ name: "allVariablesList" }
		AssignedVariablesList	{ name: "dependent";	title: qsTr("Dependent Variable");	suggestedColumns: ["scale"];	singleVariable: true}
		AssignedVariablesList	{ name: "covariates";	title: qsTr("Covariates");			suggestedColumns: ["scale"];	allowedColumns: ["scale"]	}
		AssignedVariablesList 	{ name: "factors";		title: qsTr("Factors");				allowedColumns: ["ordinal", "nominal", "nominalText"]		}
		//AssignedVariablesList	{ name: "time";			title: qsTr("Time");				suggestedColumns: ["nominal"];	singleVariable: true		}
	}

	columns: 2

	Group
	{
		title: qsTr("Output")
		columns: 1
		CheckBox{ name: "postSummaryTable"; label: qsTr("Posterior summary of coefficients"); id: postSummaryTable }
		CIField
		{
			name: "posteriorSummaryCoefCredibleIntervalValue"
			label: qsTr("Credible interval")
			enabled: postSummaryTable.checked
		}

	}
	Group
	{
		DoubleField
		{
			name: "expectedModelSize"
			label: "Expected predictors"
			defaultValue: 1
		}
	}

	Section
	{

		title: qsTr("Model Components")

		//columns: 2
		VariablesForm
		{
			preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
			AvailableVariablesList
			{
				name: "availableTerm"
				title: qsTr("Components")
				width: parent.width / 4
				source: ['covariates', 'factors']
			}
			ModelTermsList {name: "modelTerms";width: parent.width * 5 / 9}
		}
		CheckBox
		{
			name: "checkboxAr"
			label: qsTr("Add autoregressive component")
			checked: false
			id: checkAr
			Layout.columnSpan: 2

			columns: 2

			RadioButtonGroup
			{
				columns: 1
				enabled: checkAr.checked
				name: "lagSelectionMethod"
				title: qsTr("Lag selection method")
				radioButtonsOnSameRow: TRUE
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
					value: "autoAR"; label: qsTr("Automatic")
					columns: 1
					DoubleField { name: "maxNoLags";	label: qsTr("Maximal lags");	fieldWidth: 40; 	defaultValue: 1;}
				}
			}

			//CheckBox
			//{
			//	name: 'arSdPrior'
			//	enabled: checkAr.checked
			//	label: qsTr(' Custom Stand. Dev. Prior') //not sure about the name as it is actually an 	inverse Gamma prior

			//	DoubleField { name:'arSigmaGuess';		label: "σ guess";	fieldWidth: 40;}
			//	DoubleField { name:'arSigmaWeight';		label: "Weight";	fieldWidth: 40;}
			//}
		}

		CheckBox
		{
			name: "checkboxLocalLevel"
			label: qsTr("Add local level component")
			id: checkLocalLevel
			checked: false
			Layout.columnSpan: 2
			//CheckBox
			//{
			//	name: 'localLevelSdPrior'
			//	enabled: checkLocalLevel.checked
			//	label: qsTr(' Custom random walk SD prior') //not sure about the name as it is actually an 	inverse Gamma prior

			//	DoubleField { name:'localLevelSigmaGuess';		label: "σ guess";	fieldWidth: 40;}
			//	DoubleField { name:'localLevelSigmaWeight';		label: "Weight";	fieldWidth: 40;}
			//}
		}
		//Local Linear Trend
		// abbreviated as Llt for priors
		CheckBox
		{
			name: "checkboxLocalLinearTrend"
			label: qsTr("Add local linear trend component")
			id: checkLocalLinearTrend
			checked: false
			columns: 2
			//Layout.columnSpan: 2
			//CheckBox
			//{
			//	name: 'lltLevelPrior'
			//	enabled: checkLocalLinearTrend.checked
			//	label: qsTr(' Custom level SD prior')

			//	DoubleField { name:'lltLevelSigmaGuess';		label: "σ guess";	fieldWidth: 40;}
			//	DoubleField { name:'lltLevelSigmaWeight';		label: "Weight";	fieldWidth: 40;}
			//}
			//CheckBox
			//{
			//	name: 'lltSlopePrior'
			//	enabled: checkLocalLinearTrend.checked
			//	label: qsTr(' Custom slope SD prior')

			//	DoubleField { name:'lltSlopeSigmaGuess';		label: "σ guess";	fieldWidth: 40;}
			//	DoubleField { name:'lltSlopeSigmaWeight';		label: "Weight";	fieldWidth: 40;}
			//}
		}
		//Dynamic Regression Component
		CheckBox
		{
			name: "checkboxDynReg"
			label: qsTr("Add dynamic regression component")
			checked: false
			id: checkDynReg
			Layout.columnSpan: 2

			columns: 2
			DoubleField { name:'DynRegLags';		label: "Lag of coefficients";	fieldWidth: 40;}
		}

		Group
		{
			title: qsTr("Seasonalities")

			ColumnLayout
			{
				spacing: 0 * preferencesModel.uiScale

				RowLayout
				{
					Label { text: qsTr("Name"); Layout.preferredWidth: 80 * preferencesModel.uiScale							}
					Label { text: qsTr("Number"); Layout.preferredWidth: 45 * preferencesModel.uiScale				}
					Label { text: qsTr("Duration"); Layout.preferredWidth: 45 * preferencesModel.uiScale				}
					Label { text: qsTr("Inverse gamma prior σ² \n      σ²     sample size"); Layout.preferredWidth: 140 * preferencesModel.uiScale			}
					Label { text: qsTr("Normal prior initial state  \n      μ           σ²"); Layout.preferredWidth: 140 * preferencesModel.uiScale			}
				}

				ComponentsList
				{
					name: "seasonalities"
					rowComponent: RowLayout
					{
						Row
						{
							Layout.preferredWidth: 80 * preferencesModel.uiScale
							spacing: 4 * preferencesModel.uiScale

							TextField
							{
								name: "name"
								fieldWidth: 80 * preferencesModel.uiScale
								placeholderText: "Yearly"
							}
						}
						Row
						{
							Layout.preferredWidth: 45 * preferencesModel.uiScale
							spacing: 4 * preferencesModel.uiScale

							DoubleField
							{
								name: "nSeason"
								defaultValue: 2
								min: 2
							}
						}
						Row
						{
							Layout.preferredWidth: 45 * preferencesModel.uiScale
							spacing: 4 * preferencesModel.uiScale

							DoubleField
							{
								name: "seasonDuration"
								defaultValue: 1
							}
						}
						Row
						{
							Layout.preferredWidth: 140 * preferencesModel.uiScale
							spacing: 4 * preferencesModel.uiScale

							TextField
							{
								name: "sigma.guess"
								fieldWidth: 60 * preferencesModel.uiScale
								placeholderText: ".01 * sdy"

							}
							DoubleField
							{
								name: "sample.size"
								defaultValue: 0.01
							}
						}
						Row
						{
							Layout.preferredWidth: 100 * preferencesModel.uiScale
							spacing: 4 * preferencesModel.uiScale

							DoubleField
							{
								name: "mu"
								defaultValue: 0


							}
							TextField
							{
								name: "sigma"
								placeholderText: "sdy"
								fieldWidth: 40 * preferencesModel.uiScale
							}
						}
					}
				}
			}
		}

	}


	Section
	{

		title: qsTr('Plots')

		Group
		{
			title: qsTr('State Plots')

			CheckBox
			{
				name: 'checkboxPlotAggregatedStates'
				label: qsTr('Aggregated state contribution')

				//DropDown
				//{
				//	name: "scaleAggregatedStates"
				//	label: qsTr("Scale")
				//	values: [ 'linear', 'mean']
				//}

				CIField
				{
					name: 'ciAggregatedStates'
					label: qsTr('Credible interval')
				}
				CheckBox
				{
					name: "actualValuesAggregatedStates"
					label: qsTr("Show observations")
				}


			}

			CheckBox
			{
				name: 'checkboxPlotComponentStates'
				label: qsTr('Component state contribution')

			}
		}

		//Group
		//{
		//	title: qsTr('Coefficients')

		//	CheckBox
		//	{
		//		name: 'checkboxPlotIncProb'
		//		label: qsTr('Inclusion probability plot')
		//	}
		//	CheckBox
		//	{
		//		name: 'checkboxPlotDynReg'
		//		label: qsTr('Dynamic regression plot')
		//	}
		//}
		Group
		{
			title: qsTr('Residuals')
			CheckBox {name:'checkBoxResidual'; label: qsTr('Posterior distribution of residuals')}
			//CheckBox {name:'checkBoxForecast'; label: qsTr('Posterior distribution of one-step-ahead prediction')}
			CheckBox {name:'checkBoxForecastError'; label: qsTr('Posterior distribution of one-step-ahead prediction error')}
		}

		Group
		{
			title: qsTr("Prediction")
			DoubleField
			{
				name: "predictionHorizon"
				label: qsTr("Horizon")
			}
		}

	}

	//Section
	//{
	//	title: qsTr("Priors")

	//	VariablesList
	//	{
	//		name: "ManualPriors"
	//		source: [ { rSource: "myRSource" } ]
	//	}
	//}

	Section

	{

		title: qsTr('Advanced Options')

		//DropDown
		//{
		//	name: "distFam"
		//	indexDefaultValue: 1
		//	label: qsTr("Distribution family")
		//	values: [ 'Gaussian', 'Logit','Poisson','Student']
		//}

		DoubleField { name:'mcmcDraws';		label: "Desired MCMC draws";	fieldWidth: 60; defaultValue: 500}

		DoubleField { name:'timeout';		label: "Timout in seconds";	fieldWidth: 60; defaultValue: 30}

		RadioButtonGroup
		{
			name: "burnSpecification"
			title: qsTr("Burn-in Specification")
			radioButtonsOnSameRow: TRUE
			RadioButton
			{
				value: "burnSuggested"; label: qsTr("Automatic suggestion"); checked: true
				DoubleField { name:'propBurnSuggested'
											label: "Proportion"
											fieldWidth: 60
										 	defaultValue: 0.1
											min:0
											max: 0.999
										 	}
			}
			RadioButton
			{
				value: "burnManual"; label: qsTr("Manual")
				DoubleField { name:'numberBurnManual'
											label: qsTr("Number")
											fieldWidth: 60
											defaultValue: 0

										 	}
			}
		}

	}
}
