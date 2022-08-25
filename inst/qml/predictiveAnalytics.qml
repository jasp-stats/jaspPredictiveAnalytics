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
		AssignedVariablesList	{ name: "dates";		title: qsTr("Time");				suggestedColumns: ["nominal"];	singleVariable: true		}
	}


	Section
	{
		title: qsTr("Time Series Descriptives")

		Group
		{
			title: qsTr("Error Bound Selection Method")
			DropDown
			{
				name: "errorBoundMethodDrop"
				id: methodSelection
				values:
 				[
    				{ label: "Manual Bounds", value: "manualBound"},
    				{ label: "Data Based", value: "stdDevBound"}
  				]
			}

			Group
			{
				visible: methodSelection.currentValue == "manualBound"

				RadioButtonGroup
				{
					name: "manualBoundMethod"
					RadioButton
					{
						value: "manualBoundUniform"
						checked: true
						childrenOnSameRow: true
						Group
						{
							columns: 2
							DoubleField{name: "manualBoundMean";label: "Mean"; negativeValues: true}
							DoubleField{name: "manualBoundErrorBound";label: "+/-"}
						}




					}
					RadioButton
					{
						value: "manualBoundCustom"
						childrenOnSameRow: true
						Group
						{
							columns: 1
							DoubleField{name: "manualUpperBound";label: "Upper bound"; negativeValues: true}
							DoubleField{name: "manualLowerBound";label: "Lower bound"; negativeValues: true}
						}

					}
				}

				//IntegerField{name: "controlMean"; label: qsTr("Control Mean"); defaultValue: Null; negativeValues: true}
				//IntegerField{name: "controlError"; label: qsTr("Control Error"); defaultValue: 0; negativeValues: false}
			}

			Group
			{
				visible: methodSelection.currentValue == "stdDevBound"
				DoubleField{name: "sigmaBound"; label: qsTr("σ threshold"); defaultValue: 2}
				CheckBox
				{
					name: "trimmedMeanCheck"
					label: qsTr("Trimmed mean")
					DoubleField{name: "trimmedMeanPercent";label: qsTr("Percent");	max: 0.5}
				}
					CheckBox
					{
						name: "controlPeriodCheck"
						label: qsTr("Custom Period")
						childrenOnSameRow: false
						// fix that end period is from start to nrow of series
						Group
						{
							columns: 2
							IntegerField{name:"controlPeriodStart"; label: qsTr("Start"); defaultValue: 0}
							IntegerField{name:"controlPeriodEnd"; label: qsTr("End"); defaultValue: 0}

						}

					}


			}






		}
		Group
		{
			title: "Control Plots"

			CheckBox
			{
				name: "controlPlotCheck"
				label: qsTr("Display control chart")
				id: controlPlotCheckbox
				RadioButtonGroup

            	{
                	name: "controlLineType"
                	radioButtonsOnSameRow: true
                	RadioButton { value: "points";	label: qsTr("Points") }
                	RadioButton { value: "line";	label: qsTr("Line") }
                	RadioButton { value: "both";	label: qsTr("Both");	checked: true }
            	}

				RadioButtonGroup
				{
					name: "xAxisLimit"
					title: "Y-Axis Limit:"
					radioButtonsOnSameRow: true
					RadioButton { value: "allData";	label: qsTr("All data") }
					RadioButton { value: "controlBounds";	label: qsTr("Control bounds") }
				}

				CheckBox
				{
					name: "controlPlotZoomCheck"
					label: qsTr("Custom Plot Focus")
					childrenOnSameRow: false
					enabled: controlPlotCheckbox.checked
					// fix that end period is from start to nrow of series
					Group
					{
						columns: 2
						IntegerField{name:"zoomPeriodStart"; label: qsTr("Start"); defaultValue: 0}
						IntegerField{name:"zoomPeriodEnd"; label: qsTr("End"); defaultValue: 0}

					}

				}
			}
		}


	}
	Section
	{
		title: qsTr("Diagnostics")

		columns: 2


		Group
		{
			title: qsTr("Tables")

			CheckBox
			{
				name: "summaryStatsTableCheck"
				label: "Summary statistics"
			}
			CheckBox
			{
				name: "outlierTableCheck"
				label: "Outlier table"
				CheckBox {name: "outlierTableTransposeCheck"; label: "Transpose table"}
				CheckBox
				{
					name: "outlierTableFocusCheck"
					label: qsTr("Custom Table Focus")
					childrenOnSameRow: false
					enabled: "summaryStatsTableCheck".checked
					// fix that end period is from start to nrow of series
					Group
					{
						columns: 2
						IntegerField{name:"outLierTableStart"; label: qsTr("Start"); defaultValue: 0}
						IntegerField{name:"outLierTableEnd"; label: qsTr("End"); defaultValue: 0}

					}

				}
			}

		}

		Group
		{
			title: qsTr("Plots")

				CheckBox
				{
					name: "outlierHistogramCheck"
					label: qsTr("Histogram")
					CheckBox
					{
						name: "outlierHistogramDensity"
						label: qsTr("Show densities")
					}
				}

			CheckBox
			{
				name: "acfPlotCheck"
				label: "Autocorrelation function"
				IntegerField{name:"options$acfLagsMax"; label: qsTr("Lags"); defaultValue: 30}
			}

		}
	}


	//Section
	//{
	//	title: qsTr("Full Data Prediction")
//
	//	CheckBox
	//	{
	//		name: "controlPredictionCheck"
//
	//		label: "Control Prediction"
//
	//		Group
	//		{
	//			title: "Model Period"
	//			columns: 2
	//			IntegerField{name: "controlPredictionStart";label: "Start"}
	//			IntegerField{name: "controlPredictionEnd";label: "End"}
	//		}
//
	//		IntegerField{name: "controlPredictionHorizon";label: "Prediction Horizon"}
	//		IntegerField{name: "predDraws"; label: "Niter"}
	//		CheckBox{name:"controlPredictionFocus"; label: "Focus on all available data"}
//
//
//
	//	}
	//}


	Section
	{
		title: qsTr("Model Selection")
		columns: 2






	//	Group
	//	{
	//		//title: qsTr("Model Selection")
	//		CheckBox{name:"forecastModelBaselineRunVar";label: "baseline - running variance"}
	//		CheckBox{name:"forecastModelBaselineRunVarMean";label: "baseline - running variance & mean"}
	//		CheckBox{name:"forecastModelBstsLocalLevelCheck";label: "bsts - local level model"}
	//		CheckBox{name:"forecastModelBstsLinearTrendCheck";label: "bsts - linear trend model"}
	//		CheckBox{name:"forecastModelBstsArCheck";label: "bsts - autoregressive model"}
	//		CheckBox{name:"forecastModelBstsSemiLocalCheck";label: "bsts - semi local trend model"}


	//	}

		VariablesForm
		{
			preferredHeight: jaspTheme.smallDefaultVariablesFormHeight

			AvailableVariablesList
			{
				name: "modelOptions"
				source: [{values: ["baseline - running variance",
									"baseline - running variance & mean",
									"bsts - local level model",
									"bsts - linear trend model",
									"bsts - autoregressive model",
									"bsts - semi local trend model"]}]
			}
			AssignedVariablesList
			{
				name: "selectedModels"
			}
		}
	}


	Section
	{
		title: qsTr("Model Plots")
		VariablesForm
		{
			preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
			AvailableVariablesList
			{

				name: "fromR"
				source: [ { rSource: "plottableModelsQml" } ]
			}
			AssignedVariablesList
			{
				//height: 200
				name: "modelsToPlot"
			}
		}
	}

	Section

	{
		columns : 2


		title: qsTr("Forecast Verification")

		VariablesForm
		{
			preferredHeight: jaspTheme.smallDefaultVariablesFormHeight

			AvailableVariablesList
			{
				name: "forecastVerificationAvailableModels"
				source: [ { rSource: "plottableModelsQml" } ]
			}
			AssignedVariablesList
			{
					name: "forecastVerificationSelectedModels"
			}
		}

		Group
		{
			Layout.columnSpan: 1
	 		IntegerField{name: "forecastVerificationPredictionSteps";label: "k-step ahead prediction";defaultValue: 1;min: 1}
			IntegerField{name: "forecastVerificationModelWindow";label: "Sliding window";defaultValue: 30 ;min: 0}
			IntegerField{name: "forecastVerificationDraws";label: "MCMC draws";defaultValue:10;min: 10}
			IntegerField{name: "forecastVerificationModelHistory";label: "Model history";defaultValue:200;min: 0}

		}


		Group
		{
			title: qsTr("Probalistic Forecast Verification Metrics")

			//CheckBox{name:"forecastMetricsLog";label: "Logarithmic score";checked:true}
			CheckBox{name:"forecastMetricsCRPS";label: "Continuous ranked probability score";checked:true}
			CheckBox{name:"forecastMetricsDSS";label: "Dawid-Sebastiani score";checked:true}
			CheckBox{name:"forecastMetricsAUC";label: "ROC";checked:true}
			CheckBox{name:"forecastMetricsPR";label: "Precision-recall score";checked:true}
			CheckBox{name:"forecastMetricsBrier";label: "Brier score";checked:true}
			//CheckBox{name:"forecastMetricsRSME";label: "Root mean squared error"}


		}
	}
	Section
	{
		title: qsTr("Bayesian Model Averaging")

		Group
		{
			//DropDown
			//{
			//	label: qsTr("Bias correction method")

			//	name: "BMABiasCorrectionMethod"
			//	values:
 			//	[
    		//		{ label: "None", value: "none" },
    		//		{ label: "Additive", value: "additive"},
			//		{ label: "Regression", value: "regression"}
  			//	]
			//}
			CheckBox{name: "checkBMA";label: "Perform BMA";id: bmaEnabled}
			//IntegerField{name: "BMAtrainingPeriod"}
			CheckBox{name: "checkBMATrainingPlot"; label: "Show averaged predictions"; enabled:bmaEnabled.checked}

		}

		Group
		{
			title: qsTr("Model weights")
			enabled:bmaEnabled.checked

		CheckBox{name: "checkBMAmodelWeights"; label: "Show table"}
		CheckBox
		{
			name: "checkmodelWeightsPlot"
			label: qsTr("Plot weights over time")
			IntegerField{name: "modelWeightRunningWindow";label: qsTr("Running mean window:");min: 1;defaultValue:1}


		}
	}

		//Group{
//
		//	title: "Future Predictions"
//
		//	CheckBox{name: "checkFuturePredictionPlot"; label: "Future prediction plot"}
		//	CheckBox{name: "checkFuturePredictionTable"; label: "Future out-of-bound probability"}
//
//
		//}
	}

	Section
	{

		title: qsTr("Future Predictions")
		columns: 2

		CheckBox
		{
			name: "checkBoxFuturePredictions"
			label: qsTr("Predictions")


			IntegerField{name: "futurePredictions"; label: qsTr("Number of time points");min:1;defaultValue:10}



			RadioButtonGroup
			{
				title: qsTr("Model choice")
				name: "predictionModelChoice"
				radioButtonsOnSameRow: false
				RadioButton
				{
					value: "forecastBMA"
					label: qsTr("BMA model")
					enabled: bmaEnabled.checked
					IntegerField{name: "modelWeightWindow"; label: qsTr("Model weight window");defaultValue: 10; min: 10}
				}
				RadioButton
				{
					childrenOnSameRow: true
					value: "forecastBestModel"
					label: qsTr("Best model based on")
					checked: !bmaEnabled.checked
					DropDown
					{
						name: "forecastModelSelectionMetric"
						id: metricSelection
						values:
 						[
    						{ value: "CRPS", 	label: "modelSelectionCRPS"},
    						{ value: "DSS", 	label: "modelSelectionDSS"},
							{ value: "roc", 	label: "modelSelectionROC"},
							{ value: "pr", 		label: "modelSelectionPR"},
							{ value: "brier", 	label: "modelSelectionBrier"}

  						]
					}

				}
			}

		}

		Group
		{
			//enabled: bmaEnabled.checked ||
			CheckBox{name: "checkBoxOutBoundProbabilities"; label: "Out-of-bound probabilities"}
			CheckBox{name: "checkBoxOutBoundPlot"; label: "Future data predictions"}


		}

	}

	Section
	{
		title: qsTr("Binary Control Analysis")
		//Group
		//{
			CheckBox
			{
				name: "binaryControlChartCheck"

				label: "Show binary control chart"

				DropDown
				{
					name: "binaryControlMethod"
					id: binaryMethodSelection
					label: "Select Control Method"
					values: ["state space", "rolling average"]

				}

				DoubleField{ name: "binaryControlOutPropLimit"; label: qsTr("Proportion Limit")}
				Group
				{
					visible: binaryMethodSelection.currentValue == "state space"
					DoubleField
					{
						name: "binaryStateSpaceNiter"
						label: qsTr("MCMC samples")
						defaultValue: 500
					}
				}
			}
		//}
	}




}
