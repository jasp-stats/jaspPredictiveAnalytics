import QtQuick
import QtQuick.Layouts
import JASP.Controls
import JASP.Widgets






Form
{

	VariablesForm
	{

		AvailableVariablesList	{ name: "allVariablesList" }

		AssignedVariablesList
		{
			name: "dependent"
			id: dependent
			title: qsTr("Dependent Variable")
			suggestedColumns: ["scale"]
			singleVariable: true
			info: qsTr("Time series variable to be predicted. (needed)")
		}

		AssignedVariablesList
		{
			name: "time"
			id: time
			title: qsTr("Time")
			suggestedColumns: ["nominal"]
			singleVariable: true
			info: qsTr("Time variable that each corresponds to the time stamp of each observation. Can be in the following formats: ['YYYY-MM-DD', 'YYYY/MM/DD', 'YYYY-MM-DD HH:MM:SS', 'YYYY/MM/DD HH:MM:SS'] (needed)")
		}

		AssignedVariablesList
		{
			name: "covariates"
			id: covariates
			title: qsTr("Covariates")
			suggestedColumns: ["scale"]
			allowedColumns: ["scale"]
			info: qsTr("Covariates to be used in the prediction model. (optional)")
		}

		AssignedVariablesList
		{
			name: "factors"
			id: factors
			title: qsTr("Factors")
			allowedColumns: ["ordinal", "nominal"]
		}

		AssignedVariablesList
		{
			name: "trainingIndicator"
			id: trainingIndicatorVariable
		 	title: qsTr("Include in Training")
			suggestedColumns: ["scale"]
			singleVariable: true
			info : qsTr("Logical variable (only 0 or 1) indicating which cases should be used for training and verifying the models (= 1) and which cases should be predicted (= 0). This variable is necessary for making predictions when covariates and factors are supplied")
		}
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
					{ label: "Data Based", value: "stdDevBound"},
    				{ label: "Manual Bounds", value: "manualBound"}
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
						label: qsTr("Custom period")
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
				checked: true


				CheckBox
				{
					name: "controlSpreadPointsEqually"
					label: qsTr("Spread points equally")
					checked: true
				}
				RadioButtonGroup

            	{
                	name: "controlLineType"
                	radioButtonsOnSameRow: true
                	RadioButton { value: "points";	label: qsTr("Points") }
                	RadioButton { value: "line";	label: qsTr("Line"); checked: true }
                	RadioButton { value: "both";	label: qsTr("Both")}
            	}

				RadioButtonGroup
				{
					name: "xAxisLimit"
					title: "Y-Axis Limit:"
					radioButtonsOnSameRow: true
					RadioButton { value: "allData";	label: qsTr("All data") }
					RadioButton { value: "controlBounds";	label: qsTr("Control bounds") }
				}

				CheckBox{name: "controlPlotGrid"; label: qsTr("Enable grid")}

				CheckBox
				{
					name: "controlPlotZoomCheck"
					label: qsTr("Custom plot focus")
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
				CheckBox
				{
					name: "controlPlotReportingCheck"
					enabled: preferencesModel.reportingMode
					checked: false
					label: "Reporting mode"
					CIField{name: "controlPlotReportingPercent"; label: "Out-of-bound percent threshold";defaultValue:5}

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
				label: "Control summary table"
			}
			CheckBox
			{
				name: "outlierTableCheck"
				label: "Outlier table"
				CheckBox {name: "outlierTableTransposeCheck"; label: "Transpose table"}
				CheckBox
				{
					name: "outlierTableFocusCheck"
					label: qsTr("Custom table focus")
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
					//CheckBox
					//{
					//	name: "outlierHistogramDensity"
					//	label: qsTr("Show densities")
					//}
				}

			CheckBox
			{
				name: "acfPlotCheck"
				label: "Autocorrelation function"
				IntegerField{name:"acfLagsMax"; label: qsTr("Lags"); defaultValue: 30}
				CheckBox{name: "acfPartialCheck";label: qsTr("Partial autocorrelation function")}
			}

		}
	}


	Section
	{
		title: qsTr("Feature Engineering")

		IntegerField{name: "featEngLags";id: featEngLags; label: "Nr. of lags"; defaultValue: 0; min: 0; max: (resampleInitialTraining.value - 1)}

		CheckBox{name: "featEngAutoTimeBased"; id: featEngAutoTimeBased; label: "Automatic time-based features"}






		CheckBox
		{
			name: "featEngImputeTS"
			label: qsTr("Impute missing values")

		}



	}
	Section
    {
        title: qsTr("Forecast Evaluation")
		Group
		{
			Layout.columnSpan: 2
			columns:2
			Group
        {
			title: qsTr("Evaluation Plan")
            //Layout.columnSpan: 1
			IntegerField{
				name: "resampleForecastHorizon"
				min: 2
				max: dataSetInfo.rowCount - resampleInitialTraining.value
				id: "resampleForecastHorizon"
				label: qsTr("Prediction window")
				defaultValue: Math.floor((dataSetInfo.rowCount / 5)*0.6)
			}
			IntegerField{
				name: "resampleInitialTraining"
				max: dataSetInfo.rowCount - resampleForecastHorizon.value
				min: 2
				id: "resampleInitialTraining"
				label: qsTr("Training window")
				defaultValue: Math.floor((dataSetInfo.rowCount / 5)*1.4)
			}
            IntegerField{name: "resampleSkip"; label: qsTr("Skip between training slices");defaultValue: resampleForecastHorizon.value}

			RadioButtonGroup
			{
				name: "resampleSliceStart"
				title: qsTr("Select slices from:")
				radioButtonsOnSameRow: true
				RadioButton{ value: "head"; label: qsTr("Start")}
				RadioButton{ value: "tail"; label: qsTr("End"); checked: true}
			}
            IntegerField{name: "resampleMaxSlice"; id: "maxSlices"; label: qsTr("Maximum nr. of slices"); defaultValue:5; min: 1}
            CheckBox{name: "resampleCumulativeCheck"; label: qsTr("Cumulative training")}

			CheckBox
			{
				name: "resampleCheckShowTrainingPlan"
				label: qsTr("Show evaluation plan")
				CheckBox{ name: "resamplePlanPlotEqualDistance"; label: qsTr("Spread points equally"); checked: true}
				IntegerField{name: "resamplePlanPlotMaxPlots"; label: "Max slices shown:"; defaultValue: maxSlices.value ; max: maxSlices.value ;min:1}
			}
        }
		}





		Group
		{
			title: qsTr("Model Choice")
			Layout.columnSpan: 2


			VariablesForm
			{
				// this function dynamically selects the available models based on users input
				// due to some bug however, the values can't be directly selected as they dissapear in between saves
				// instead the values are computed, hidden and then used as input for another AvailableVariables List
				visible: false
				AvailableVariablesList
				{
					name:"availableModelsHidden"
					
					// at the current moment, the user can choose models from a variety of predefined models
					// some of these models contain a regressive component or lagged values
					// but they only work if the user provided covariates or created some in the feature engineering section
					// to streamline the user experience, this function dynamically changes which models are available in the qml menu
					function getAvailableModels()
					{
						const models = []

						if(!dependent.count > 0 && !time.count > 0)
							return([{values: models}])


						// 'pure' time series models that only depend on time variable
 						// triggered if ready == T as time and dependent are needed for that
						if(dependent.count > 0 && time.count > 0)
						{
							models[0] = {label : qsTr("linear regression - y ~ time"), value: "lmSpike"}
							models[3] = {label : qsTr("bsts - linear trend model"), value: "bstsLinear"}
							models[5] = {label : qsTr("bsts - autoregressive model"), value: "bstsAr"}
							models[7] = {label : qsTr("prophet"), value: "prophet"}
						}
						// if covariates are provided or time based features are created
						if(featEngAutoTimeBased.checked || factors.count > 0 || covariates.count > 0)
						{
							models[1] = {label : qsTr("linear regression - regression"), value: "lmSpikeReg"}
							models[4] = {label : qsTr("bsts - linear trend model - regression"), value: "bstsLinearReg"}
							models[6] = {label : qsTr("bsts - autoregressive model - regression"), value: "bstsArReg"}
							models[8] = {label : qsTr("prophet - regression"), value: "prophetReg"}
							models[9] = {label : qsTr("bart - regression"), value: "bartReg" }

						}
						//extra model for artificial lags because it can increase computation time a lot
						if(featEngLags.value > 0)
						{
							models[2] = {label : qsTr("linear regression - regression + lag"), value: "lmSpikeRegLag"}
							models[10] = {label : qsTr("bart - regression + lag"), value: "bartRegLag"}
						}
						// function returns one empty value and i haven't figured out why, so this is workaround
						const modelsFiltered = models.filter((obj) => obj.label !== '')

						const modelsList = [{values: modelsFiltered}]
						return(modelsList)
					}
					source: getAvailableModels()
				}
			}

			VariablesForm
			{
				// this function dynamically selects the available models based on users input
				// due to some bug however, the values can't be directly selected as they dissapear in between saves
				// instead the values are computed, hidden and then used as input for another AvailableVariables List
				AvailableVariablesList
				{
					name: "availableModels"
					source: "availableModelsHidden"
				} 
				AssignedVariablesList
				{
					name: "selectedModels"
					id: selectedModels

				}
			}



		}


		Group
		{

			title: qsTr("Evaluation Metrics")
			Layout.columnSpan: 2
		}
			Group
			{
				title: qsTr("Probabilistic")


				CheckBox{ name: "metricCrps"; 		label: qsTr("Continuous ranked probability score"); checked: true}
				CheckBox{ name: "metricDss"; 		label: qsTr("Dawid–Sebastiani score"); checked: true}
				CheckBox{ name: "metricLog"; 		label: qsTr("Log score"); checked: true}
				CheckBox{ name: "metricCoverage";	label: qsTr("Coverage"); checked: true}
				CheckBox{ name: "metricBias"; 		label: qsTr("Bias"); checked: true}
				CheckBox{ name: "metricPit";		label: qsTr("Probability integral transform"); checked: true}
			}





		Group
		{
			title: qsTr("Deterministic")

			CheckBox{ name: "metricMae"; 		label: qsTr("Mean absolute error"); checked: true}
			CheckBox{ name: "metricRmse"; 		label: qsTr("Root mean squared error"); checked: true}
			CheckBox{ name: "metricR2"; 		label: qsTr("R²"); checked: true}
		}

		Group
		{
			title: qsTr("PIT Binned Density Plots")
			Layout.columnSpan: 2
			VariablesForm

			{

				preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
				AvailableVariablesList
				{

					name: "fromRSource"
					source: (!doBMA.checked) ? "selectedModels" : [ "selectedModels", { values: ["BMA"] } ]
				}
				AssignedVariablesList
				{
					//height: 200
					name: "pitPlots"

				}
			}
			//CheckBox{name: "modelsToPlotCredibleInterval"; label: qsTr("Show credible interval")}

		}



	}



	Section
	{
		title: qsTr("Prediction Plots")
		Group
		{
			title: qsTr("Models to plot")
			VariablesForm

			{

				preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
				AvailableVariablesList
				{

					name: "fromR"
					source: (!doBMA.checked) ? "selectedModels" : [ "selectedModels", { values: ["BMA"] } ]
				}
				AssignedVariablesList
				{
					//height: 200
					name: "modelsToPlot"

				}
			}
			//CheckBox{name: "modelsToPlotCredibleInterval"; label: qsTr("Show credible interval")}
			IntegerField{name: "modelsToPlotSlices"; label: "Max slices shown:"; defaultValue: maxSlices.value ; max: maxSlices.value ;min:1}

		}
	}


	Section
	{
		title: qsTr("Ensemble Bayesian Model Averaging")
		CheckBox
		{
			name: "checkPerformBma"
			label: "Perform eBMA"
			enabled: selectedModels.count > 0
			id: doBMA
			//checked: true

			RadioButtonGroup
			{
				name: "bmaMethod"
				title: qsTr("Method")
				RadioButton{ value: "bmaMethodEm"; label: qsTr("Expectation–maximization"); checked: true}
				RadioButton{ value: "bmaMethodGibbs"; label: qsTr("Gibbs sampling")}
			}
			RadioButtonGroup
			{
				name: "bmaTestPeriod"
				visible: false
				title: "Evaluation Method"
				RadioButton{ value: "bmaTestNextSlice"; label: qsTr("Next test slice"); checked: true}
				RadioButton
				{
					value: "bmaSameSlice"
					label: qsTr("Same test slice")
					CIField { name: "bmaTestProp"; label: qsTr("Last");afterLabel: qsTr("% of data");defaultValue: 30;decimals:0;fieldWidth: 30}
				}
			}
		}
		Group
		{
			title: qsTr("Tables")
			CheckBox{
				name: "bmaWeightsTable"
				enabled: doBMA.checked
				label: qsTr("Model weights")
				checked: true
				CheckBox{name: "bmaWeightsTablePerSlice"; label: qsTr("Show per slice");checked: false}
			}

		}

	}


	Section
	{
		title: qsTr("Future Prediction")


		VariablesForm
		{
			preferredHeight: jaspTheme.smallDefaultVariablesFormHeight/2
			enabled: futurePredPredictionType.value != "noFuturePrediction"
			//title: qsTr("Models")
			AvailableVariablesList
				{

					name: "futurePredictionModels"
					source:  (!doBMA.checked) ? "selectedModels" : [ "selectedModels", { values: ["BMA"] } ]

				}
				AssignedVariablesList
				{

					name: "selectedFuturePredictionModel"
					singleVariable: true
					height: 30

				}

		}


		Group
		{
			RadioButtonGroup
			{
				title: qsTr("Prediction type")
				name: "futurePredPredictionType"
				id: futurePredPredictionType

				RadioButton
				{
					value: "noFuturePrediction"
					checked: trainingIndicatorVariable.count == 0
					id: noFuturePrediction
					
					label: qsTr("No forecast - verification only")
				}


				RadioButton
				{
					value: "trainingIndicator"
					id: trainingIndicator
					enabled: trainingIndicatorVariable.count > 0
					checked: trainingIndicatorVariable.count > 0
					label: qsTr("Training indicator")
				}

				//RadioButton
				//{
				//	name: "timepoints"
				//	label: qsTr("Time points")
				//	childrenOnSameRow: true
				//	IntegerField
				//	{
				//		name: "futurePredictionPoints"; afterLabel: qsTr("data points");min: 1; defaultValue: resampleForecastHorizon.value 
				//	}
				//}

				RadioButton
				{
					value: "periodicalPrediction"
					id: periodicalPrediction
					label: qsTr("Periodical")
					IntegerField
					{
						name: "periodicalPredictionNumber"
						label: qsTr("Number of periods")
						defaultValue: resampleForecastHorizon.value
					}
					DropDown
					{
						name: "periodicalPredictionUnit"
						label: qsTr("Unit")
						indexDefaultValue: 3
						values:
						[
							{ label: qsTr("Seconds"), value: "secs"		},
							{ label: qsTr("Minutes"), value: "mins"		},
							{ label: qsTr("Hours"),   value: "hours"	},
							{ label: qsTr("Days"),    value: "days"		},
							{ label: qsTr("Weeks"),   value: "weeks"	},
							{ label: qsTr("Months"),   value: "months"	},
							{ label: qsTr("Years"),   value: "years"	}
						]
					}
				}
			}

			RadioButtonGroup
			{
				title: qsTr("Training window")
				name: "futurePredTrainingPeriod"
				enabled: futurePredPredictionType.value != "noFuturePrediction"

				RadioButton
				{
					value: "last"
					checked: !resampleCumulativeCheck.checked
					label: qsTr("Last")
					childrenOnSameRow: true
						IntegerField{name: "futurePredTrainingPoints"; afterLabel: qsTr("data points"); defaultValue: resampleInitialTraining.value}
				}
				RadioButton{name: "all"; label: qsTr("All data points"); checked: resampleCumulativeCheck.checked }

		}
	}

	Group
	{


		CheckBox
		{
			name: "checkFuturePredictionPlot"
			label: "Future prediction plot"
			checked: periodicalPrediction.checked || trainingIndicator.checked
			enabled: periodicalPrediction.checked || trainingIndicator.checked
			CheckBox
			{
				name: "futurePredSpreadPointsEqually"
				label: qsTr("Spread points equally")
				checked: periodicalPrediction.checked || trainingIndicator.checked
				enabled: periodicalPrediction.checked || trainingIndicator.checked
			}

		}
		CheckBox
		{
			name: "futurePredReportingCheck"
			label: "Reporting mode"
			checked: false
			enabled: preferencesModel.reportingMode && (periodicalPrediction.checked || trainingIndicator.checked)
			CIField{name: "futurePredThreshold"; label: "Out-of-bound probability threshold"}

		}

	}

	}

	//Section
	//{
		//title: qsTr("Advanced Options")

		//CheckBox{name: 'parallelComputation'; label: 'Parallel model computation';checked: true}






	//}




}
