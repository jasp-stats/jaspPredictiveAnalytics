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
			title: qsTr("Dependent Variable")
			suggestedColumns: ["scale"]
			singleVariable: true
			info: qsTr("Time series variable to be predicted. (needed)")
		}

		AssignedVariablesList
		{
			name: "time"
			title: qsTr("Time")
			suggestedColumns: ["nominal"]
			singleVariable: true
			info: qsTr("Time variable that each corresponds to the time stamp of each observation. Can be in the following formats: ['YYYY-MM-DD', 'YYYY/MM/DD', 'YYYY-MM-DD HH:MM:SS', 'YYYY/MM/DD HH:MM:SS'] (needed)")
		}

		AssignedVariablesList
		{
			name: "covariates"
			title: qsTr("Covariates")
			suggestedColumns: ["scale"]
			allowedColumns: ["scale"]
			info: qsTr("Covariates to be used in the prediction model. (optional)")
		}

		AssignedVariablesList
		{
			name: "factors"
			title: qsTr("Factors")
			allowedColumns: ["ordinal", "nominal", "nominalText"]
		}

		AssignedVariablesList
		{
			name: "trainingIndicator"
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

		IntegerField{name: "featEngLags";label: "Nr. of lags";defaultValue: 0; min: 0}

		CheckBox{name: "featEngAutoTimeBased"; label: "Automatic time-based features"}



		CheckBox
		{
			name: "featEngImputeTS"
			label: qsTr("Impute missing values")

		}

		Group
		{
			Layout.columnSpan: 2
			CheckBox{name: "featEngRemoveZV"; label: qsTr("Remove zero-variance variables")}
			CheckBox{
				name: "featEngRemoveCor"
				label: qsTr("Remove variables that are stronger correlated than:")
				childrenOnSameRow: true
				DoubleField{ name: "featEngRemoveCorAbove"; defaultValue: 0.8}
			}
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
				id: "resampleForecastHorizon"
				label: qsTr("Prediction window")
				defaultValue: Math.floor((dataSetInfo.rowCount / 5)*0.6)
			}
			IntegerField{
				name: "resampleInitialTraining"
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
				preferredHeight: jaspTheme.smallDefaultVariablesFormHeight

				AvailableVariablesList
				{
					name: "modelSelection"
					width: preferencesModel.uiScale * 300
					source: [{values: [
					          {label : qsTr("linear regression - y ~ time"), value: "lmSpike"},
										{label : qsTr("linear regression - regression"), value: "lmSpikeReg"},
										{label : qsTr("linear regression - regression + lag"), value: "lmSpikeRegLag"},
										{label : qsTr("bsts - linear trend model"), value: "bstsLinear"},
										{label : qsTr("bsts - linear trend model - regression"), value: "bstsLinearReg"},
										//{label : qsTr("bsts - linear trend model - regression + lag"), value: "bstsLinearLag"},
										{label : qsTr("bsts - autoregressive model"), value: "bstsAr"},
										{label : qsTr("bsts - autoregressive model - regression"), value: "bstsArReg"},
										//{label : qsTr("bsts - autoregressive model - regression + lag"), value: "bstsArRegLag"},
										{label : qsTr("prophet"), value: "prophet"},
										{label : qsTr("prophet - regression"), value: "prophetReg"},
										//{label : qsTr("prophet - regression + lag"), value: "prophetRegLag"},
										//{label : qsTr("xgboost - regression"), value: "xgboostReg"},
										//{label : qsTr("xgboost - regression + lag"), value: "xgboostRegLag"},
										{label : qsTr("bart - regression"), value: "bartReg" },
										{label : qsTr("bart - regression + lag"), value: "bartRegLag"}
										//{label : qsTr("bart - stack"), value: "bartStackReg"}



										]
									}]
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
					source: [ { rSource: "plottableModelsQml" } ]
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
					source: [ { rSource: "plottableModelsQml" } ]
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
			id: doBMA
			//checked: true

			RadioButtonGroup
			{
				name: "bmaMethod"
				title: qsTr("Method")
				RadioButton{ value: "bmaMethodEm"; label: qsTr("Expectation–maximization")}
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
				CheckBox{name: "bmaWeightsTablePerSlice"; label: qsTr("Show per slice");checked: true}
			}

		}

	}


	Section
	{
		title: qsTr("Future Prediction")


		VariablesForm
		{
			preferredHeight: jaspTheme.smallDefaultVariablesFormHeight/2
			//title: qsTr("Models")
			AvailableVariablesList
				{

					name: "futurePredictionModels"
					source:  [ { rSource: "plottableModelsQml" } ]

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
				title: qsTr("Prediction horizon")
				name: "futurePredPredictionHorizon"


				//RadioButton
				//{
				//	name: "trainingIndicator"
				//	label: qsTr("Training indicator")
				//	checked: true
				//}

				//RadioButton
				//{
				//	name: "timepoints"
				//	label: qsTr("Time points")
				//	childrenOnSameRow: true
					IntegerField{name: "futurePredictionPoints"; afterLabel: qsTr("data points");min: 1; defaultValue: resampleForecastHorizon.value }
				//	checked: true
				//}

				//RadioButton
				//{
				//	name: "days"
				//	label: qsTr("Days:")
				//	childrenOnSameRow: true
				//	IntegerField{name: "futurePredictionDays"; min: 0; defaultValue: 0}
//
				//}

			}

			RadioButtonGroup
			{
				title: qsTr("Training window")
				name: "futurePredTrainingPeriod"

				RadioButton
				{
					value: "last"
					checked: true
					label: qsTr("Last")
					childrenOnSameRow: true
						IntegerField{name: "futurePredTrainingPoints"; afterLabel: qsTr("data points"); defaultValue: resampleInitialTraining.value}
				}
				RadioButton{name: "all"; label: qsTr("All data points")}

		}
	}

	Group
	{


		CheckBox
		{
			name: "checkFuturePredictionPlot"
			label: "Future prediction plot"
			checked: true
			CheckBox
			{
				name: "futurePredSpreadPointsEqually"
				label: qsTr("Spread points equally")
				checked: true
			}

		}
		CheckBox
		{
			name: "futurePredReportingCheck"
			label: "Reporting mode"
			checked: false
			enabled: preferencesModel.reportingMode
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
