import QtQuick 2.11
import QtQuick.Layouts 1.3
import JASP.Controls 1.0
import JASP.Widgets 1.0

Form
{
    VariablesForm
    {
        AvailableVariablesList	{ name: "allVariablesList" }
        AssignedVariablesList	{ name: "variables";	title: qsTr("Control Variables");	allowedColumns: ["scale"]}
    }






    IntegerField
    {
        name: "previousDataPoints"
        Layout.columnSpan: 2
        label: qsTr("Only include previous")
        defaultValue: 10000
        afterLabel: "data points"

    }



    CheckBox
    {
        name: "overallControlSummaryTable"
        Layout.columnSpan: 1
        label: qsTr("Overall control summary table")
        checked: true
        CheckBox{name: "orderTableByOutBound"; label: qsTr("Order table by proportion"); checked: true}
        CheckBox{name : "transposeOverallTable"; label: qsTr("Transpose table")}

    }


    CheckBox
    {
        name: "outBoundOverallPlotCheck"
        label: qsTr("Overall control plot")

        checked: true
        Layout.rowSpan: 2
        //CheckBox{name: "summaryPlotIndividualVars"; label: qsTr("plot individual variables")}

        RadioButtonGroup
            	{
                	name: "outBoundOverallPlotLineType"
                	radioButtonsOnSameRow: true
                    id: overollPlotType
                	RadioButton { value: "points";	label: qsTr("Points");	checked: true }
                	RadioButton { value: "line";	label: qsTr("Line") }
                	RadioButton { value: "both";	label: qsTr("Both") }
            	}

                CheckBox
                {
                    name: "outBoundOverallPlotJitterCheck"
                    label: qsTr("Add jitter")
                    enabled : overollPlotType.value == "points" || overollPlotType.value == "both"
                    checked:  !overollPlotType.value == "line"
                }
        RadioButtonGroup
        {

            name: "outBoundOverallPlotMetricChoice"
            title: qsTr("Out-of-bound metric")
            radioButtonsOnSameRow: false
            RadioButton{value: "number"; label: qsTr("Number");checked: true}
            RadioButton{value: "proportion"; label: qsTr("Proportion")}
        }
    }
    Group
    {
        title: qsTr("Proportion limits for reporting")
        DoubleField{name: "estimatedLimit"; defaultValue: 0.2; label: qsTr("Estimation limit")}
        DoubleField{name: "predictionLimit"; defaultValue: 0.2; label: qsTr("Prediction limit")}

    }
    Section
    {
        title: qsTr("Proportion Estimation")

        IntegerField
        {
            name: "multiBinWindow"
            min: 1
            defaultValue: 1
            label: qsTr("Summarise after every")
            afterLabel: qsTr("data points")
        }
        IntegerField{name: "multiBinomDraws"; defaultValue: 500; label: qsTr("MCMC draws")}

        CheckBox
        {
            name: "multiBinaryCheckPlot"
            label: qsTr("Multivariate binary control plot")
            checked: true
        }



    }
    Section
    {
        title: qsTr("Prediction")

        IntegerField
        {
            name: "predictionHorizon"
            Layout.columnSpan: 2
            id: predictionHorizon
            defaultValue: 500
            label: qsTr("Prediction horizon")
        }
        CheckBox
        {
            name: "predictionTimePlot"
            checked: true
            label: qsTr("Forecast plot")
            enabled: predictionHorizon.value > 0
        }
        CheckBox
        {
            name: "predictionTimeTable"
            label: qsTr("Prediction table")
            checked: true
            enabled: predictionHorizon.value > 0
            CheckBox{name: "predictionTableNumber";label: qsTr("Show predicted number"); checked: true}
            IntegerField{name: "binTable"; defaultValue: predictionHorizon.value/10; min:1;max: predictionHorizon.value; label: qsTr("Bin predictions every");afterLabel: "data points"}

        }
    }
}
