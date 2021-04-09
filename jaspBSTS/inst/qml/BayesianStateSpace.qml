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
	  title: qsTr("Add autoregressive process")

	  CheckBox
    {
      name: "checkboxAr"
      label: qsTr("Autoregressive Process")
      id: checkAr
      Layout.columnSpan: 2

      columns: 2

      IntegerField
      {
          name: "ArLag"
          label: qsTr("No. of lags")
          defaultValue: 1
          fieldWidth: 60
			}



    }

  }
}
