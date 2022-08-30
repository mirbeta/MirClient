//---------------------------------------------------------------------------

#include <vcl.h>
#include <stdlib.h>
#include "shellapi.hpp"
#pragma hdrstop

#include "CarsData.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxControls"
#pragma link "cxClasses"
#pragma link "cxLookAndFeels"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
#pragma link "dxmdaset"
#pragma link "cxEdit"
#pragma link "cxEditRepositoryItems"
#pragma link "cxDBEditRepository"
#pragma link "cxStyles"
#pragma link "cxCustomData"
#pragma link "cxFilter"
#pragma link "cxData"
#pragma link "cxDataStorage"
#pragma link "cxNavigator"
#pragma link "cxHyperLinkEdit"
#pragma link "cxDBData"
#pragma link "cxMemo"
#pragma resource "*.dfm"
TdmCars *dmCars;
//---------------------------------------------------------------------------
__fastcall TdmCars::TdmCars(TComponent* Owner): TDataModule(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TdmCars::DataModuleCreate(TObject* Sender)
{
	String APath = ExtractFilePath(Application->ExeName) + "..\\..\\Data\\";
	mdBodyStyle->LoadFromBinaryFile(APath + "CarsBodyStyle.dat");
	mdCategory->LoadFromBinaryFile(APath + "CarsCategory.dat");
	mdModels->LoadFromBinaryFile(APath + "CarsModel.dat");
	mdTrademark->LoadFromBinaryFile(APath + "CarsTrademark.dat");
	mdTransmissionType->LoadFromBinaryFile(APath + "CarsTransmissionType.dat");
	mdCarOrders->LoadFromBinaryFile(APath + "CarOrders.dat");

	mdBodyStyle->Active = True;
	mdCategory->Active = True;
	mdTrademark->Active = True;
	mdTransmissionType->Active = True;
	mdModels->Active = True;
	mdCarOrders->Active = True;
}
//---------------------------------------------------------------------------
void __fastcall TdmCars::mdModelsCalcFields(TDataSet* DataSet)
{
  mdModelsFullName->Value = mdModelsTrademark->Value + " " + mdModelsName->Value;
}
