//---------------------------------------------------------------------------

#include <vcl.h>
#include <stdlib.h>
#include "shellapi.hpp"
#pragma hdrstop

#include "CarsDataForGrid.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxLookAndFeels"
#pragma link "CarsData"
#pragma link "cxEditRepositoryItems"
#pragma link "cxDBExtLookupComboBox"
#pragma link "cxEdit"
#pragma link "cxDBEditRepository"
#pragma link "cxClasses" 
#pragma link "dxmdaset"
#pragma link "cxStyles"
#pragma link "cxCustomData" 
#pragma link "cxGraphics"
#pragma link "cxFilter"
#pragma link "cxData"
#pragma link "cxDataStorage"
#pragma link "cxNavigator"
#pragma link "cxDBData"
#pragma link "cxHyperLinkEdit"
#pragma link "cxGridCustomTableView"
#pragma link "cxGridTableView"
#pragma link "cxGridBandedTableView"
#pragma link "cxGridDBBandedTableView"
#pragma link "cxControls"
#pragma link "cxGridCustomView"
#pragma link "cxGrid"
#pragma resource "*.dfm"
TdmGridCars *dmGridCars;
//---------------------------------------------------------------------------
__fastcall TdmGridCars::TdmGridCars(TComponent* Owner): TdmCars(Owner)
{
}
