//---------------------------------------------------------------------------

#include <vcl.h>
#include "shellapi.hpp"
#pragma hdrstop

#include "UnboundDesignDefinedDemoMain.h"
#include "AboutDemoForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxControls"
#pragma link "cxCustomData"
#pragma link "cxData"
#pragma link "cxDBData"
#pragma link "cxEdit"
#pragma link "cxFilter"
#pragma link "cxGraphics"
#pragma link "cxGrid"
#pragma link "cxGridCustomTableView"
#pragma link "cxGridCustomView"
#pragma link "cxGridDBTableView"
#pragma link "cxGridLevel"
#pragma link "cxGridTableView"
#pragma link "cxStyles"
#pragma link "cxButtons"
#pragma link "cxCheckBox"
#pragma link "cxContainer"
#pragma link "cxGridCardView"
#pragma link "cxGridDBCardView"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxMaskEdit"
#pragma link "cxSpinEdit"
#pragma link "cxTextEdit"
#pragma link "cxLookAndFeels"
#pragma link "cxGridBandedTableView"
#pragma link "cxCurrencyEdit"
#pragma link "cxDataStorage"
#pragma link "cxHyperLinkEdit"
#pragma link "cxImage"
#pragma link "cxImageComboBox"
#pragma resource "*.dfm"
TUnboundDesignDefinedDemoMainForm *UnboundDesignDefinedDemoMainForm;
//---------------------------------------------------------------------------
__fastcall TUnboundDesignDefinedDemoMainForm::TUnboundDesignDefinedDemoMainForm(TComponent* Owner)
  : TfmBaseForm(Owner)
{
}

