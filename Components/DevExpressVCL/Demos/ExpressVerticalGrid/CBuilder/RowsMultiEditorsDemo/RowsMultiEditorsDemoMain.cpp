//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "RowsMultiEditorsDemoMain.h"
#include "RowsMultiEditorsDemoData.h"
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
#pragma link "cxStyles"
#pragma link "cxLookAndFeels"
#pragma link "DemoBasicMain"
#pragma link "cxDBVGrid"
#pragma link "cxEditRepositoryItems"
#pragma link "cxInplaceContainer"
#pragma link "cxVGrid"
#pragma resource "*.dfm"
TRowsMultiEditorsDemoMainForm *RowsMultiEditorsDemoMainForm;
//---------------------------------------------------------------------------
__fastcall TRowsMultiEditorsDemoMainForm::TRowsMultiEditorsDemoMainForm(TComponent* Owner)
  : TDemoBasicMainForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TRowsMultiEditorsDemoMainForm::erCarImagePropertiesGetEditingProperties(
  TcxCustomEditorRowProperties *Sender, int ARecordIndex,
  TcxCustomEditProperties *&AProperties)
{
  AProperties = erepCarPictEditing->Properties;
}
//---------------------------------------------------------------------------

void __fastcall TRowsMultiEditorsDemoMainForm::erCarImagePropertiesGetEditProperties(
  TcxCustomEditorRowProperties *Sender, int ARecordIndex,
  TcxCustomEditProperties *&AProperties)
{
  AProperties = erepCarPictEdit->Properties;
}
//---------------------------------------------------------------------------

void __fastcall TRowsMultiEditorsDemoMainForm::merPaymentDescrEditors2GetEditingProperties(
  TcxCustomEditorRowProperties *Sender, int ARecordIndex,
  TcxCustomEditProperties *&AProperties)
{
  AProperties = erepPaymentAmountEditing->Properties;
}
//---------------------------------------------------------------------------

void __fastcall TRowsMultiEditorsDemoMainForm::miLayoutStyleClick(TObject *Sender)
{
  ((TMenuItem*)Sender)->Checked = true;
  cxDBVerticalGrid->LayoutStyle = (TcxvgLayoutStyle)((TMenuItem*)Sender)->Tag;
}
//---------------------------------------------------------------------------



