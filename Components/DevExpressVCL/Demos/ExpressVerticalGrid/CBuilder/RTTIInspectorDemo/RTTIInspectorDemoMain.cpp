//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "RTTIInspectorDemoMain.h"
#include "RTTIInspectorDemoData.h"
#include "RTTIInspectorDemoVGEditor.h"
#include "cxExportVGLink.hpp"
#include "cxImageComboBox.hpp"
#include <assert.h>
#include <ImgList.hpp>
#include "RTTIInspectorDemoPropEditors.h"
#include "dxCore.hpp"
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
#pragma link "cxInplaceContainer"
#pragma link "cxOI"
#pragma link "cxVGrid"
#pragma link "cxLookAndFeelPainters"
#pragma resource "*.dfm"
TRTTIInspectorDemoMainForm *RTTIInspectorDemoMainForm;
//---------------------------------------------------------------------------

PTypeInfo cxGetTypeInfo(PTypeInfo ATypeInfo, String APropName)
{
  PPTypeInfo Temp;
  Temp = GetPropInfo(ATypeInfo, APropName)->PropType;
  assert(Temp != NULL);
  return *Temp;
}

__fastcall TRTTIInspectorDemoMainForm::TRTTIInspectorDemoMainForm(TComponent* Owner)
  : TDemoBasicMainForm(Owner)
{
  PTypeInfo AInfo = cxGetTypeInfo(__typeinfo(TcxCustomEditorRowProperties),
    "EditPropertiesClassName");
  cxRegisterPropertyEditor(AInfo, __classid(TcxCustomEditorRowProperties),
    "EditPropertiesClassName", NULL);
  AInfo = cxGetTypeInfo(__typeinfo(TcxCustomEditorRowProperties), "ImageIndex");
  cxRegisterPropertyEditor(AInfo , __classid(TcxCustomEditorRowProperties),
    "ImageIndex", __classid(TcxImageIndexProperty));
  cxRegisterEditPropertiesClass(__classid(TcxImageIndexProperty),
    __classid(TcxImageComboBoxProperties));
}
//---------------------------------------------------------------------------

void __fastcall TRTTIInspectorDemoMainForm::actVGEditExecute(TObject *Sender)
{
  GetVerticalGridEditor(cxDBVerticalGrid, FSelectedObjectEvent)->Show();
}
//---------------------------------------------------------------------------

void __fastcall TRTTIInspectorDemoMainForm::FormCreate(TObject *Sender)
{
  TDemoBasicMainForm::FormCreate(Sender);
  FSelectedObjectEvent = ObjectSelected;
  ObjectSelected(cxDBVerticalGrid);
}
//---------------------------------------------------------------------------

void __fastcall TRTTIInspectorDemoMainForm::cxRTTIInspectorPropertyChanged(TObject *Sender)
{
  if (((TcxEditorRow*)((TcxRTTIInspector*)Sender)->FocusedRow)->Properties->Caption == "Name")
    GetVerticalGridEditor(cxDBVerticalGrid, FSelectedObjectEvent)->DoItemsModified();
}
//---------------------------------------------------------------------------

void __fastcall TRTTIInspectorDemoMainForm::cxVerticalGridClick(TObject *Sender)
{
  ObjectSelected(Sender);
}
//---------------------------------------------------------------------------

void __fastcall TRTTIInspectorDemoMainForm::actExportVGToHTMLExecute(TObject *Sender)
{
  cxExportVGToHTML("temp.html", cxDBVerticalGrid, false, 8, "html");
  dxShellExecute("temp.html");
}
//---------------------------------------------------------------------------

void __fastcall TRTTIInspectorDemoMainForm::actExportVGToExcelExecute(TObject *Sender)
{
  cxExportVGToXLSX("temp.xlsx", cxDBVerticalGrid, false, true, 8, "xlsx");
  ShellExecute(Handle, "OPEN", "temp.xlsx", NULL, NULL, SW_SHOWMAXIMIZED);
}
//---------------------------------------------------------------------------

void __fastcall TRTTIInspectorDemoMainForm::cxDBVerticalGridLayoutChanged(TObject *Sender)
{
  GetVerticalGridEditor(cxDBVerticalGrid, FSelectedObjectEvent)->DoItemsModified();
}
//---------------------------------------------------------------------------

void __fastcall TRTTIInspectorDemoMainForm::actShowCustomizeExecute(TObject *Sender)
{
  cxDBVerticalGrid->Customizing->Visible = true;
}
//---------------------------------------------------------------------------

void __fastcall TRTTIInspectorDemoMainForm::actAboutExecute(TObject *Sender)
{
  ShowAbout(false, true);
}
//---------------------------------------------------------------------------

void __fastcall TRTTIInspectorDemoMainForm::ObjectSelected(TObject *Sender)
{
  cxRTTIInspector->InspectedObject = (TPersistent*)Sender;
  UpdateLabel((TComponent*)Sender);
}
//---------------------------------------------------------------------------

void __fastcall TRTTIInspectorDemoMainForm::UpdateLabel(TComponent *AObject)
{
  if (AObject != NULL) {
    lbInspectedObject->Caption = AObject->Name + ": " + AObject->ClassName();
  } else {
    lbInspectedObject->Caption = "<not selected>";
  }
}
//---------------------------------------------------------------------------


