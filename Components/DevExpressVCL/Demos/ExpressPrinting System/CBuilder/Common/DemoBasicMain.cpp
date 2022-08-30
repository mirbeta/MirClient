//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "DemoBasicMain.h"
#include "AboutDemoForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxDrawTextUtils"
#pragma link "cxControls"
#pragma link "cxClasses"
#pragma link "cxLookAndFeels"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
#pragma link "dxBkgnd"
#pragma link "dxPrnDev"
#pragma link "dxPrnPg"
#pragma link "dxPSCompsProvider"
#pragma link "dxPSCore"
#pragma link "dxPScxEditorProducers"
#pragma link "dxPScxExtEditorProducers"
#pragma link "dxPScxPageControlProducer"
#pragma link "dxPSEdgePatterns"
#pragma link "dxPSEngn"
#pragma link "dxPSFillPatterns"
#pragma link "dxPSGlbl"
#pragma link "dxPSPDFExport"
#pragma link "dxPSPDFExportCore"
#pragma link "dxPSPrVwStd"
#pragma link "dxPSUtl"
#pragma link "dxWrap"
#pragma link "cxImageList"
#pragma resource "*.dfm"
TDemoBasicMainForm *DemoBasicMainForm;
//---------------------------------------------------------------------------
__fastcall TDemoBasicMainForm::TDemoBasicMainForm(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TDemoBasicMainForm::actDXOnTheWebExecute(TObject *Sender)
{
  ShellExecute(Handle, "OPEN", "http://www.devexpress.com", NULL, NULL, SW_SHOWMAXIMIZED);
}
//---------------------------------------------------------------------------
void __fastcall TDemoBasicMainForm::actExitExecute(TObject *Sender)
{
  Close();
}
//---------------------------------------------------------------------------
void __fastcall TDemoBasicMainForm::actShowDemoDescriptionExecute(TObject *Sender)
{
  lbDescrip->Visible = !lbDescrip->Visible;
  ((TCustomAction*)Sender)->Checked = !((TCustomAction*)Sender)->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TDemoBasicMainForm::actPageSetupExecute(TObject *Sender)
{
  dxComponentPrinter->PageSetup(NULL);
}
//---------------------------------------------------------------------------
void __fastcall TDemoBasicMainForm::actPreviewExecute(TObject *Sender)
{
  dxComponentPrinter->Preview(true, NULL);
}
//---------------------------------------------------------------------------
void __fastcall TDemoBasicMainForm::actPrintExecute(TObject *Sender)
{
  dxComponentPrinter->Print(true, NULL, NULL);
}
//---------------------------------------------------------------------------
void __fastcall TDemoBasicMainForm::actDesignerExecute(TObject *Sender)
{
  dxComponentPrinter->DesignReport(NULL);
}
//---------------------------------------------------------------------------
void __fastcall TDemoBasicMainForm::actAboutExecute(TObject *Sender)
{
  ShowAboutDemoForm();
}
//---------------------------------------------------------------------------
TMenuItem* __fastcall TDemoBasicMainForm::CreateStyleItem(TComponent *AOwner, String ACaption, int ATag)
{
	TMenuItem* ANewItem = new TMenuItem(AOwner);
	ANewItem->Caption = ACaption;
	ANewItem->RadioItem = True;
	ANewItem->GroupIndex = 2;
	ANewItem->Tag = ATag;
	ANewItem->OnClick = PreviewDialogStyleClick;
	if (dxPSEngineController1->PreviewDialogStyle == ACaption)
	  ANewItem->Checked = True;
	return(ANewItem);
}
//---------------------------------------------------------------------------
void __fastcall TDemoBasicMainForm::FormCreate(TObject *Sender)
{
  miAntiAliasing->ImageIndex = -1;
  FSmoothlyStretchImages = actImageAntiAliasing->Checked;
  miPreviewDialogStyles->Clear();
  int i;
  for (i = 0; i < dxPSPreviewDialogManager()->Count; i++)
	{miPreviewDialogStyles->Add(CreateStyleItem(miPreviewDialogStyles,
	  dxPSPreviewDialogManager()->Names[i], i));};
  miNativeStyle->Checked = dxPSEngineController1->DialogsLookAndFeel->NativeStyle;
  for (i = 0; i < miKind->Count; i++)
	{miKind->Items[i]->Checked =
	  dxPSEngineController1->DialogsLookAndFeel->Kind == miKind->Items[i]->Tag;}
}
//---------------------------------------------------------------------------

void __fastcall TDemoBasicMainForm::DialogsLookAndFeelChanged(TObject *Sender)
{
  if (((TMenuItem*)Sender)->Tag < 4)
  {
	((TMenuItem*)Sender)->Checked = true;
	dxPSEngineController1->DialogsLookAndFeel->Kind = TcxLookAndFeelKind(((TMenuItem*)Sender)->Tag);
	miNativeStyle->Checked = false;
  };
  dxPSEngineController1->DialogsLookAndFeel->NativeStyle = miNativeStyle->Checked;
}
//---------------------------------------------------------------------------


void __fastcall TDemoBasicMainForm::AlwaysEnabled(TObject *Sender)
{
  ((TCustomAction*)Sender)->Enabled = true;
}
//---------------------------------------------------------------------------

void __fastcall TDemoBasicMainForm::PreviewDialogStyleClick(TObject *Sender)
{
  dxPSEngineController1->PreviewDialogStyle =
	dxPSPreviewDialogManager()->Names[((TMenuItem*)Sender)->Tag];
  ((TMenuItem*)Sender)->Checked = true;
}
void __fastcall TDemoBasicMainForm::actImageAntiAliasingExecute(TObject *Sender)
{
  FSmoothlyStretchImages = actImageAntiAliasing->Checked;
}
//---------------------------------------------------------------------------

