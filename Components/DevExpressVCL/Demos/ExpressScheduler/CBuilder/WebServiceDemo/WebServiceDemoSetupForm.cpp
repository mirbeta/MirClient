//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "WebServiceDemoSetupForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxContainer"
#pragma link "cxControls"
#pragma link "cxEdit"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma link "cxMemo"
#pragma link "cxRichEdit"
#pragma link "cxTextEdit"
#pragma link "dxCustomWizardControl"
#pragma link "dxLayoutContainer"
#pragma link "dxLayoutControl"
#pragma link "dxLayoutcxEditAdapters"
#pragma link "dxWizardControl"
#pragma link "cxButtons"
#pragma link "dxLayoutControlAdapters"
#pragma link "dxLayoutLookAndFeels"
#pragma resource "*.dfm"
TWebServiceDemoSetupWizard *WebServiceDemoSetupWizard;
//---------------------------------------------------------------------------
__fastcall TWebServiceDemoSetupWizard::TWebServiceDemoSetupWizard(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TWebServiceDemoSetupWizard::FormCreate(TObject *Sender)
{
	reAbout->Lines->LoadFromFile(ExtractFilePath(Application->ExeName) + "Wizard.rtf");
	reMSGraph->Lines->LoadFromFile(ExtractFilePath(Application->ExeName) + "Wizard-MSGraph.rtf");
	reGoogleApi->Lines->LoadFromFile(ExtractFilePath(Application->ExeName) + "Wizard-GoogleAPI.rtf");
}
//---------------------------------------------------------------------------
void __fastcall TWebServiceDemoSetupWizard::reURLClick(TcxCustomRichEdit *Sender,
          const UnicodeString URLText, TMouseButton Button)
{
  	dxShellExecute(Sender->Handle, URLText);
}
//---------------------------------------------------------------------------
void __fastcall TWebServiceDemoSetupWizard::teChange(TObject *Sender)
{
	UpdateStateButtons();
}
//---------------------------------------------------------------------------
void __fastcall TWebServiceDemoSetupWizard::UpdateStateButtons()
{
  btnStart->Enabled = ((Trim(teMSGraphClientID->Text) != "") && (Trim(teMSGraphClientSecret->Text) != "")) ||
	((Trim(teGoogleApiClientID->Text) != "") && (Trim(teGoogleApiClientSecret->Text) != ""));
}
//---------------------------------------------------------------------------

