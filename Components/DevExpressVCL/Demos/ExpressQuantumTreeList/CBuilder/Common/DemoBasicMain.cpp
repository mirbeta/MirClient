//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "DemoBasicMain.h"
#include "DemoRating.h"
#include "AboutDemoForm.h"
#include "DemoUtils.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxControls"
#pragma link "cxClasses"
#pragma link "cxLookAndFeels"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
#pragma resource "*.dfm"
TDemoBasicMainForm *DemoBasicMainForm;
//---------------------------------------------------------------------------
__fastcall TDemoBasicMainForm::TDemoBasicMainForm(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TDemoBasicMainForm::actAboutExecute(TObject *Sender)
{
  ShowAboutDemoForm();
}
//---------------------------------------------------------------------------

void __fastcall TDemoBasicMainForm::miTouchModeClick(TObject *Sender)
{
  cxLookAndFeelController->TouchMode = miTouchMode->Checked;
}

void __fastcall TDemoBasicMainForm::actDXOnTheWebExecute(TObject *Sender)
{
  ShowWebPage((TdxWebPageType)(((TComponent*)Sender)->Tag));
}
//---------------------------------------------------------------------------

void __fastcall TDemoBasicMainForm::actExitExecute(TObject *Sender)
{
  Close();
}
//---------------------------------------------------------------------------


void __fastcall TDemoBasicMainForm::actHelpExecute(TObject *Sender)
{
  Application->HelpCommand(HELP_FINDER, 0);
}
//---------------------------------------------------------------------------


void __fastcall TDemoBasicMainForm::actRateDemoExecute(TObject *Sender)
{
  TDemoRatingForm *AForm = new TDemoRatingForm(this);
    __try{
      AForm->ShowModal();
    }
    __finally{
      AForm->Free();
    }
}
//---------------------------------------------------------------------------

void __fastcall TDemoBasicMainForm::actShowDemoDescriptionExecute(TObject *Sender)
{
  lscrip->Visible = !lscrip->Visible;
  ((TCustomAction*)Sender)->Checked = !((TCustomAction*)Sender)->Checked;
}
//---------------------------------------------------------------------------



void __fastcall TDemoBasicMainForm::ExportToClick(TObject *Sender)
{
  TcxCustomTreeList* ATreeList = GetTreeList();
  if ((ATreeList != NULL) && SaveDialog1->Execute())
	switch (((TMenuItem*)Sender)->Tag){
	  case 0:
		cxExportTLToExcel(SaveDialog1->FileName, ATreeList); break;
	  case 1:
		cxExportTLToText(SaveDialog1->FileName, ATreeList); break;
	  case 2:
		cxExportTLToHTML(SaveDialog1->FileName, ATreeList); break;
	  case 3:
	   	cxExportTLToXML(SaveDialog1->FileName, ATreeList); break;
	  case 4:
	   	cxExportTLToXLSX(SaveDialog1->FileName, ATreeList); break;
	 };
}
//---------------------------------------------------------------------------

TcxCustomTreeList* __fastcall TDemoBasicMainForm::GetTreeList()
{
  for (int i = 0; ControlCount - 1; i++)
  {
	if (dynamic_cast<TcxCustomTreeList*>(Controls[i]) != NULL)
	  return (TcxCustomTreeList*)(Controls[i]);
  };
  return NULL;
}
void __fastcall TDemoBasicMainForm::FormCreate(TObject *Sender)
{
  SetDefaultLookAndFeel();
  AddLookAndFeelMenu();
  miTouchMode->Checked = cxIsTouchModeEnabled;
}

void __fastcall TDemoBasicMainForm::AddLookAndFeelMenu()
{
  miOptions->Insert(miOptions->IndexOf(miSeparator2),
	CreateLookAndFeelMenuItems(miOptions, cxLookAndFeelController));
}

TcxLookAndFeelKind __fastcall TDemoBasicMainForm::GetDefaultLookAndFeelKind()
{
  return(lfUltraFlat);
}

bool __fastcall TDemoBasicMainForm::IsNativeDefaultStyle()
{
  return(true);
}

void __fastcall TDemoBasicMainForm::SetDefaultLookAndFeel()
{
  cxLookAndFeelController->NativeStyle = IsNativeDefaultStyle();
  cxLookAndFeelController->Kind = GetDefaultLookAndFeelKind();
}

//---------------------------------------------------------------------------

