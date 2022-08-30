//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "BaseForm.h"
#include "AboutDemoForm.h"
#include "DemoUtils.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxControls"
#pragma link "cxClasses"
#pragma link "cxLookAndFeels"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
#pragma link "dxSpreadSheet"
#pragma resource "*.dfm"
TfmBaseForm *fmBaseForm;
//---------------------------------------------------------------------------
__fastcall TfmBaseForm::TfmBaseForm(TComponent* Owner)
	: TForm(Owner)
{
  FLookAndFeelController = new TcxLookAndFeelController(this);
}
//---------------------------------------------------------------------------}
//---------------------------------------------------------------------------

void __fastcall TfmBaseForm::miSaveAsClick(TObject *Sender)
{
		if (SaveDialog->Execute())
			GetSpreadSheet()->SaveToFile(SaveDialog->FileName);
}
//---------------------------------------------------------------------------

void __fastcall TfmBaseForm::miExitClick(TObject *Sender)
{
		Close();
}
//---------------------------------------------------------------------------

TdxSpreadSheet* TfmBaseForm::GetSpreadSheet()
{
	return NULL;
}
//---------------------------------------------------------------------------

void __fastcall TfmBaseForm::miAboutClick(TObject *Sender)
{
  TRect R = cxNullRect;
  if (WindowState == wsMaximized) {
	R = cxRectSetOrigin(GetSpreadSheet()->Bounds, GetSpreadSheet()->ClientToScreen(cxNullPoint));
	R = cxRectSetRight(R, R.Right - 30, 400);
	R = cxRectSetTop(R, R.Top + 30, 500);
  }
  ShowAboutDemoFormEx(R);
}
//---------------------------------------------------------------------------

void __fastcall TfmBaseForm::miShowFormulasClick(TObject *Sender)
{
  GetSpreadSheet()->BeginUpdate();
  try
  {
	for (int I = 0; I < GetSpreadSheet()->SheetCount - 1; I++)
		if (dynamic_cast<TdxSpreadSheetTableView*>(GetSpreadSheet()->Sheets[I]))
	    	((TdxSpreadSheetTableView*)(GetSpreadSheet()->Sheets[I]))->Options->ShowFormulas = bDefault;
	GetSpreadSheet()->OptionsView->ShowFormulas = !(GetSpreadSheet()->OptionsView->ShowFormulas);
	GetSpreadSheet()->ActiveSheetAsTable->Options->ShowFormulas = (TdxDefaultBoolean)GetSpreadSheet()->OptionsView->ShowFormulas;
  }
  __finally
  {
    GetSpreadSheet()->EndUpdate();
  };

}
//---------------------------------------------------------------------------

void __fastcall TfmBaseForm::FormCreate(TObject *Sender)
{
  SetDefaultLookAndFeel();
  AddLookAndFeelMenu();
  CreateTouchModeMenuOption();
  GetSpreadSheet()->OptionsBehavior->History = true;
}
//---------------------------------------------------------------------------

void __fastcall TfmBaseForm::AddLookAndFeelMenu()
{
  mmMain->Items->Insert(mmMain->Items->IndexOf(miOptions),
	CreateLookAndFeelMenuItems(mmMain->Items, FLookAndFeelController));
}
//---------------------------------------------------------------------------

void __fastcall TfmBaseForm::CreateTouchModeMenuOption()
{
  TMenuItem* AItem = new TMenuItem(miOptions);
  AItem->Caption = "TouchMode";
  AItem->RadioItem = false;
  AItem->AutoCheck = true;
  miOptions->Add(AItem);
  AItem->OnClick = TouchModeClick;
}
//---------------------------------------------------------------------------

TcxLookAndFeelKind __fastcall TfmBaseForm::GetDefaultLookAndFeelKind()
{
  return(lfOffice11);
}

bool __fastcall TfmBaseForm::IsNativeDefaultStyle()
{
  return(false);
}

void __fastcall TfmBaseForm::SetDefaultLookAndFeel()
{
  FLookAndFeelController->NativeStyle = IsNativeDefaultStyle();
  FLookAndFeelController->Kind = GetDefaultLookAndFeelKind();
}
//---------------------------------------------------------------------------

void __fastcall TfmBaseForm::TouchModeClick(TObject *Sender)
{
  FLookAndFeelController->TouchMode = static_cast<TMenuItem*>(Sender)->Checked;
}
//---------------------------------------------------------------------------

