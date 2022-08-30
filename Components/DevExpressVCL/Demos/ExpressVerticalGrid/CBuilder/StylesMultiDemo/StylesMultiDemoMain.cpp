//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "StylesMultiDemoMain.h"
#include "StylesMultiDemoData.h"
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
#pragma link "cxButtons"
#pragma link "cxDBVGrid"
#pragma link "cxInplaceContainer"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxStyleSheetEditor"
#pragma link "cxVGrid"
#pragma link "cxVGridStyleSheetPreview"
#pragma resource "*.dfm"
TStylesMultiDemoMainForm *StylesMultiDemoMainForm;
//---------------------------------------------------------------------------
__fastcall TStylesMultiDemoMainForm::TStylesMultiDemoMainForm(TComponent* Owner)
  : TDemoBasicMainForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TStylesMultiDemoMainForm::FormCreate(TObject *Sender)
{
  TDemoBasicMainForm::FormCreate(Sender);
  CreateStyleSheetsList(shtUserDefined);
  CreateStyleSheetsList(shtPredefined);
  SelectFistChild(shtUserDefined);
  cxDBVerticalGrid->FullExpand();
}
//---------------------------------------------------------------------------

void __fastcall TStylesMultiDemoMainForm::actSaveToFileExecute(TObject *Sender)
{
  if (SaveDialog->Execute())
    SaveUserDefinedStyleSheets(SaveDialog->FileName);
}
//---------------------------------------------------------------------------

void __fastcall TStylesMultiDemoMainForm::actLoadFromFileExecute(TObject *Sender)
{
  if (OpenDialog->Execute()){
    LoadUserDefinedStyleSheets(OpenDialog->FileName);
    SelectFistChild(shtUserDefined);
  }
}
//---------------------------------------------------------------------------

void __fastcall TStylesMultiDemoMainForm::actEditStyleSheetExecute(TObject *Sender)
{
  ShowcxStyleSheetEditor(GetCurrentStyleSheet(), NULL);
}
//---------------------------------------------------------------------------

void __fastcall TStylesMultiDemoMainForm::FormActivate(TObject *Sender)
{
  OpenDialog->InitialDir = ExtractFileDir(Application->ExeName);
  SaveDialog->InitialDir = OpenDialog->InitialDir;
}
//---------------------------------------------------------------------------

void __fastcall TStylesMultiDemoMainForm::OnGetDisplayText(
  TcxCustomEditorRowProperties *Sender, int ARecord,
  String &AText)
{
  AText = Sender->Caption;
}
//---------------------------------------------------------------------------

void __fastcall TStylesMultiDemoMainForm::vgStyleSheetsItemChanged(TObject *Sender,
  TcxCustomRow *AOldRow, int AOldCellIndex)
{
  if ((vgStyleSheets->FocusedRow != NULL) && (vgStyleSheets->FocusedRow->Tag != -1)){
    UpdateGridStyleSheets(((TcxVerticalGridStyleSheet*)vgStyleSheets->FocusedRow->Tag));
    vgStyleSheets->Update();
  }
}
//---------------------------------------------------------------------------

void __fastcall TStylesMultiDemoMainForm::actEditAndSaveStyleSheetUpdate(TObject *Sender)
{
  ((TCustomAction*)Sender)->Enabled = (vgStyleSheets->FocusedRow->Parent != NULL) &&
    (vgStyleSheets->FocusedRow->Parent == vgStyleSheetsUserDefinedStyleSheets);
}
//---------------------------------------------------------------------------

void __fastcall TStylesMultiDemoMainForm::vgStyleSheetsStylesGetCategoryStyle(
  TObject *Sender, TcxCustomRow *ARow, TcxStyle *&AStyle)
{
   if ((vgStyleSheets->FocusedRow == ARow))
    AStyle = StylesMultiDemoDataDM->cxVerticalGridStyleSheetDevExpress->Styles->Selection;
}
//---------------------------------------------------------------------------

void __fastcall TStylesMultiDemoMainForm::vgStyleSheetsDrawRowHeader(
  TObject *Sender, TcxCanvas *ACanvas, TcxvgPainter *APainter,
  TcxCustomRowHeaderInfo *AHeaderViewInfo, bool &Done)
{
  AHeaderViewInfo->FocusRect = Rect(0, 0, 0, 0);
}
//---------------------------------------------------------------------------

TcxVerticalGridStyleSheet* __fastcall TStylesMultiDemoMainForm::GetCurrentStyleSheet()
{
  return ((TcxVerticalGridStyleSheet*)cxDBVerticalGrid->Styles->StyleSheet);
}
//---------------------------------------------------------------------------

class TcxCustomRowAccess : public TcxCustomRow
{
public:
	inline void __fastcall RemoveAll(void) {TcxCustomRow::RemoveAll();};
};

void __fastcall TStylesMultiDemoMainForm::CreateStyleSheetsList(
  TcxStyleRepositoryType AStyleRepositoryType)
{
  TcxStyleRepository *AStyleRepository;
  TcxCategoryRow *ACategory;
  TcxCategoryRow *ARow;
  if (AStyleRepositoryType == shtNone)
    return;
  if (AStyleRepositoryType == shtUserDefined){
    AStyleRepository = StylesMultiDemoDataDM->strepUserDefined;
    ACategory = vgStyleSheetsUserDefinedStyleSheets;
  }
  else{
    AStyleRepository = StylesMultiDemoDataDM->strepPredefined;
    ACategory = vgStyleSheetsPredefinedStyleSheets;
  }

  ((TcxCustomRowAccess*)ACategory)->RemoveAll();
  for (int I = 0; I < AStyleRepository->StyleSheetCount; I++){
    ARow = (TcxCategoryRow*)vgStyleSheets->AddChild(ACategory, __classid(TcxCategoryRow));
    ARow->Properties->Caption = AStyleRepository->StyleSheets[I]->Caption;
    ARow->Options->TabStop = false;
    ARow->Styles->Header =
      StylesMultiDemoDataDM->cxVerticalGridStyleSheetDevExpress->Styles->Content;
    ARow->Tag = (int)AStyleRepository->StyleSheets[I];
  }
}
//---------------------------------------------------------------------------

void __fastcall TStylesMultiDemoMainForm::UpdateGridStyleSheets(
  TcxVerticalGridStyleSheet *AStyleSheet)
{
  if (GetCurrentStyleSheet() == AStyleSheet) return;
  cxDBVerticalGrid->Styles->StyleSheet = AStyleSheet;
  if (AStyleSheet != NULL)
    pnlCurrentStyleSheet->Caption = AStyleSheet->Caption;
  else
    pnlCurrentStyleSheet->Caption = "None";
  cxDBVerticalGrid->Update();
}
//---------------------------------------------------------------------------

void __fastcall TStylesMultiDemoMainForm::ClearUserDefinedStyleSheets()
{
  TcxStyleRepository *ARepository = StylesMultiDemoDataDM->strepUserDefined;
  ARepository->Clear();
  ARepository->ClearStyleSheets();
}
//---------------------------------------------------------------------------

void __fastcall TStylesMultiDemoMainForm::LoadUserDefinedStyleSheets(
  TFileName AFileName)
{
  UpdateGridStyleSheets(NULL);
  ClearUserDefinedStyleSheets();

  LoadStyleSheetsFromIniFile(AFileName, StylesMultiDemoDataDM->strepUserDefined,
    __classid(TcxVerticalGridStyleSheet), NULL, NULL, NULL, NULL);

  CreateStyleSheetsList(shtUserDefined);
}
//---------------------------------------------------------------------------

void __fastcall TStylesMultiDemoMainForm::SaveUserDefinedStyleSheets(
  TFileName AFileName)
{
  TList *AList = new TList();
  try{
    PopulateStyleSheetsList(AList);
    SaveStyleSheetsToIniFile(AFileName, AList);
  }
  __finally{
    delete AList;
  }
}
//---------------------------------------------------------------------------

void __fastcall TStylesMultiDemoMainForm::SelectFistChild(
  TcxStyleRepositoryType AStyleRepositoryType)
{
  switch (AStyleRepositoryType){
    case shtNone: vgStyleSheets->FocusedRow = vgStyleSheetsNone; break;
    case shtPredefined:
      vgStyleSheets->FocusedRow = vgStyleSheetsPredefinedStyleSheets->Rows[0];
      break;
    case shtUserDefined:
      vgStyleSheets->FocusedRow = vgStyleSheetsUserDefinedStyleSheets->Rows[0];
      break;
  }
}
//---------------------------------------------------------------------------


