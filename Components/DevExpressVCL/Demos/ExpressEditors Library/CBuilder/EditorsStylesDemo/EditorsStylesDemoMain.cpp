//---------------------------------------------------------------------------

#include <vcl.h>
#include <shellapi.hpp>
#pragma hdrstop

#include "EditorsStylesDemoMain.h"
#include "EditorsStylesDemoUtils.h"
#include "AboutDemoForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxContainer"
#pragma link "cxControls"
#pragma link "cxDropDownEdit"
#pragma link "cxEdit"
#pragma link "cxFontNameComboBox"
#pragma link "cxGroupBox"
#pragma link "cxHint"
#pragma link "cxLookAndFeels"
#pragma link "cxMaskEdit"
#pragma link "cxMemo"
#pragma link "cxSpinEdit"
#pragma link "cxSplitter"
#pragma link "cxTextEdit"
#pragma link "cxTreeView"
#pragma link "cxPC"
#pragma link "cxClasses"
#pragma link "cxGraphics"
#pragma link "cxGridCardView"
#pragma link "cxGridTableView"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxStyles"
#pragma link "dxCustomHint"
#pragma resource "*.dfm"
TEditorsStylesDemoDemoMainForm *EditorsStylesDemoDemoMainForm;
TEditorsStylesDemoBaseFrame* ActiveFrame;
//---------------------------------------------------------------------------
__fastcall TEditorsStylesDemoDemoMainForm::TEditorsStylesDemoDemoMainForm(TComponent* Owner)
  : TfmBaseForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TEditorsStylesDemoDemoMainForm::FormShow(TObject *Sender)
{
  RegisterFrames();
  AdjustDescriptionsPageControl();
  FillTreeViewItems();
  cxTreeView->Items->GetFirstNode()->Expand(true);
  cxTreeView->Items->GetFirstNode()->Selected = true;
  cxTreeViewChange(Sender, cxTreeView->Items->GetFirstNode());
  FDefaultHintStyle = false;
  FDefaultDisplayStyle = false;
  FCurrentDisplayStyle = shtLightBlue;
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoDemoMainForm::RegisterFrames()
{
  EditorsStylesDemoFrameManager()->AddFrame(CreateFrameByID(TextProcessingFrameID));
  EditorsStylesDemoFrameManager()->AddFrame(CreateFrameByID(ImageProcessingFrameID));
  EditorsStylesDemoFrameManager()->AddFrame(CreateFrameByID(SolarSystemFrameID));
  EditorsStylesDemoFrameManager()->AddFrame(CreateFrameByID(NoteBookFrameID));
  EditorsStylesDemoFrameManager()->AddFrame(CreateFrameByID(IssuesFrameID ));
  EditorsStylesDemoFrameManager()->AddFrame(CreateFrameByID(StylesPaletteFrameID));
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoDemoMainForm::AddChildNodes(TcxExtEditorTypes AExtEditorTypes, TStrings* AStrings, TTreeNode* ANode)
{
  cxGetEditorsNamesListByTypes(AStrings, AExtEditorTypes);
  for (int i=0; i < AStrings->Count; i++)
    cxTreeView->Items->AddChild(ANode, AStrings->Strings[i]);
  ANode->AlphaSort();  
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoDemoMainForm::AdjustDescriptionsPageControl()
{
  for (int I = 0; I < EditorsStylesDemoFrameManager()->FramesCount; I++){
    TcxTabSheet *APage = new TcxTabSheet(PageControl);
    APage->PageControl = PageControl;
    TEditorsStylesDemoBaseFrame *AFrame = EditorsStylesDemoFrameManager()->Frames[I];
    APage->Caption = AFrame->BriefName();
    AFrame->pnlDescription->Parent = APage;
    AFrame->pnlDescription->Visible = true;
    AFrame->pnlDescription->Align = alClient;
    AFrame->memDescrip->Properties->WordWrap = true;
    APage->ImageIndex = APage->TabIndex;
  }
}

void __fastcall TEditorsStylesDemoDemoMainForm::FillTreeViewItems()
{
  cxTreeView->Items->Clear();
  TStringList* AStrings = new TStringList();
  TTreeNode* ANode;
  for (int i=0; i < EditorsStylesDemoFrameManager()->FramesCount; i++) {
    EditorsStylesDemoFrameManager()->Frames[i]->OnFileNameChanged = UpdateFileNameStatusPanel;
    ANode = cxTreeView->Items->AddChildObject(NULL,
      EditorsStylesDemoFrameManager()->Frames[i]->Name(), EditorsStylesDemoFrameManager()->Frames[i]);
    AddChildNodes(EditorsStylesDemoFrameManager()->Frames[i]->
      GetExtEditorTypes(EditorsStylesDemoFrameManager()->Frames[i]), AStrings, ANode);
  }
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoDemoMainForm::FormDestroy(TObject *Sender)
{
  delete EditorsStylesDemoFrameManager();
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoDemoMainForm::UpdateCurrentHintStyle(TcxHintType AHintType, bool ADefaultHintStyle)
{
  TMenuItem* AMenuItem;
  if (!ADefaultHintStyle)
  {
		UncheckMenuItems(miDefHintStyle);
		miCurHintStyle->Items[(int)AHintType]->Checked = true;
  }
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoDemoMainForm::AdjustFrameDisplayStyle(TEditorsStylesDemoBaseFrame* AFrame,
  bool ADefaultDisplayStyle)
{
  if (ADefaultDisplayStyle)
    ActiveFrame->ChangeDisplayStyle(FCurrentDisplayStyle);
  else {
    miCurDisplayStyle->Items[(int)AFrame->DisplayStyle]->Checked = true;
    AFrame->DisplayStyle = AFrame->DisplayStyle;
  }
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoDemoMainForm::AdjustFileNameStatusPanel()
{
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoDemoMainForm::AdjustNotePane(TEditorsStylesDemoBaseFrame* AFrame)
{
  gbDescription->Color = AFrame->GetStyleBackgroundColor();
  gbDescription->CaptionBkColor = AFrame->GetStyleBackgroundColor();
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoDemoMainForm::AdjustFileMenu()
{
  miFileOpen->Visible = ActiveFrame->MenuOpenFileVisible();
  miFileSave->Visible = ActiveFrame->MenuSaveFileVisible();
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoDemoMainForm::cxTreeViewChange(
      TObject *Sender, TTreeNode *Node)
{
  String AControlClassName;
  if (Node)
  {
	if (Node->Parent)
	{
      AControlClassName = Node->Text;
      Node = Node->Parent;
    }
    TEditorsStylesDemoBaseFrame* AFrame = (TEditorsStylesDemoBaseFrame*)Node->Data;
    if (ActiveFrame)
      ActiveFrame->Flickering = false;
    if ((AFrame) && (AControlClassName != NULL))
      AFrame->FlickerControls(AControlClassName);
	if (ActiveFrame != AFrame)
	{
	  if ((ActiveFrame) && (AFrame))
	  {
        ActiveFrame->Visible = false;
        ActiveFrame->Parent = NULL;
      }
      ActiveFrame = AFrame;
      ActiveFrame->Parent = cxGroupBox;
      ActiveFrame->Visible = true;
	  UpdateCurrentHintStyle(ActiveFrame->HintStyle, DefaultHintStyle);
	  AdjustFrameDisplayStyle(ActiveFrame, DefaultDisplayStyle);
	  AdjustFileMenu();
	  AdjustNotePane(AFrame);
	  PageControl->ActivePage = PageControl->Pages[Node->Index];
	  AdjustFileNameStatusPanel();
	  miStyle->Visible = AFrame->StyleMenuVisible();
	}
  }
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoDemoMainForm::UncheckMenuItems(TMenuItem* AItems)
{
  for (int i=0; i < AItems->Count; i++)
    AItems->Items[i]->Checked = false;
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoDemoMainForm::SetLightInfoHintStyle()
{
  if (!ShowHint) ShowHint = true;
  AdvancedHintStyle->Standard = false;
  AdvancedHintStyle->Animate = cxhaFadeIn;
  AdvancedHintStyle->Color = clInfoBk;
  AdvancedHintStyle->Font->Color = clBlack;
  AdvancedHintStyle->Font->Height = -11;
  AdvancedHintStyle->Font->Style.Clear();
  AdvancedHintStyle->IconType = cxhiInformation;
  AdvancedHintStyle->IconSize = cxisDefault;
  AdvancedHintStyle->Rounded = false;
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoDemoMainForm::SetLightSlideLeftHintStyle()
{
  if (!ShowHint) ShowHint = true;
  AdvancedHintStyle->Standard = false;
  AdvancedHintStyle->Animate = cxhaSlideFromLeft;
  AdvancedHintStyle->Color = TColor(0x00EDCBB6);
  AdvancedHintStyle->Font->Color = clBlack;
  AdvancedHintStyle->Font->Height = -13;
  AdvancedHintStyle->Font->Style.Clear();
  AdvancedHintStyle->IconType = cxhiCurrentApplication;
  AdvancedHintStyle->IconSize = cxisSmall;
  AdvancedHintStyle->Rounded = false;
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoDemoMainForm::SetBlueSlideUpHintStyle()
{
  if (!ShowHint) ShowHint = true;
  AdvancedHintStyle->Standard = false;
  AdvancedHintStyle->Animate = cxhaSlideUpward;

  AdvancedHintStyle->Color = TColor(0x00EDBB87);
  AdvancedHintStyle->Font->Color = TColor(0x00AD3F29);
  AdvancedHintStyle->Font->Height = -13;
  AdvancedHintStyle->Font->Style.Clear();
  AdvancedHintStyle->IconType = cxhiCustom;
  AdvancedHintStyle->IconSize = cxisDefault;
  AdvancedHintStyle->Rounded = false;
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoDemoMainForm::SetRoundedInfoHintStyle()
{
  if (!ShowHint) ShowHint = true;
  AdvancedHintStyle->Standard = false;
  AdvancedHintStyle->Animate = cxhaFadeIn;
  AdvancedHintStyle->Color = clInfoBk;
  AdvancedHintStyle->Font->Color = clBlack;
  AdvancedHintStyle->Font->Height = -11;
  AdvancedHintStyle->Font->Style.Clear();
  AdvancedHintStyle->IconType = cxhiInformation;
  AdvancedHintStyle->IconSize = cxisDefault;
  AdvancedHintStyle->Rounded = true;
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoDemoMainForm::SetStandardHintStyle()
{
  if (!ShowHint) ShowHint = true;
  AdvancedHintStyle->Color = clInfoBk;
  AdvancedHintStyle->Standard = true;
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoDemoMainForm::SetNoHintStyle()
{
  ShowHint = false;
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoDemoMainForm::SetHintStyle(TcxHintType AHintType)
{
  switch (AHintType) {
    case hcstLightInfo: {
      SetLightInfoHintStyle(); break; }
    case hcstLightSlideLeft: {
      SetLightSlideLeftHintStyle(); break; }
    case hcstBlueSlideUp: {
      SetBlueSlideUpHintStyle(); break; }
    case hcstRoundedInfo: {
      SetRoundedInfoHintStyle(); break; }
    case hcstStandard: {
      SetStandardHintStyle(); break; }
    case hcstNoHint: {
      SetNoHintStyle(); break; }
    default: SetLightInfoHintStyle();
  }
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoDemoMainForm::miCurDisplayStyleClick(
      TObject *Sender)
{
  ((TMenuItem*)Sender)->Checked = true;
  UncheckMenuItems(miDefDisplayStyle);
  DefaultDisplayStyle = false;
  ActiveFrame->DisplayStyle = (TcxStyleSheetType)((TMenuItem*)Sender)->Tag;
  gbDescription->Color = ActiveFrame->GetStyleBackgroundColor();
  gbDescription->CaptionBkColor = ActiveFrame->GetStyleBackgroundColor();
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoDemoMainForm::miDefaultDisplayStyleClick(
      TObject *Sender)
{
  ((TMenuItem*)Sender)->Checked = true;
  UncheckMenuItems(miCurDisplayStyle);
  DefaultDisplayStyle = true;
  FCurrentDisplayStyle = (TcxStyleSheetType)((TMenuItem*)Sender)->Tag;
  ActiveFrame->ChangeDisplayStyle((TcxStyleSheetType)((TMenuItem*)Sender)->Tag);
  gbDescription->Color = ActiveFrame->GetStyleBackgroundColor();
  gbDescription->CaptionBkColor = ActiveFrame->GetStyleBackgroundColor();
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoDemoMainForm::miCurHintStyleClick(
      TObject *Sender)
{
  DefaultHintStyle = false;
  UncheckMenuItems(miDefHintStyle);
  ((TMenuItem*)Sender)->Checked = true;
  ActiveFrame->HintStyle = (TcxHintType)((TMenuItem*)Sender)->Tag;
  SetHintStyle((TcxHintType)((TMenuItem*)Sender)->Tag);
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoDemoMainForm::miHintStyleClick(
      TObject *Sender)
{
  DefaultHintStyle = true;
  UncheckMenuItems(miCurHintStyle);
  ((TMenuItem*)Sender)->Checked = true;
  SetHintStyle((TcxHintType)((TMenuItem*)Sender)->Tag);
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoDemoMainForm::UpdateFileNameStatusPanel(String AFileName)
{
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoDemoMainForm::miFileOpenClick(TObject *Sender)
{
  ActiveFrame->OpenFile(this);
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoDemoMainForm::miFileSaveClick(TObject *Sender)
{
  ActiveFrame->SaveFile(this);
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoDemoMainForm::PageControlChange(
      TObject *Sender)
{
  int APageIndex = PageControl->ActivePage->TabIndex;
  if (APageIndex != -1) {
  gbDescription->Caption = EditorsStylesDemoFrameManager()->Frames[APageIndex]->Description();
  }
}
//---------------------------------------------------------------------------

TcxHintStyle* __fastcall TEditorsStylesDemoDemoMainForm::GetAdvancedHintStyle()
{
  return((TcxHintStyle*)cxHintStyleController->HintStyle);
}
