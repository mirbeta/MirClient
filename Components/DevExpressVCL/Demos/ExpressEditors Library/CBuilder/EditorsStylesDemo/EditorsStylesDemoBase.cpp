//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "EditorsStylesDemoBase.h"
#include "cxButtons.hpp"
#include "EditorsStylesDemoFrameControl.h"
#include "cxSplitter.hpp"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxContainer"
#pragma link "cxControls"
#pragma link "cxEdit"
#pragma link "cxMemo"
#pragma link "cxPropertiesStore"
#pragma link "cxTextEdit"
#pragma link "cxClasses"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma resource "*.dfm"
TEditorsStylesDemoBaseFrame *EditorsStylesDemoBaseFrame;
TEditorsStylesDemoFrameManager*  FInstance = NULL;
const
  String AExtEditorNames[eetToggleSwitch + 1] = {"TcxLabel",
    "TcxProgressBar", "TcxTrackBar", "TcxCheckListBox", "TcxColorComboBox",
    "TcxFontNameComboBox", "TcxCheckComboBox", "TcxTreeView",
    "TcxShellTreeView", "TcxShellComboEdit", "TcxSplitter", "TcxGroupBox",
    "TcxSpinButton", "TcxHintStyleController", "TcxMCListBox", "TcxListView",
    "TcxHeader", "TcxShellListView", "TcxDBColorComboBox", "TcxDBLabel",
    "TcxDBProgressBar", "TcxDBTrackBar", "TcxDBCheckListBox",
    "TcxDBCheckComboBox", "TcxDBFontNameComboBox", "TcxDBShellComboEdit", "TcxMaskEdit", "TcxCheckBox",
    "TcxComboBox", "TcxButton", "TcxImage", "TcxDBTextEdit", "TcxDBSpinEdit",
	"TcxDBNavigator", "TcxDBDateEdit", "TcxDBLookupComboBox", "TcxDBMemo", "TcxGrid",
	"TcxCalcEdit", "TcxDateEdit", "TcxTextEdit", "TcxRichEdit", "TdxZoomTrackBar",
	"TdxCheckGroupBox", "TdxToggleSwitch"};

//---------------------------------------------------------------------------
__fastcall TEditorsStylesDemoBaseFrame::TEditorsStylesDemoBaseFrame(TComponent* Owner)
  : TForm(Owner)
{
  FFileName = "";
  FHintStyle = hcstLightInfo;
  FDisplayStyle = shtLightBlue;
  FTempDisplayStyle = shtLightBlue;
  FFrameControls = new TList();
  FSplitterFlickering = false;
}
//---------------------------------------------------------------------------

__fastcall TEditorsStylesDemoBaseFrame::~TEditorsStylesDemoBaseFrame()
{
  ClearFrameControls();
  delete FFrameControls;
}
//---------------------------------------------------------------------------

String __fastcall TEditorsStylesDemoBaseFrame::Name()
{
  return "Name";
}
//---------------------------------------------------------------------------

String __fastcall TEditorsStylesDemoBaseFrame::BriefName()
{
  return "BriefName";
}
//---------------------------------------------------------------------------

TcxExtEditorTypes GetcxControlType(String AcxControlName)
{
  TcxExtEditorTypes Result = TcxExtEditorTypes();
  for (int i=0; i < eetToggleSwitch + 1; i++)
    if (AExtEditorNames[(TcxExtEditorType)i] == AcxControlName) {
      Result << (TcxExtEditorType)i;
      break;
    }
  return Result;
}

TcxExtEditorTypes TEditorsStylesDemoBaseFrame::GetExtEditorTypes(TWinControl *AControl)
{
  TcxExtEditorTypes Result;
  Result.Clear();
  for (int i=0; i < AControl->ControlCount; i++){
    if ( dynamic_cast<TWinControl*>(AControl->Controls[i]))
      Result = Result + GetExtEditorTypes((TWinControl*)AControl->Controls[i]);
    if (( dynamic_cast<TcxControl*>(AControl->Controls[i]) ||
      dynamic_cast<TcxCustomButton*>(AControl->Controls[i])) &&
      ((TControl*)AControl->Controls[i])->Visible)
      Result += GetcxControlType(AControl->Controls[i]->ClassName());
  }
  return Result;
}
//---------------------------------------------------------------------------

TEditorsStylesDemoFrameManager::TEditorsStylesDemoFrameManager()
{
  FFrameList = new TList();
}
//---------------------------------------------------------------------------

TEditorsStylesDemoBaseFrame* TEditorsStylesDemoFrameManager::GetFrame(int AIndex)
{
 return ((TEditorsStylesDemoBaseFrame*)(FFrameList->Items[AIndex]));
}
//---------------------------------------------------------------------------

String TEditorsStylesDemoBaseFrame::StylesIniPath()
{
  return ("");
}
//---------------------------------------------------------------------------

void TEditorsStylesDemoBaseFrame::ChangeDisplayStyle(TcxStyleSheetType ADisplayStyle)
{
  if (ADisplayStyle != FTempDisplayStyle) {
    FTempDisplayStyle = ADisplayStyle;
    String AIniFileName = StylesIniPath() + StyleSheetIniFiles[ADisplayStyle];
    if (FileExists(AIniFileName)) {
      cxPropertiesStore->Active = true;
      cxPropertiesStore->StorageName = AIniFileName;
      cxPropertiesStore->RestoreFrom();
      cxPropertiesStore->Active = false;
    }
  }
}
//---------------------------------------------------------------------------

TColor TEditorsStylesDemoBaseFrame::GetStyleBackgroundColor()
{
  return Color;
}
//---------------------------------------------------------------------------

String TEditorsStylesDemoBaseFrame::Description()
{
  return "Brief Description";
}
//---------------------------------------------------------------------------

bool TEditorsStylesDemoBaseFrame::ShowControlsAboveDescription()
{
  return (false);
}
//---------------------------------------------------------------------------

bool TEditorsStylesDemoBaseFrame::GetFlickering()
{
  return FlickerTimer->Enabled;
}
//---------------------------------------------------------------------------

void TEditorsStylesDemoBaseFrame::ClearFrameControls()
{
  while (FFrameControls->Count > 0) {
    delete (TcxFrameControl*)FFrameControls->Items[0];
    FFrameControls->Delete(0);
  }
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoBaseFrame::cxSplitterMoved(TObject* Sender)
{
  if (FFlickeringClassName == "TcxSplitter")
    Flickering = FSplitterFlickering;
  FSplitterFlickering = false;
  if (Flickering) FlickerTimerTimer(FlickerTimer);
} 
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoBaseFrame::cxSplitterCanResize(TObject* Sender, int &NewSize, bool &Accept)
{
  if ((FFlickeringClassName == "TcxSplitter") && !FSplitterFlickering) {
    FSplitterFlickering = Flickering;
    Flickering = false;
  }
}
//---------------------------------------------------------------------------

void TEditorsStylesDemoBaseFrame::CreateFrameControls(TWinControl* AControl)
{
  TcxFrameControl* AFrameControl;
  for (int i=0; i < AControl->ControlCount; i++) {
    if ((String)AControl->Controls[i]->ClassName() == FFlickeringClassName) {
      if (dynamic_cast<TcxSplitter*>(AControl->Controls[i])) {
        ((TcxSplitter*)AControl->Controls[i])->OnMoved = cxSplitterMoved;
        ((TcxSplitter*)AControl->Controls[i])->OnCanResize = cxSplitterCanResize;
      }
      AFrameControl = new TcxFrameControl(this);
      AFrameControl->Parent = this;
      AFrameControl->FrameControl(AControl->Controls[i]);
      FFrameControls->Add(AFrameControl);
    }
    if (dynamic_cast<TWinControl*>(AControl->Controls[i]))
      CreateFrameControls(((TWinControl*)AControl->Controls[i]));
  }
}
//---------------------------------------------------------------------------

void TEditorsStylesDemoBaseFrame::AdjustFlickeringShapes(bool AIsFlickerRun)
{
  ClearFrameControls();
  if (AIsFlickerRun) 
    CreateFrameControls(this);
}
//---------------------------------------------------------------------------

void TEditorsStylesDemoBaseFrame::SetFlickering(const bool Value)
{
  if (Value != FlickerTimer->Enabled) {
    AdjustFlickeringShapes(Value);
    FlickerTimer->Enabled = Value;
  }
}
//---------------------------------------------------------------------------

void TEditorsStylesDemoBaseFrame::FlickerControls(String AControlClassName)
{
  FFlickeringClassName = AControlClassName;
  Flickering = true;
}
//---------------------------------------------------------------------------

bool TEditorsStylesDemoBaseFrame::MenuOpenFileVisible()
{
  return (false);
}
//---------------------------------------------------------------------------

bool TEditorsStylesDemoBaseFrame::MenuSaveFileVisible()
{
  return (false);
}
//---------------------------------------------------------------------------

void TEditorsStylesDemoBaseFrame::OpenFile(TObject* Sender)
{

}
//---------------------------------------------------------------------------

void TEditorsStylesDemoBaseFrame::SaveFile(TObject* Sender)
{

}
//---------------------------------------------------------------------------

bool TEditorsStylesDemoBaseFrame::StyleMenuVisible()
{
  return (true);
}
//---------------------------------------------------------------------------

void TEditorsStylesDemoBaseFrame::SetDisplayStyle(const TcxStyleSheetType Value)
{
  if ((FDisplayStyle != Value) || (FDisplayStyle != FTempDisplayStyle)) {
    FDisplayStyle = Value;
    ChangeDisplayStyle(FDisplayStyle);
  }
}
//---------------------------------------------------------------------------

int TEditorsStylesDemoFrameManager::GetFramesCount()
{
  return(FFrameList->Count);
}
//---------------------------------------------------------------------------

TEditorsStylesDemoFrameManager::~TEditorsStylesDemoFrameManager()
{
  TEditorsStylesDemoBaseFrame* AFrame;
  for (int i=0; i < FFrameList->Count; i++) {
    AFrame = (TEditorsStylesDemoBaseFrame*)FFrameList->Items[i];
    delete AFrame;
  }
  delete FFrameList;
}
//---------------------------------------------------------------------------

void TEditorsStylesDemoFrameManager::AddFrame(TEditorsStylesDemoBaseFrame* AEditorsStylesDemoBaseFrame)
{
  FFrameList->Add(AEditorsStylesDemoBaseFrame);
}
//---------------------------------------------------------------------------

TEditorsStylesDemoFrameManager* EditorsStylesDemoFrameManager()
{
  if (!FInstance)
    FInstance = new TEditorsStylesDemoFrameManager();
  return FInstance;
}
//---------------------------------------------------------------------------

void cxGetEditorsNamesListByTypes(TStrings* AEditorsNames, TcxExtEditorTypes AExtEditorTypes)
{
  if (AEditorsNames) {
    AEditorsNames->Clear();
    for (int i=eetLabel; i < eetToggleSwitch + 1; i++)
      if (AExtEditorTypes.Contains((TcxExtEditorType)i))
        AEditorsNames->Add(AExtEditorNames[i]);
  }
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoBaseFrame::FlickerTimerTimer(TObject *Sender)
{
  for (int i=0; i < FFrameControls->Count; i++) {
    ((TcxFrameControl*)FFrameControls->Items[i])->Visible =
      !((TcxFrameControl*)FFrameControls->Items[i])->Visible;
    ((TcxFrameControl*)FFrameControls->Items[i])->UpdateFrameControlPos();
  }
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoBaseFrame::DoOnFileNameChanged()
{
  if (FOnFileNameChanged) FOnFileNameChanged(FFileName);
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoBaseFrame::FrameResize(TObject *Sender)
{
  if (Flickering) FlickerTimerTimer(FlickerTimer);
}
//---------------------------------------------------------------------------

