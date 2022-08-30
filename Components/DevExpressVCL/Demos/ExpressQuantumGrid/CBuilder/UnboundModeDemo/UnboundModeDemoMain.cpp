//---------------------------------------------------------------------------

#include <vcl.h>
#include <stdlib.h>
#include "shellapi.hpp"
#pragma hdrstop

#include "UnboundModeDemoMain.h"
#include "AboutDemoForm.h"
#include "UnboundModeDemoMinerCore.h"
#include "UnboundModeDemoCustomField.h"
#include "UnboundModeDemoFastestSweepers.h"
#include "ExtCtrls.hpp"
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
#pragma link "cxLookAndFeels"
#pragma resource "*.dfm"
TUnboundModeDemoMainForm *UnboundModeDemoMainForm;
//---------------------------------------------------------------------------
__fastcall TUnboundModeDemoMainForm::TUnboundModeDemoMainForm(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TUnboundModeDemoMainForm::miAboutClick(TObject *Sender)
{
  ShowAboutDemoForm();
}
//---------------------------------------------------------------------------

void __fastcall TUnboundModeDemoMainForm::miExitClick(TObject *Sender)
{
  Close();
}
//---------------------------------------------------------------------------

void __fastcall TUnboundModeDemoMainForm::FormShow(TObject *Sender)
{
  miNewClick(Sender);
}
//---------------------------------------------------------------------------

void __fastcall TUnboundModeDemoMainForm::FormCreate(TObject *Sender)
{
  MinerField = new TMinerField();

  FOnChangeGameDifficulty = MinerField->HandleEvChangeGameDifficulty;
  IntMinerField = new TIntMinerField(this, MinerField);
  IntMinerField->SetParent(this);
  FImageIndex = imSmile;
  IntMinerField->Images = ilGame;
  IntMinerField->OnImageChanged = HandleEvImageChanged;
  IntMinerField->OnMineCountChanged = HandleMineCountChangedEvent;
  IntMinerField->OnGameStatusChanged = HandleEvGameStatusChanged;
  InitGameSettings();
  ReadMinerSettings();
}
//---------------------------------------------------------------------------

void __fastcall TUnboundModeDemoMainForm::FormMouseDown(TObject *Sender,
      TMouseButton Button, TShiftState Shift, int X, int Y)
{
  TRect Rct;
  if(Button != mbLeft) return;
  SetButtonBounds(Rct);
  if(!IsPointInRect(Point(X, Y), Rct)) {
    if(FImageIndex == 2) {
      FImageIndex = 0;
      DrawButton();
    };
    return;
  }
  if(!FMouseButtonPressed) {
    FMouseButtonPressed = true;
    FDown = true;
    DrawButton();
  }
}
//---------------------------------------------------------------------------

void __fastcall TUnboundModeDemoMainForm::FormMouseMove(TObject *Sender,
      TShiftState Shift, int X, int Y)
{
  TRect Rct;
  SetButtonBounds(Rct);
  if(FMouseButtonPressed) {
    if(!IsPointInRect(Point(X, Y), Rct)) {
      FMouseButtonPressed = false;
      DrawButton();
    }
  }
  else {
    if((IsPointInRect(Point(X, Y), Rct)) && (Shift == (TShiftState() << ssLeft)) && FDown) {
      FMouseButtonPressed = true;
      DrawButton();
    }
  }
}
//---------------------------------------------------------------------------

void __fastcall TUnboundModeDemoMainForm::FormMouseUp(TObject *Sender,
      TMouseButton Button, TShiftState Shift, int X, int Y)
{
  TRect Rct;
  FDown = false;
  if(FImageIndex == 0) {
    FImageIndex = 2;
    DrawButton();
  }

  SetButtonBounds(Rct);
  if(IsPointInRect(Point(X, Y), Rct))
    if(FMouseButtonPressed) {
      FMouseButtonPressed = false;
      miNewClick(NULL);
    }
}
//---------------------------------------------------------------------------

void __fastcall TUnboundModeDemoMainForm::FormDestroy(TObject *Sender)
{
  WriteMinerSettings();
  delete MinerField;
}
//---------------------------------------------------------------------------

void __fastcall TUnboundModeDemoMainForm::FormPaint(TObject *Sender)
{
  Canvas->Brush->Style = bsSolid;
  Canvas->Brush->Color =
    SchemeColors[(int)IntMinerField->ColorScheme][cliBackground];
  Canvas->FillRect(Rect(0, 0, Width, Height));
  DrawOuterFrame();
  DrawIndicatorBoard();
  DrawTime();
  DrawMineCount();
  DrawButton();
}
//---------------------------------------------------------------------------

void __fastcall TUnboundModeDemoMainForm::FormResize(TObject *Sender)
{
  if(Left + Width > Screen->Width)
    Left = Screen->Width - Width;
  Canvas->Brush->Style = bsSolid;
  Canvas->Brush->Color = clBlueSky;
  Canvas->FillRect(Rect(0, 0, Width, Height));
  FormPaint(Sender);
}
//---------------------------------------------------------------------------

void __fastcall TUnboundModeDemoMainForm::miNewClick(TObject *Sender)
{
  FTime = 0;
  Timer->Enabled = false;
  FireGameDifficultyChangedEvent(FGameDifficulty);
  FMineCount = FGameDifficulty.MineCount;
  DrawMineCount();
}
//---------------------------------------------------------------------------

void __fastcall TUnboundModeDemoMainForm::miBeginnerClick(
      TObject *Sender)
{
  FGameDifficulty.DifficultyType = dtBeginner;
  miNewClick(Sender);
}
//---------------------------------------------------------------------------

void __fastcall TUnboundModeDemoMainForm::miIntermediateClick(
      TObject *Sender)
{
  FGameDifficulty.DifficultyType = dtIntermediate;
  miNewClick(Sender);
}
//---------------------------------------------------------------------------

void __fastcall TUnboundModeDemoMainForm::miExpertClick(TObject *Sender)
{
  FGameDifficulty.DifficultyType = dtExpert;
  miNewClick(Sender);
}
//---------------------------------------------------------------------------

void __fastcall TUnboundModeDemoMainForm::miCustomClick(TObject *Sender)
{
  TUnboundModeDemoCustomFieldForm* CustomField = new TUnboundModeDemoCustomFieldForm(this);
  try {
    CustomField->edtHeight->Text = IntToStr(FGameDifficulty.Height);
    CustomField->edtWidth->Text = IntToStr(FGameDifficulty.Width);
    CustomField->edtMineCount->Text = IntToStr(FGameDifficulty.MineCount);

    if(CustomField->ShowModal() == mrOk) {
      FGameDifficulty.Height = StrToInt(CustomField->edtHeight->Text);
      FGameDifficulty.Width = StrToInt(CustomField->edtWidth->Text);
      FGameDifficulty.MineCount = StrToInt(CustomField->edtMineCount->Text);
      FGameDifficulty.DifficultyType = dtCustom;
      miNewClick(Sender);
    }
  }
  __finally {
    delete CustomField;
  }
}
//---------------------------------------------------------------------------

void __fastcall TUnboundModeDemoMainForm::miBestTimesClick(
      TObject *Sender)
{
  TUnboundModeDemoFastestSweepersForm* FastestSweepersForm = new TUnboundModeDemoFastestSweepersForm(this);
  try {
    FastestSweepersForm->lbBeginnerTime->Caption = IntToStr(FTimes[0]);
    FastestSweepersForm->lbIntermediateTime->Caption = IntToStr(FTimes[1]);
    FastestSweepersForm->lbExpertTime->Caption = IntToStr(FTimes[2]);
    FastestSweepersForm->lbBeginnerName->Caption = FNames[0];
    FastestSweepersForm->lbIntermediateName->Caption = FNames[1];
    FastestSweepersForm->ibExpertName->Caption = FNames[2];
    FastestSweepersForm->ShowModal();
    if(FastestSweepersForm->FastestTimesResetted)
      ResetFastestTimes();
  }
  __finally {
    delete FastestSweepersForm;
  }
}
//---------------------------------------------------------------------------

void __fastcall TUnboundModeDemoMainForm::miMarksClick(TObject *Sender)
{
  ((TMenuItem*)Sender)->Checked = !((TMenuItem*)Sender)->Checked;
  IntMinerField->QuestionMarkCell = ((TMenuItem*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TUnboundModeDemoMainForm::miBlueClick(
      TObject *Sender)
{
  if(IntMinerField->ColorScheme != csBlue) {
    IntMinerField->ColorScheme = csBlue;
    FormPaint(this);
    ((TMenuItem*)Sender)->Checked = true;
  }
}
//---------------------------------------------------------------------------

void __fastcall TUnboundModeDemoMainForm::miGreenClick(
      TObject *Sender)
{
  if(IntMinerField->ColorScheme != csGreen) {
    IntMinerField->ColorScheme = csGreen;
    FormPaint(this);
    ((TMenuItem*)Sender)->Checked = true;
  }
}
//---------------------------------------------------------------------------

void __fastcall TUnboundModeDemoMainForm::miSystemClick(
      TObject *Sender)
{
  if(IntMinerField->ColorScheme != csSystem) {
    IntMinerField->ColorScheme = csSystem;
    FormPaint(this);
    ((TMenuItem*)Sender)->Checked = true;
  }
}
//---------------------------------------------------------------------------

void __fastcall TUnboundModeDemoMainForm::miGoldClick(
      TObject *Sender)
{
  if(IntMinerField->ColorScheme != csGold) {
    IntMinerField->ColorScheme = csGold;
    FormPaint(this);
    ((TMenuItem*)Sender)->Checked = true;
  }
}
//---------------------------------------------------------------------------

void __fastcall TUnboundModeDemoMainForm::HandleMineCountChangedEvent(TObject* Sender, TMineCountChangedEventType AMineCountChangedEventType)
{
  switch (AMineCountChangedEventType) {
    case mcIncMineCount: FMineCount++; break;
    case mcDecMineCount: FMineCount--; break;
  }
  DrawMineCount();
}
//---------------------------------------------------------------------------

void __fastcall TUnboundModeDemoMainForm::HandleEvGameStatusChanged(TObject* Sender, TGameStatus AGameStatus, const TGameDifficulty &AGameDifficulty)
{
  switch (AGameStatus) {
    case gsNew: {
      FGameDifficulty = AGameDifficulty;
      FImageIndex = 2;
      FTime = 0;
      Timer->Enabled = false;
      FMineCount = FGameDifficulty.MineCount;
      CheckMenuItem(FGameDifficulty.DifficultyType);
      OnPaint(this);
    }; break;
    case gsRun:    {
      // Timer on
      Timer->Enabled = true;
      TimerTimer(this);
    }; break;
    case gsLost: {
      FImageIndex = 1;
      OnPaint(this);
      // Timer off
      Timer->Enabled = false;
    }; break;
    case gsWon: {
      Timer->Enabled = false;
      FImageIndex = 3;
      FMineCount = 0;
      OnPaint(this);
      CheckBestTimes();
    }; break;
  }
}
//---------------------------------------------------------------------------

void __fastcall TUnboundModeDemoMainForm::HandleEvImageChanged(TObject* Sender, int AImageIndex)
{
  switch(AImageIndex) {
    case imSmile: FImageIndex = 2; break;
    case imAstonisment: FImageIndex = 0; break;
    case imWon: FImageIndex = 3; break;
    case imLost: FImageIndex = 1; break;
  }
  DrawButton();
}
//---------------------------------------------------------------------------

bool __fastcall TUnboundModeDemoMainForm::IsPointInRect(TPoint APoint, TRect ARect)
{
  return ((ARect.Left <= APoint.x) && (APoint.x <= ARect.Right) &&
            (ARect.Top <= APoint.y) && (APoint.y <= ARect.Bottom));
}
//---------------------------------------------------------------------------

void __fastcall TUnboundModeDemoMainForm::SetButtonBounds(TRect &ARect)
{
  int AButtonXPos = (Width >> 1) - (biButtonWidth >> 1);
  ARect = Rect(AButtonXPos, psBoardInnerIndent,
    AButtonXPos + biButtonWidth, psBoardInnerIndent + biButtonWidth);
}
//---------------------------------------------------------------------------

void __fastcall TUnboundModeDemoMainForm::FireGameDifficultyChangedEvent(const TGameDifficulty ANewGameDifficulty)
{
  if (FOnChangeGameDifficulty != NULL)
    FOnChangeGameDifficulty(this, ANewGameDifficulty);
}
//---------------------------------------------------------------------------

void __fastcall TUnboundModeDemoMainForm::DrawMineCount()
{
  TArrInteger mCount;
  if (FMineCount >=0)
    MakeArrayFromInt(FMineCount, mCount, biMineDigitCount);
  else {
    MakeArrayFromInt(abs(FMineCount), mCount, biMineDigitCount);
    mCount[biMineDigitCount - 1] = 10; // minus
  }
  TRect Rct = Rect(psBoardInnerIndent, psBoardInnerIndent,
    psBoardInnerIndent + biMineDigitCount * ilNumbers->Width + 2*biCountersBorderWidth,
    biNumberHeight + 2*biCountersBorderWidth);
  Frame3D(Canvas, Rct,
    SchemeColors[(int)IntMinerField->ColorScheme][cliFrame3dTopColor],
    SchemeColors[(int)IntMinerField->ColorScheme][cliFrame3dBottomColor],
    biCountersBorderWidth);
  for(int i=0; i < biMineDigitCount; i++)
    ilNumbers->Draw(Canvas, Rct.Left + ilNumbers->Width * i, Rct.Top, mCount[biMineDigitCount - 1 - i], true);
}
//---------------------------------------------------------------------------

void __fastcall TUnboundModeDemoMainForm::DrawTime()
{
  TArrInteger tArr;
  MakeArrayFromInt(FTime, tArr, biTimerDigitCount);
  int ATimerWidth = biTimerDigitCount * ilNumbers->Width + 2 * biCountersBorderWidth;
  TRect Rct = Rect(ClientWidth - ATimerWidth - psBoardInnerIndent,
    psBoardInnerIndent, ClientWidth - psBoardInnerIndent,
    biNumberHeight + 2 * biCountersBorderWidth);
  Frame3D(Canvas, Rct,
    SchemeColors[(int)IntMinerField->ColorScheme][cliFrame3dTopColor],
    SchemeColors[(int)IntMinerField->ColorScheme][cliFrame3dBottomColor],
    biCountersBorderWidth);
  for (int i=0; i < biTimerDigitCount; i++)
    ilNumbers->Draw(Canvas, Rct.Left + ilNumbers->Width * i, Rct.Top, tArr[biTimerDigitCount - 1 - i], true);
}
//---------------------------------------------------------------------------

void __fastcall TUnboundModeDemoMainForm::DrawButton()
{
  TRect Rct, RctBk, RctPressed;

  SetButtonBounds(Rct);
  Canvas->Brush->Style = bsSolid;
  RctBk.Left = Rct.Left - 1;
  RctBk.Top = Rct.Top - 1;
  RctBk.Right = Rct.Right + 1;
  RctBk.Bottom = Rct.Bottom + 1;
  Canvas->Brush->Color = SchemeColors[(int)IntMinerField->ColorScheme][cliButtonColor];
  Canvas->FillRect(Rect(RctBk.Left, RctBk.Top, RctBk.Right + 1, RctBk.Bottom + 1));

  Canvas->Brush->Color =
    SchemeColors[(int)IntMinerField->ColorScheme][cliFrame3dTopColor];
  Canvas->FrameRect(RctBk);

  if(!FMouseButtonPressed) {
    Frame3D(Canvas, Rct,
      SchemeColors[(int)IntMinerField->ColorScheme][cliFrame3dBottomColor],
      SchemeColors[(int)IntMinerField->ColorScheme][cliFrame3dTopColor],  2);
    ilFaces->Draw(Canvas, Rct.Left + 1, Rct.Top + 1, FImageIndex, true);
  }
  else {
    RctPressed.Left = RctBk.Left + 1;
    RctPressed.Top = RctBk.Top + 1;
    RctPressed.Right = RctBk.Right + 1;
    RctPressed.Bottom = RctBk.Bottom + 1;

    Canvas->FrameRect(RctPressed);
    Canvas->Brush->Color = SchemeColors[(int)IntMinerField->ColorScheme][8];
    Canvas->FillRect(
    Rect(RctPressed.Left + 1, RctPressed.Top + 1, RctPressed.Right - 1, RctPressed.Bottom-1));
    ilFaces->Draw(Canvas, Rct.Left + 4, Rct.Top + 4 , 2, true);
  };
}
//---------------------------------------------------------------------------

void __fastcall TUnboundModeDemoMainForm::DrawIndicatorBoard()
{
  TRect Rct = Rect(psBorder, psBorder,
    (ClientWidth - (psBorder - psOuterFrameWidth)), biBoardHeight);
  Frame3D(Canvas, Rct,
    SchemeColors[(int)IntMinerField->ColorScheme][cliFrame3dTopColor],
    SchemeColors[(int)IntMinerField->ColorScheme][cliFrame3dBottomColor], 2);
}
//---------------------------------------------------------------------------

void __fastcall TUnboundModeDemoMainForm::DrawOuterFrame()
{
  TRect Rct = Rect(1, 1, Width, Height);
  Frame3D(Canvas, Rct,
    SchemeColors[(int)IntMinerField->ColorScheme][cliFrame3dTopColor],
    SchemeColors[(int)IntMinerField->ColorScheme][cliFrame3dBottomColor],
    psOuterFrameWidth);
}
//---------------------------------------------------------------------------

void __fastcall TUnboundModeDemoMainForm::ReadMinerSettings()
{
  TRegistry* Registry = new TRegistry();
  try {
    Registry->RootKey = HKEY_CURRENT_USER;
    Registry->OpenKey(Section, False);
    if(Registry->ValueExists(Difficulty)) {
      int Diff = Registry->ReadInteger(Difficulty);
      if ((Diff >= 0) && (Diff <= 3))
        FGameDifficulty.DifficultyType = TDifficultyType(Diff);
      else
        FGameDifficulty.DifficultyType = dtBeginner;
    };
    if(FGameDifficulty.DifficultyType == dtCustom) {
      if(Registry->ValueExists("Width"))
        FGameDifficulty.Width = Registry->ReadInteger(UnboundModeDemoTypesH::Width);
      if(Registry->ValueExists("Height"))
        FGameDifficulty.Height = Registry->ReadInteger(UnboundModeDemoTypesH::Height);
      if(Registry->ValueExists("MineCount"))
        FGameDifficulty.MineCount = Registry->ReadInteger(MineCount);
    };
    if(Registry->ValueExists(UnboundModeDemoTypesH::Mark))
      Registry->ReadInteger(UnboundModeDemoTypesH::Mark);
    if(Registry->ValueExists(Name1))
      FNames[0] = Registry->ReadString(Name1);
    if(Registry->ValueExists(Name2))
      FNames[1] = Registry->ReadString(Name2);
    if(Registry->ValueExists(Name3))
      FNames[2] = Registry->ReadString(Name3);
    if(Registry->ValueExists(Time1))
      FTimes[0] = Registry->ReadInteger(Time1);
    if(Registry->ValueExists(Time2))
      FTimes[1] = Registry->ReadInteger(Time2);
    if(Registry->ValueExists(Time3))
      FTimes[2] = Registry->ReadInteger(Time3);
  }
  __finally {
    Registry->CloseKey();
    delete Registry;
  };
}
//---------------------------------------------------------------------------

void __fastcall TUnboundModeDemoMainForm::WriteMinerSettings()
{
  TRegistry* Registry = new TRegistry();
  try {
    Registry->RootKey = HKEY_CURRENT_USER;
    if(!Registry->OpenKey(Section, false)) {
      Registry->CreateKey(Section);
      Registry->OpenKey(Section, false);
    };
    Registry->WriteInteger(Difficulty, (int)FGameDifficulty.DifficultyType);
    Registry->WriteInteger("Width", FGameDifficulty.Width);
    Registry->WriteInteger("Height", FGameDifficulty.Height);
    Registry->WriteInteger("MineCount", FGameDifficulty.MineCount);
    Registry->WriteInteger(UnboundModeDemoTypesH::Mark, 1);
    for(int i=0; i < 3; i++) {
      Registry->WriteString("Name" + IntToStr(i+1), FNames[i]);
      Registry->WriteInteger("Time" + IntToStr(i+1), FTimes[i]);
    };
  }
  __finally {
    Registry->CloseKey();
    delete Registry;
  };
}
//---------------------------------------------------------------------------

void __fastcall TUnboundModeDemoMainForm::InitGameSettings()
{
  FGameDifficulty.DifficultyType = dtBeginner;
  ResetFastestTimes();
}
//---------------------------------------------------------------------------

void __fastcall TUnboundModeDemoMainForm::ResetFastestTimes()
{
  for(int i=0; i < 3; i++) {
    FTimes[i] = 999;
    FNames[i] = "Anonymous";
  }
}
//---------------------------------------------------------------------------

void __fastcall TUnboundModeDemoMainForm::CheckBestTimes()
{
  String Level;
  if (FGameDifficulty.DifficultyType == dtCustom)
    return;
  if (FTimes[(int)FGameDifficulty.DifficultyType] > FTime) {
    switch (FGameDifficulty.DifficultyType) {
      case dtBeginner: Level = "beginner"; break;
      case dtIntermediate: Level = "intermediate"; break;
      case dtExpert: Level = "expert"; break;
    }
    FTimes[(int)FGameDifficulty.DifficultyType] = FTime;
    FNames[(int)FGameDifficulty.DifficultyType] = InputBox("You are the champion in the "+ Level+" level", "Please enter your name.",
      FNames[(int)FGameDifficulty.DifficultyType]);
    miBestTimesClick(this);
  }
}
//---------------------------------------------------------------------------

void __fastcall TUnboundModeDemoMainForm::CheckMenuItem(TDifficultyType AGameDifficulty)
{
  switch(AGameDifficulty) {
    case dtBeginner: miBeginner->Checked = true; break;
    case dtIntermediate: miIntermediate->Checked = true; break;
    case dtExpert: miExpert->Checked = true; break;
    case dtCustom: miCustom->Checked = true; break;
  }
}
//---------------------------------------------------------------------------
void __fastcall TUnboundModeDemoMainForm::TimerTimer(TObject *Sender)
{
  if (FTime < 999) FTime++;
  DrawTime();
}
//---------------------------------------------------------------------------


