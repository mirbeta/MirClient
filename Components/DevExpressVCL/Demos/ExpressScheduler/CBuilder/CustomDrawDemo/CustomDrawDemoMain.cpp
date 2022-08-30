//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
#define HDC unsigned int

#include "CustomDrawDemoMain.h"

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxControls"
#pragma link "cxEdit"
#pragma link "cxGraphics"
#pragma link "cxScheduler"
#pragma link "cxSchedulerCustomControls"
#pragma link "cxSchedulerCustomResourceView"
#pragma link "cxSchedulerDateNavigator"
#pragma link "cxSchedulerDayView"
#pragma link "cxStyles"
#pragma link "DemoBasicMain"
#pragma link "cxSchedulerStorage"
#pragma link "cxFormats"
#pragma link "dxOffice11"

#pragma link "DemoBasicMain"
#pragma link "cxSchedulerGanttView"
#pragma link "cxSchedulerHolidays"
#pragma link "cxSchedulerTimeGridView"
#pragma link "cxSchedulerUtils"
#pragma link "cxSchedulerWeekView"
#pragma link "cxSchedulerYearView"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma link "cxSchedulerGanttView"
#pragma link "cxSchedulerHolidays"
#pragma link "cxSchedulerTimeGridView"
#pragma link "cxSchedulerUtils"
#pragma link "cxSchedulerWeekView"
#pragma link "cxSchedulerYearView"
#pragma link "cxSchedulerAgendaView"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma link "cxSchedulerGanttView"
#pragma link "cxSchedulerHolidays"
#pragma link "cxSchedulerRecurrence"
#pragma link "cxSchedulerTimeGridView"
#pragma link "cxSchedulerUtils"
#pragma link "cxSchedulerWeekView"
#pragma link "cxSchedulerYearView"
#pragma resource "*.dfm"

TCustomDrawDemoMainForm *CustomDrawDemoMainForm;

  int W1;
  int H1;
  int C1;
  int C2;
  Graphics::TBitmap *ABitmap;
  unsigned char FDaysOffIndexes[2];

// we do not use dxOffice11.FillGradientRect because of the problem in C++Builder during linking
void __fastcall FillGradientRectEx(TcxCanvas* ACanvas, const TRect &ARect, int AColor1, int AColor2, bool AHorizontal)
{
  int AFirstOffset, ALastOffset, APixelSteps, AColorStepsR, AColorStepsG, AColorStepsB;
  TRect R = ARect;

  if (AHorizontal) {
    AFirstOffset = ARect.Left;
    ALastOffset = ARect.Right - 1;
  }
  else
  {
    AFirstOffset = ARect.Top;
    ALastOffset = ARect.Bottom - 1;
  }
  APixelSteps = ALastOffset - AFirstOffset;
  AColorStepsR = GetRValue(AColor2) - GetRValue(AColor1);
  AColorStepsG = GetGValue(AColor2) - GetGValue(AColor1);
  AColorStepsB = GetBValue(AColor2) - GetBValue(AColor1);

  for (int I = AFirstOffset; I <= ALastOffset; I++) {
    if (AHorizontal) {
      R.Left = I;
      R.Right = I + 1;
    }
	else
    {
      R.Top = I;
      R.Bottom = I + 1;
    }
    ACanvas->Brush->Color = (TColor)RGB(
      GetRValue(AColor1) + MulDiv(I - AFirstOffset, AColorStepsR, APixelSteps),
      GetGValue(AColor1) + MulDiv(I - AFirstOffset, AColorStepsG, APixelSteps),
      GetBValue(AColor1) + MulDiv(I - AFirstOffset, AColorStepsB, APixelSteps));
    ACanvas->FillRect(R, NULL, False);
  }
}

//---------------------------------------------------------------------------
__fastcall TCustomDrawDemoMainForm::TCustomDrawDemoMainForm(TComponent* Owner)
        : TDemoBasicMainForm(Owner)
{
//
}

TcxLookAndFeelKind __fastcall TCustomDrawDemoMainForm::GetDefaultLookAndFeelKind()
{
  return(lfUltraFlat);
}

//---------------------------------------------------------------------------
void __fastcall TCustomDrawDemoMainForm::FormCreate(TObject *Sender)
{
  TDemoBasicMainForm::FormCreate(Sender);
  ABitmap = new Graphics::TBitmap();
  ABitmap->PixelFormat = pf32bit;
  Scheduler->OptionsView->RotateResourceCaptions = false;
  Scheduler->ViewDay->EventShadows = false;
  int I = (int)cxFormatController()->StartOfWeek + (int)(dSunday);
  int J = (int)(dSunday) - I;
  if (J < 0) J = J + 7;
  FDaysOffIndexes[0] = J;
  J = (int)(dSaturday) - I;
  if (J < 0) J = J + 7;
  FDaysOffIndexes[1] = J;
  GenerateRandomEvents(100, true);
}
//---------------------------------------------------------------------------
void __fastcall TCustomDrawDemoMainForm::FormDestroy(TObject *Sender)
{
  delete ABitmap;
}
//---------------------------------------------------------------------------
void __fastcall TCustomDrawDemoMainForm::SchedulerDateNavigatorCustomDrawHeader(
  TObject *Sender, TcxCanvas *ACanvas, TcxSchedulerDateNavigatorMonthHeaderViewInfo *AViewInfo, bool &ADone)
{
  if (!miMonthHeaders->Checked)
    return;
  FillGradientRectEx(ACanvas, AViewInfo->Bounds, 0xFFE0E0, 0xFF8080, false);
  ACanvas->FrameRect(AViewInfo->Bounds, clBlue, 1, TcxBorders()<<bLeft<<bRight<<bTop<<bBottom, False);
  AViewInfo->Transparent = true;
}
//---------------------------------------------------------------------------
void __fastcall TCustomDrawDemoMainForm::SchedulerDateNavigatorCustomDrawDayCaption(
  TObject *Sender, TcxCanvas *ACanvas, TcxSchedulerDateNavigatorDayCaptionViewInfo *AViewInfo, bool &ADone)
{
  if (!miDayCaptions->Checked)
	return;
  if (AViewInfo->Index == FDaysOffIndexes[0] || AViewInfo->Index == FDaysOffIndexes[1])
    ACanvas->Font = csRed->Font;
}
//---------------------------------------------------------------------------
void __fastcall TCustomDrawDemoMainForm::SchedulerDateNavigatorCustomDrawDayNumber(
  TObject *Sender, TcxCanvas *ACanvas, TcxSchedulerDateNavigatorDayNumberViewInfo *AViewInfo, bool &ADone)
{
  if (!miDays->Checked) return;
  int ADay = DayOfWeek(AViewInfo->Date);
  if (AViewInfo->Selected)
    ACanvas->Brush->Color = clAppWorkSpace;
  else
    if (ADay == 1 || ADay == 7)
      ACanvas->Font = csRed->Font;
}
//---------------------------------------------------------------------------
void __fastcall TCustomDrawDemoMainForm::SchedulerCustomDrawDayHeader(
  TObject *Sender, TcxCanvas *ACanvas, TcxSchedulerDayHeaderCellViewInfo *AViewInfo, bool &ADone)
{
  if (!miHeaders->Checked) return;
  AViewInfo->Transparent = true;
  FillGradientRectEx(ACanvas, AViewInfo->Bounds, 0xA0A0A0, 0x707070, false);
  ACanvas->FrameRect(AViewInfo->Bounds, clGray, 1, TcxBorders()<<bLeft<<bRight<<bTop<<bBottom, false);
  ACanvas->Brush->Style = bsClear;
  ACanvas->Font = csItalic->Font;
  if ((dynamic_cast<TcxSchedulerAgendaViewDayHeaderCellViewInfo*>(AViewInfo)) && (Scheduler->ViewAgenda->DayHeaderOrientation == dhoVertical))
  {
	String ADisplayText = AViewInfo->DisplayText;
	int P = Pos(',', ADisplayText);
	ADisplayText = ADisplayText.SubString(1, P - 1) + "\r\n" + ADisplayText.SubString(P + 2, ADisplayText.Length() - P - 1);
	TRect ABounds = cxRectInflate(AViewInfo->Bounds, -2 * cxTextOffset, -2 * cxTextOffset);
	ACanvas->DrawTexT(ADisplayText, ABounds, cxAlignHCenter | cxAlignTop | cxWordBreak);
  }
  else
    ACanvas->DrawTexT(AViewInfo->DisplayText, AViewInfo->Bounds, cxAlignCenter, true);
  ACanvas->Brush->Style = bsSolid;
  ADone = True;
}
//---------------------------------------------------------------------------
void __fastcall TCustomDrawDemoMainForm::SchedulerViewDayCustomDrawRuler(
  TObject *Sender, TcxCanvas *ACanvas, TcxSchedulerTimeRulerCellViewInfo *AViewInfo, bool &ADone)
{
  if (!miTimeRuler->Checked) return;
  AViewInfo->Transparent = true;
  FillGradientRectEx(ACanvas, AViewInfo->Bounds[true], 0x909090, 0xE0E0E0, true);
}
//---------------------------------------------------------------------------
void __fastcall TCustomDrawDemoMainForm::SchedulerViewDayCustomDrawContainer(
  TObject *Sender, TcxCanvas *ACanvas, TcxSchedulerContainerCellViewInfo *AViewInfo, bool &ADone)
{
  if (!miContainer->Checked || AViewInfo->Selected) return;
  AViewInfo->Transparent = true;
  FillGradientRectEx(ACanvas, AViewInfo->Bounds, 0xC0E0C0, 0x70A070, false);
}
//---------------------------------------------------------------------------
void __fastcall TCustomDrawDemoMainForm::SchedulerCustomDrawEvent(
  TObject *Sender, TcxCanvas *ACanvas, TcxSchedulerEventCellViewInfo *AViewInfo, bool &ADone)
{
  if (!miEvents->Checked) return;
  AViewInfo->Transparent = true;
  if (dynamic_cast<TcxSchedulerAgendaViewEventCellViewInfo*>(AViewInfo))
    FillGradientRectEx(ACanvas, AViewInfo->Bounds, clWhite, dxGetMiddleRGB(clBlack, AViewInfo->Color, 10), false);
  else
    FillGradientRectEx(ACanvas, AViewInfo->Bounds, 0xA0A0A0, AViewInfo->Color, true);
}
//---------------------------------------------------------------------------
void __fastcall TCustomDrawDemoMainForm::UpdateCustomDraw(TObject *Sender)
{
  ((TMenuItem*)Sender)->Checked = !((TMenuItem*)Sender)->Checked;
  Scheduler->LayoutChanged();
}
//---------------------------------------------------------------------------
void __fastcall TCustomDrawDemoMainForm::SchedulerCustomDrawContent(
  TObject *Sender, TcxCanvas *ACanvas, TcxSchedulerContentCellViewInfo *AViewInfo, bool &ADone)
{
  if (!miContent->Checked) return;
  AViewInfo->Transparent = true;
  if (dynamic_cast<TcxSchedulerAgendaViewFreeDayCellViewInfo*>(AViewInfo))
    FillGradientRectEx(ACanvas, AViewInfo->Bounds, clWhite, dxGetMiddleRGB(clBlack, AViewInfo->Color, 10), false);
  else
    FillGradientRectEx(ACanvas, AViewInfo->Bounds, AViewInfo->Color, dxGetMiddleRGB(AViewInfo->Color, clBlack, 75), true);
}
//---------------------------------------------------------------------------
void __fastcall TCustomDrawDemoMainForm::SchedulerCustomDrawResourceHeader(
  TObject *Sender, TcxCanvas *ACanvas, TcxSchedulerHeaderCellViewInfo *AViewInfo, bool &ADone)
{
  if (!miResources->Checked) return;
  AViewInfo->Transparent = True;
  FillGradientRectEx(ACanvas, AViewInfo->Bounds, 0x50F0F0, 0x009090, false);
  ACanvas->Brush->Style = bsClear;
  ACanvas->Font = csBoldItalic->Font;
  ACanvas->DrawTexT(AViewInfo->DisplayText, AViewInfo->Bounds, cxAlignCenter, true);
  ACanvas->Brush->Style = bsSolid;
  ADone = True;
}
//---------------------------------------------------------------------------
void __fastcall TCustomDrawDemoMainForm::SchedulerCustomDrawGroupSeparator(
  TObject *Sender, TcxCanvas *ACanvas, TcxSchedulerGroupSeparatorCellViewInfo *AViewInfo, bool &ADone)
{
  if (!miGroupSeparator->Checked) return;
  FillGradientRectEx(ACanvas, AViewInfo->Bounds, 0x50F0F0, 0x009090, true);
  ADone = True;
}
//---------------------------------------------------------------------------
void __fastcall TCustomDrawDemoMainForm::SchedulerDateNavigatorCustomDrawContent(
  TObject *Sender, TcxCanvas *ACanvas, TcxSchedulerDateNavigatorMonthContentViewInfo *AViewInfo, bool &ADone)
{
  TColor AColor;
  if (!miDNContent->Checked) return;
  if (AViewInfo->Month >= 3 && AViewInfo->Month <= 11)
  {
    if (AViewInfo->Month <= 5)
      AColor = (TColor)0xD0FFD0;
    else
      if (AViewInfo->Month <= 8)
        AColor = (TColor)0xD0D0FF;
      else
        if (AViewInfo->Month <= 11)
          AColor = (TColor)0xD0FFFF;
  }
  else
    AColor = (TColor)0xFFE7E7;
  TRect R = AViewInfo->Bounds;
  ACanvas->Brush->Color = AColor;
  ACanvas->FillRect(R, NULL, false);
  ACanvas->Font->Height = R.Height();
  ACanvas->Font->Color = (TColor)dxGetMiddleRGB(AColor, clBlack, 85);
  ACanvas->DrawTexT(IntToStr(AViewInfo->Month), R, cxAlignCenter, true);
  ACanvas->Font = AViewInfo->ViewParams.Font;
  AViewInfo->Transparent = true;
  ADone = True;
}
//---------------------------------------------------------------------------

