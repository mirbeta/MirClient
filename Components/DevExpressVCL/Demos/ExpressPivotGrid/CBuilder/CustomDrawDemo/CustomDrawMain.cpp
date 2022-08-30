//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "CustomDrawMain.h"
//---------------------------------------------------------------------------
#include "cxGraphics.hpp"

#pragma package(smart_init)
#pragma link "cxLookAndFeels"
#pragma link "DemoBasicMain"
#pragma link "cxControls"
#pragma link "cxCustomPivotGrid"
#pragma link "cxDBPivotGrid"
#pragma link "cxClasses"
#pragma link "cxCustomData"
#pragma link "cxStyles"
#pragma link "cxEdit"
#pragma link "cxLookAndFeelPainters"
#pragma resource "*.dfm"
TfrmCustomDraw *frmCustomDraw;

// we do not use dxOffice11.FillGradientRect because of the problem in C++Builder during linking
void __fastcall FillGradientRectEx(TcxCanvas* ACanvas, const TRect &ARect, TColor AColor1, TColor AColor2, bool AHorizontal)
{
  int AFirstOffset, ALastOffset, APixelSteps, AColorStepsR, AColorStepsG, AColorStepsB;
  TRect R = ARect;
  TColor AColor;
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

  for (int I = AFirstOffset; I <= ALastOffset; I++)
  {
    if (AHorizontal) {
      R.Left = I;
	  R.Right = I + 1;
	}
    else
    {
      R.Top = I;
      R.Bottom = I + 1;
	}
	AColor = (TColor)RGB(
	  GetRValue(AColor1) + MulDiv(I - AFirstOffset, AColorStepsR, APixelSteps),
	  GetGValue(AColor1) + MulDiv(I - AFirstOffset, AColorStepsG, APixelSteps),
	  GetBValue(AColor1) + MulDiv(I - AFirstOffset, AColorStepsB, APixelSteps));
	ACanvas->FillRect(R, AColor);
  }
}


//---------------------------------------------------------------------------
__fastcall TfrmCustomDraw::TfrmCustomDraw(TComponent* Owner)
        : TfrmDemoBasicMain(Owner)
{
}

TcxLookAndFeelKind __fastcall TfrmCustomDraw::GetDefaultLookAndFeelKind()
{
  return(lfUltraFlat);
}

TcxCustomPivotGrid* __fastcall TfrmCustomDraw::PivotGrid()
{
  return DBPivotGrid;
}

//---------------------------------------------------------------------------

void __fastcall TfrmCustomDraw::DrawingClick(TObject *Sender)
{
  switch (((TMenuItem*)Sender)->Tag){
    case 1: {
	  if (((TMenuItem*)Sender)->Checked) {
        DBPivotGrid->OnCustomDrawCell = DBPivotGridCustomDrawCell;
      }
	  else {
        DBPivotGrid->OnCustomDrawCell = NULL;
      }
    break;
    }
    case 3: {
      if (((TMenuItem*)Sender)->Checked) {
        DBPivotGrid->OnCustomDrawColumnHeader = DBPivotGridCustomDrawColumnHeader;
        DBPivotGrid->OnCustomDrawRowHeader = DBPivotGridCustomDrawRowHeader;
      }
      else {
        DBPivotGrid->OnCustomDrawColumnHeader = NULL;
        DBPivotGrid->OnCustomDrawRowHeader = NULL;
      }
    }
  }
  PivotGrid()->LayoutChanged();
}
//---------------------------------------------------------------------------


void __fastcall TfrmCustomDraw::DBPivotGridCustomDrawColumnHeader(
	  TcxCustomPivotGrid *Sender, TcxCanvas *ACanvas,
      TcxPivotGridHeaderCellViewInfo *AViewInfo, bool &ADone)
{
  TPoint P = Sender->ViewData->FocusedCell;
  int AMin = (((TcxPivotGridViewDataItemAccess*)((TcxPivotGridHeaderCellViewInfoAccess*)AViewInfo)->Data))->GetChildLeftVisibleIndex();
  int AMax = (((TcxPivotGridViewDataItemAccess*)((TcxPivotGridHeaderCellViewInfoAccess*)AViewInfo)->Data))->GetChildRightVisibleIndex();
  if ((P.x >= AMin) && (P.x <= AMax))
	FSupportFont->Style = TFontStyles() << fsBold;
  else
  	FSupportFont->Style = TFontStyles();

  TcxViewParams AViewParams = AViewInfo->ViewParams;
  AViewParams.Font = FSupportFont;
  AViewInfo->ViewParams = AViewParams;
}
//---------------------------------------------------------------------------

void __fastcall TfrmCustomDraw::DBPivotGridCustomDrawRowHeader(
	  TcxCustomPivotGrid *Sender, TcxCanvas *ACanvas,
	  TcxPivotGridHeaderCellViewInfo *AViewInfo, bool &ADone)
{
  TPoint P = Sender->ViewData->FocusedCell;
  int AMin = (((TcxPivotGridViewDataItemAccess*)((TcxPivotGridHeaderCellViewInfoAccess*)AViewInfo)->Data))->GetChildLeftVisibleIndex();
  int AMax = (((TcxPivotGridViewDataItemAccess*)((TcxPivotGridHeaderCellViewInfoAccess*)AViewInfo)->Data))->GetChildRightVisibleIndex();
  if ((P.y >= AMin) && (P.y <= AMax))
	FSupportFont->Style = TFontStyles() << fsBold;
  else
	FSupportFont->Style = TFontStyles();

  TcxViewParams AViewParams = AViewInfo->ViewParams;
  AViewParams.Font = FSupportFont;
  AViewInfo->ViewParams = AViewParams;
}
//---------------------------------------------------------------------------

void __fastcall TfrmCustomDraw::DBPivotGridCustomDrawCell(
      TcxCustomPivotGrid *Sender, TcxCanvas *ACanvas,
      TcxPivotGridDataCellViewInfo *AViewInfo, bool &ADone)
{
  if (AViewInfo->LimitValueTypes.Contains(lvtColumnMaximum) && miLimitValues->Checked)
	ACanvas->Font->Color = clRed;
  else
	if (AViewInfo->LimitValueTypes.Contains(lvtColumnMinimum) && miLimitValues->Checked)
	  ACanvas->Font->Color = clLime;
	else
	{
	  int ACellPosition = AViewInfo->ColumnIndex + (AViewInfo->RowIndex << 16);
	  if (FCurrentColorIndex == -1)
	  {
		if ((FList->Count < 5) && (RandomRange(0, 20) == 1))
		  FList->Add(ACellPosition);
	  }
	  else
		if (FList->IndexOf(Pointer(ACellPosition)) > -1)
		{
		  ACanvas->Font->Style = TFontStyles() << fsBold;
		  ACanvas->Font->Color = FColors[FCurrentColorIndex];
		}
	}
  if (AViewInfo->RowIndex % 2 == 0)
	ACanvas->Brush->Color = clSilver;
}
//---------------------------------------------------------------------------
void __fastcall TfrmCustomDraw::DBPivotGridSelectionChanged(TObject *Sender)
{
  PivotGrid()->Invalidate();
}
//---------------------------------------------------------------------------
void __fastcall TfrmCustomDraw::FormCreate(TObject *Sender)
{
  PivotGrid()->DoubleBuffered = true;
  FSupportFont = new TFont();
  FList = new TcxPivotGridRecords();
  FCurrentColorIndex = -1;
  BYTE R = GetRValue(ColorToRGB(clWindowText));
  BYTE G = GetGValue(ColorToRGB(clWindowText));
  BYTE B = GetBValue(ColorToRGB(clWindowText));
  int I = 0;
  while (R < 255)
  {
	FColors[I] = (TColor)RGB(R, G, B);
	R = Min(R + 50, 255);
	I++;
  }
  int ACurrent = I;
  FColorCount = I * 2 - 1;
  for (I = ACurrent; I < FColorCount; I++)
	FColors[I] = FColors[FColorCount - 1 - I];
}
//---------------------------------------------------------------------------
void __fastcall TfrmCustomDraw::FormDestroy(TObject *Sender)
{
  delete FList;
  delete FSupportFont;
}
//---------------------------------------------------------------------------
void __fastcall TfrmCustomDraw::tmrColorChangeTimer(TObject *Sender)
{
  tmrColorChange->Enabled = false;
  FCurrentColorIndex++;
  if (FCurrentColorIndex >= FColorCount)
	FCurrentColorIndex = -1;
  if (FCurrentColorIndex == -1)
  {
	FList->Clear();
	tmrColorChange->Interval = 1000;
  }
  else
	tmrColorChange->Interval = 200;
  PivotGrid()->Invalidate();
  tmrColorChange->Enabled = true;
}
//---------------------------------------------------------------------------


