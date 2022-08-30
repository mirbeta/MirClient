//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "RangeControlDemoMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "BaseForm"
#pragma link "cxCheckBox"
#pragma link "cxCheckComboBox"
#pragma link "cxContainer"
#pragma link "cxControls"
#pragma link "cxDropDownEdit"
#pragma link "cxEdit"
#pragma link "cxGraphics"
#pragma link "cxGroupBox"
#pragma link "cxLabel"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma link "cxMaskEdit"
#pragma link "cxRadioGroup"
#pragma link "cxTextEdit"
#pragma link "cxTrackBar"
#pragma link "dxRangeControl"
#pragma resource "*.dfm"
TdxRangeControlDemoForm *dxRangeControlDemoForm;
//---------------------------------------------------------------------------
__fastcall TdxRangeControlDemoForm::TdxRangeControlDemoForm(TComponent* Owner)
	: TfmBaseForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TdxRangeControlDemoForm::rgNumberClientContentTypePropertiesEditValueChanged(TObject *Sender)

{
  rcNumericClient->ContentChanged();
}
//---------------------------------------------------------------------------

void __fastcall TdxRangeControlDemoForm::tbNumericClientPropertiesChange(TObject *Sender)

{
  dynamic_cast<TdxRangeControlNumericClientProperties*>(rcNumericClient->ClientProperties)->ScaleInterval = tbNumericClient->Position;
}
//---------------------------------------------------------------------------

void __fastcall TdxRangeControlDemoForm::rgDateTimeClientContentTypePropertiesEditValueChanged(TObject *Sender)
{
  rcDateTimeClient->ContentChanged();
}
//---------------------------------------------------------------------------

void __fastcall TdxRangeControlDemoForm::tbDateTimeClientPropertiesChange(TObject *Sender)
{
  dynamic_cast<TdxRangeControlDateTimeClientProperties*>(rcDateTimeClient->ClientProperties)->ScaleInterval = tbDateTimeClient->Position;
}
//---------------------------------------------------------------------------

void __fastcall TdxRangeControlDemoForm::chbAutoFormatScaleCaptionsPropertiesEditValueChanged(TObject *Sender)
{
  dynamic_cast<TdxRangeControlDateTimeHeaderClientProperties*>(rcDateTimeHeaderClient->ClientProperties)->AutoFormatScaleCaptions = chbAutoFormatScaleCaptions->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TdxRangeControlDemoForm::cxCheckComboBox1PropertiesEditValueChanged(TObject *Sender)
{
  TdxRangeControlDateTimeHeaderClientProperties *AProperties = dynamic_cast<TdxRangeControlDateTimeHeaderClientProperties*>(rcDateTimeHeaderClient->ClientProperties);
  TdxRangeControlDateTimeScales *AScales = AProperties->Scales;
  for (int i = 0; i < cxCheckComboBox1->Properties->Items->Count; i++)
    AScales->GetScale(TdxRangeControlDateTimeScaleUnit(i + (rcduDay)))->Visible = (cxCheckComboBox1->States[i] == cbsChecked);
}
//---------------------------------------------------------------------------
void __fastcall TdxRangeControlDemoForm::InitializeChartData()
{
  Randomize();
  TDateTime ADateTime = rcDateTimeClient->ClientProperties->MinValue;
  TdxRangeControlDateTimeClientHelper *A = new TdxRangeControlDateTimeClientHelper();
  while (ADateTime <= rcDateTimeClient->ClientProperties->MaxValue)
  {
	FDateTimeClientData1.Length = FDateTimeClientData1.Length + 1;
	FDateTimeClientData1[FDateTimeClientData1.High].X = ADateTime;
	FDateTimeClientData1[FDateTimeClientData1.High].Y = RandomRange(2, 50);
	FDateTimeClientData2.Length = FDateTimeClientData2.Length + 1;
	FDateTimeClientData2[FDateTimeClientData2.High].X = ADateTime;
	FDateTimeClientData2[FDateTimeClientData2.High].Y = RandomRange(52, 100);

	ADateTime = A->IncDate(ADateTime, rcduHour, 3);
  };
  for (int i = rcNumericClient->ClientProperties->MinValue; i <= rcNumericClient->ClientProperties->MaxValue; i++)
  {
	FNumericClientData.Length = FNumericClientData.Length + 1;
	FNumericClientData[FNumericClientData.High].X = i;
	FNumericClientData[FNumericClientData.High].Y = RandomRange(2, 100);
  };
  TDateTime ADateTime1 = rcDateTimeHeaderClient->ClientProperties->MinValue;
  while (ADateTime1 <= rcDateTimeHeaderClient->ClientProperties->MaxValue)
  {
	FDateTimeHeaderClientData1.Length = FDateTimeHeaderClientData1.Length + 1;
	FDateTimeHeaderClientData1[FDateTimeHeaderClientData1.High].X = ADateTime1;
	FDateTimeHeaderClientData1[FDateTimeHeaderClientData1.High].Y = RandomRange(10, 90);
	FDateTimeHeaderClientData2.Length = FDateTimeHeaderClientData2.Length + 1;
	FDateTimeHeaderClientData2[FDateTimeHeaderClientData2.High].X = ADateTime1;
	FDateTimeHeaderClientData2[FDateTimeHeaderClientData2.High].Y = RandomRange(10, 90);

	ADateTime1 = A->IncDate(ADateTime1, rcduDay);
  };
  delete A;
}
//---------------------------------------------------------------------------

void __fastcall TdxRangeControlDemoForm::FormCreate(TObject *Sender)
{
  rcNumericClient->SelectedRangeMaxValue = 10;
  rcDateTimeClient->ClientProperties->MinValue = Date() - 5;
  rcDateTimeClient->ClientProperties->MaxValue = Date() + 5;
  rcDateTimeClient->SelectedRangeMinValue = Date() - 1;
  rcDateTimeClient->SelectedRangeMaxValue = Date() + 1;
  rcDateTimeHeaderClient->ClientProperties->MinValue = Date() - 5;
  rcDateTimeHeaderClient->ClientProperties->MaxValue = Date() + 5;
  rcDateTimeHeaderClient->SelectedRangeMinValue = Date();
  rcDateTimeHeaderClient->SelectedRangeMaxValue = Date() + 2;
  InitializeChartData();
  Width = 850;
  Height = 650;
  Constraints->MinWidth = Width;
  Constraints->MinHeight = Height;
}
//---------------------------------------------------------------------------

void __fastcall TdxRangeControlDemoForm::Animation1Click(TObject *Sender)
{
  bool AChecked = dynamic_cast<TMenuItem*>(Sender)->Checked;
  rcNumericClient->Animation = AChecked;
  rcDateTimeClient->Animation = AChecked;
  rcDateTimeHeaderClient->Animation = AChecked;
}
//---------------------------------------------------------------------------

void __fastcall TdxRangeControlDemoForm::ShowRuler1Click(TObject *Sender)
{
  bool AChecked = dynamic_cast<TMenuItem*>(Sender)->Checked;
  rcNumericClient->ShowRuler = AChecked;
  rcDateTimeClient->ShowRuler = AChecked;
  rcDateTimeHeaderClient->ShowRuler = AChecked;
}
//---------------------------------------------------------------------------

void __fastcall TdxRangeControlDemoForm::ShowZoomscrollbar1Click(TObject *Sender)

{
  bool AChecked = dynamic_cast<TMenuItem*>(Sender)->Checked;
  rcNumericClient->ShowZoomScrollBar = AChecked;
  rcDateTimeClient->ShowZoomScrollBar = AChecked;
  rcDateTimeHeaderClient->ShowZoomScrollBar = AChecked;
}
//---------------------------------------------------------------------------
bool __fastcall TdxRangeControlDemoForm::IsNumberClientContentLineMode()
{
  return !rgNumberClientContentType->ItemIndex;
}
//---------------------------------------------------------------------------
bool __fastcall TdxRangeControlDemoForm::IsDateTimeClientContentLineMode()
{
  return !rgDateTimeClientContentType->ItemIndex;
}
//---------------------------------------------------------------------------
void __fastcall TdxRangeControlDemoForm::AddPolygonPoint(TPoints &APolygon, int X, int Y)
{
  APolygon.Length = APolygon.Length + 1;
  APolygon[APolygon.High].x = X;
  APolygon[APolygon.High].y = Y;
}
//---------------------------------------------------------------------------
void __fastcall TdxRangeControlDemoForm::GetPoints(TdxRangeControl *ARangeControl, TdxRangeControlCustomClientViewInfo *AViewInfo,
  TRect R, TChartDatas ADataSource, TPoints &APolygon)
{
  int AVisibleRangeMinValue = ARangeControl->VisibleRangeMinValue;
  int AVisibleRangeMaxValue = ARangeControl->VisibleRangeMaxValue;
  for (int i = 0; i < ADataSource.Length; i++)
	{
	if (InRange(ADataSource[i].X, AVisibleRangeMinValue - 1, AVisibleRangeMaxValue + 1))
	{
	  int APos = ARangeControl->GetPositionFromValue(ADataSource[i].X);
	  int AValue = R.Bottom - ADataSource[i].Y * R.Height() / 100;
      AddPolygonPoint(APolygon, APos, AValue);
	};
   }
}
//---------------------------------------------------------------------------
void __fastcall TdxRangeControlDemoForm::DrawNumericData(
  TcxCanvas *ACanvas, TdxRangeControlCustomClientViewInfo *AViewInfo, TPoints APolygon, TRect R, TdxAlphaColor AColor)
{
	dxGPPaintCanvas()->BeginPaint(ACanvas->Handle, AViewInfo->Bounds);
	dxGPPaintCanvas()->SmoothingMode = smAntiAlias;
	try
	{
	  if (IsNumberClientContentLineMode())
		dxGPPaintCanvas()->Polyline(&APolygon[0], APolygon.Length - 1, AColor, 1, psSolid);
	  else
		{
		  for (int i = 0; i < APolygon.Length; i++)
		  {
			int APos = R.Bottom;
			dxGPPaintCanvas()->Line(APolygon[i].x, APos, APolygon[i].x, APolygon[i].y, AColor, 3);
		  }
		}
	}
	__finally
	  {
		dxGPPaintCanvas()->EndPaint();
	  };
}
//---------------------------------------------------------------------------

void __fastcall TdxRangeControlDemoForm::rcNumericClientDrawContent(TdxCustomRangeControl *ASender,
	TcxCanvas *ACanvas, TdxRangeControlCustomClientViewInfo *AViewInfo,
	bool &AHandled)
{
	TPoints APolygon;
	APolygon.Length = 0;
	GetPoints(rcNumericClient, AViewInfo, AViewInfo->Content->Bounds, FNumericClientData, APolygon);
	DrawNumericData(ACanvas, AViewInfo, APolygon, AViewInfo->Content->Bounds, 0xFFEE8C4B);
}
//---------------------------------------------------------------------------
void __fastcall TdxRangeControlDemoForm::DrawDateTimeData(
	TcxCanvas *ACanvas, TdxRangeControlCustomClientViewInfo *AViewInfo, TPoints APolygon,
	TRect R, TdxAlphaColor AColor, TdxAlphaColor ABrushColor)
{
	dxGPPaintCanvas()->BeginPaint(ACanvas->Handle, AViewInfo->Bounds);
	dxGPPaintCanvas()->SmoothingMode = smAntiAlias;
	try
	{
	  if (IsDateTimeClientContentLineMode())
		dxGPPaintCanvas()->Polyline(&APolygon[0], APolygon.Length - 1, AColor, 1, psSolid);
	  else
        dxGPPaintCanvas()->Polygon(&APolygon[0], APolygon.Length - 1, AColor, ABrushColor, 1, psSolid);
	}
	__finally
	  {
		dxGPPaintCanvas()->EndPaint();
	  };
}
//---------------------------------------------------------------------------
void __fastcall TdxRangeControlDemoForm::rcDateTimeClientDrawContent(TdxCustomRangeControl *ASender,
	TcxCanvas *ACanvas, TdxRangeControlCustomClientViewInfo *AViewInfo,
	bool &AHandled)
{
	TPoints APolygon;
	TRect R = AViewInfo->Content->Bounds;
	APolygon.Length = 0;
	if (!IsDateTimeClientContentLineMode())
	  AddPolygonPoint(APolygon, R.Left, R.Bottom);
	GetPoints(rcDateTimeClient, AViewInfo, R, FDateTimeClientData2, APolygon);
	if (!IsDateTimeClientContentLineMode())
	  AddPolygonPoint(APolygon, R.Right, R.Bottom);
	DrawDateTimeData(ACanvas, AViewInfo, APolygon, R, 0xFFEE8C4B, 0x70EE8C4B);
	APolygon.Length = 0;
	if (!IsDateTimeClientContentLineMode())
	  AddPolygonPoint(APolygon, R.Left, R.Bottom);
	GetPoints(rcDateTimeClient, AViewInfo, R, FDateTimeClientData1, APolygon);
	if (!IsDateTimeClientContentLineMode())
	  AddPolygonPoint(APolygon, R.Right, R.Bottom);
	DrawDateTimeData(ACanvas, AViewInfo, APolygon, R, 0xFF6AA4D9, 0x906AA4D9);
}
//---------------------------------------------------------------------------
int __fastcall TdxRangeControlDemoForm::GetDateTimeHeaderValue(TChartDatas ADataSource, TdxRangeControlDateTimeHeaderClientContentElementViewInfo *AElement)
{
	for (int i = 0; i < ADataSource.Length; i++) {
	  if ((ADataSource[i].X >= AElement->MinDate)&&(ADataSource[i].X < AElement->MaxDate)) {
		return(ADataSource[i].Y);
	  }
	}
}
//---------------------------------------------------------------------------

void __fastcall TdxRangeControlDemoForm::rcDateTimeHeaderClientDrawContent(TdxCustomRangeControl *ASender,
          TcxCanvas *ACanvas, TdxRangeControlCustomClientViewInfo *AViewInfo,
          bool &AHandled)
{
    TdxRangeControlDateTimeHeaderClientContentViewInfo *AContentElementsViewInfo = dynamic_cast<TdxRangeControlDateTimeHeaderClientContentViewInfo*>(AViewInfo->Content);
	dxGPPaintCanvas()->BeginPaint(ACanvas->Handle, AViewInfo->Bounds);
	dxGPPaintCanvas()->SmoothingMode = smAntiAlias;
	try
	{
		for (int i = 0; i < AContentElementsViewInfo->Elements->Count; i++)
		{
			TdxRangeControlDateTimeHeaderClientContentElementViewInfo *AElement = AContentElementsViewInfo->Elements->Items[i];
			dxGPPaintCanvas()->SaveClipRegion();
			try
			{
				dxGPPaintCanvas()->SetClipRect(AElement->Bounds, gmIntersect);

				int AValue = GetDateTimeHeaderValue(FDateTimeHeaderClientData1, AElement);
				TRect ARect = AElement->Bounds;
				ARect.Right = cxRectCenter(ARect).x - 2;
				ARect.Left = ARect.Right - 15;
				ARect.Bottom ++;
				ARect.Top = ARect.Bottom - AValue * ARect.Height() / 100;
				dxGPPaintCanvas()->Rectangle(ARect, 0xFF6AA4D9, 0x646AA4D9);

                AValue = GetDateTimeHeaderValue(FDateTimeHeaderClientData2, AElement);
				ARect = AElement->Bounds;
				ARect.Left = cxRectCenter(ARect).x + 2;
				ARect.Right = ARect.Left + 15;
				ARect.Bottom ++;
				ARect.Top = ARect.Bottom - AValue * ARect.Height() / 100;
				dxGPPaintCanvas()->Rectangle(ARect, 0xFFEE8C4B, 0x64EE8C4B);
			}
			__finally
			{
				dxGPPaintCanvas()->RestoreClipRegion();
			};
		 }
	  }
	__finally
	  {
		dxGPPaintCanvas()->EndPaint();
	  }
}
//---------------------------------------------------------------------------


void __fastcall TdxRangeControlDemoForm::FormResize(TObject *Sender)
{
  int ControlCount = 3;
  int Interval = 10;
  TRect R = cxGroupBox5->ClientBounds;
  int AHeight = (R.Height() - 4 * Interval) / ControlCount;
  cxGroupBox1->Top = R.Top + Interval;
  cxGroupBox1->Height = AHeight;
  cxGroupBox2->Top = cxGroupBox1->Top + AHeight + Interval;
  cxGroupBox2->Height = AHeight;
  cxGroupBox3->Top = cxGroupBox2->Top + AHeight + Interval;
  cxGroupBox3->Height = AHeight;
}
//---------------------------------------------------------------------------

void __fastcall TdxRangeControlDemoForm::FormDestroy(TObject *Sender)
{
//
}
//---------------------------------------------------------------------------

