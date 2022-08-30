//---------------------------------------------------------------------------

#include <vcl.h>
#include "shellapi.hpp"
#pragma hdrstop

#include "SparklinesDemoMain.h"
#include "AboutDemoForm.h"
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
#pragma link "cxButtons"
#pragma link "cxCheckBox"
#pragma link "cxContainer"
#pragma link "cxGridCardView"
#pragma link "cxGridDBCardView"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxMaskEdit"
#pragma link "cxSpinEdit"
#pragma link "cxStyleSheetEditor"
#pragma link "cxTextEdit"
#pragma link "cxLookAndFeels"
#pragma link "cxGridStyleSheetsPreview"
#pragma link "cxDataStorage"
#pragma link "cxGridCustomLayoutView"
#pragma link "cxHyperLinkEdit"
#pragma link "cxImage"
#pragma link "cxNavigator"
#pragma link "dxDBSparkline"
#pragma resource "*.dfm"
TfmSparklines *fmSparklines;

const
  TdxSparklineSeriesType SparklineSeriesTypes[3] = {stArea, stLine, stBar};
  TColor SparklineMarkerColors[2] = {clNone, clDefault};

//---------------------------------------------------------------------------
__fastcall TfmSparklines::TfmSparklines(TComponent* Owner)
  : TfmBaseForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TfmSparklines::miSeriesStyleClick(TObject *Sender)
{
  ((TMenuItem*)Sender)->Checked = True;
  TdxSparklineSeriesType AType = SparklineSeriesTypes[((TComponent*)Sender)->Tag];
  TdxSparklineSeriesCollection *ASeries = ((TdxSparklineProperties*)cxgCarOrdersTableViewPayments->Properties)->Series;
  ASeries->Items[0]->SeriesType = AType;
  ASeries->Items[1]->SeriesType = AType;
  miArea->Checked = ASeries->Items[0]->SeriesType == stArea;
  miLine->Checked = ASeries->Items[0]->SeriesType == stLine;
  miBar->Checked = ASeries->Items[0]->SeriesType == stBar;
}
//---------------------------------------------------------------------------

void __fastcall TfmSparklines::miMarkersClick(TObject *Sender)
{

  bool AChecked = ((TMenuItem*)Sender)->Checked;
  TdxSparklineSeriesCollection *ASeries = ((TdxSparklineProperties*)cxgCarOrdersTableViewPayments->Properties)->Series;

  switch (((TComponent*)Sender)->Tag)
  {
	case 0:
	{
	  ASeries->Items[0]->MarkerColor = SparklineMarkerColors[(int)AChecked];
	  ASeries->Items[1]->MarkerColor = SparklineMarkerColors[(int)AChecked];
	  break;
	}
	case 1:
	{
	  ASeries->Items[0]->MinPointColor = SparklineMarkerColors[(int)AChecked];
	  ASeries->Items[1]->MinPointColor = SparklineMarkerColors[(int)AChecked];
	  break;
	}
	case 2:
	{
	  ASeries->Items[0]->MaxPointColor = SparklineMarkerColors[(int)AChecked];
	  ASeries->Items[1]->MaxPointColor = SparklineMarkerColors[(int)AChecked];
	  break;
	}
  }
}
//---------------------------------------------------------------------------

void __fastcall TfmSparklines::miPaymentAmountTotalByTrademarkClick(TObject *Sender)
{
  ((TdxLookupSparklineProperties*)(cxgCarOrdersTableViewPayments->Properties))->Series->Items[1]->Visible = ((TMenuItem*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TfmSparklines::FormCreate(TObject *Sender)
{
  cxgCarOrdersTableView->BeginUpdate();
  try
  {
	mdsCarOrders->LoadFromBinaryFile("..\\..\\Data\\CarOrders.mds");
	mdsOrderDetails->LoadFromBinaryFile("..\\..\\Data\\OrderDetails.mds");
  }
  __finally
  {
	cxgCarOrdersTableView->EndUpdate();
  }

  cxgCarOrdersTableView->ViewData->Expand(True);
  cxgCarOrdersTableView->Controller->TopRowIndex = 0;
  cxgCarOrdersTableView->ViewData->Rows[0]->Focused = True;
}
//---------------------------------------------------------------------------
