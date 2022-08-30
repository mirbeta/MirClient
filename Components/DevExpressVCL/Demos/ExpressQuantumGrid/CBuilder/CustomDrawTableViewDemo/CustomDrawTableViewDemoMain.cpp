//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "CustomDrawTableViewDemoMain.h"
#include "CustomDrawTableViewDemoData.h"
#include "AboutDemoForm.h"
#include "CustomDrawTableViewDemoStylesEditor.h"
#include "shellapi.h"
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
#pragma link "cxGridCommon"
#pragma link "cxGridCustomTableView"
#pragma link "cxGridCustomView"
#pragma link "cxGridDBTableView"
#pragma link "cxGridLevel"
#pragma link "cxGridTableView"
#pragma link "cxStyles"
#pragma link "cxLookAndFeels"
#pragma link "cxBlobEdit"
#pragma link "cxDataStorage"
#pragma link "cxImageComboBox"
#pragma link "cxTimeEdit"
#pragma link "cxBlobEdit"
#pragma link "cxDataStorage"
#pragma link "cxGridCardView"
#pragma link "cxImageComboBox"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxTimeEdit"
#pragma resource "*.dfm"
TCustomDrawTableViewDemoMainForm *CustomDrawTableViewDemoMainForm;
//---------------------------------------------------------------------------
__fastcall TCustomDrawTableViewDemoMainForm::TCustomDrawTableViewDemoMainForm(TComponent* Owner)
  : TfmBaseForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawTableViewDemoMainForm::FormShow(TObject *Sender)
{
  CustomDrawTableViewDemoStylesEditorForm->Show();
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawTableViewDemoMainForm::miCustomDrawEditorClick(
      TObject *Sender)
{
  CustomDrawTableViewDemoStylesEditorForm->Show();
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawTableViewDemoMainForm::SetCustomDrawingStyle(TViewType AViewType, TCustomDrawArea ACustomDrawArea, const TCustomDrawingStyle Value)
{
  if (FCustomDrawingStyle[(int)AViewType][(int)ACustomDrawArea] != Value) {
    FCustomDrawingStyle[(int)AViewType][(int)ACustomDrawArea] = Value;
    switch (AViewType) {
      case vtMaster: {
        tvCars->LayoutChanged(false);
        tvCars->Painter->Invalidate();
      }; break;
      case vtDetail: {
        tvOrders->LayoutChanged(false);
        tvOrders->Painter->Invalidate();
      }; break;
    };
  };
}
//---------------------------------------------------------------------------

TCustomDrawingStyle __fastcall TCustomDrawTableViewDemoMainForm::GetCustomDrawingStyle(TViewType AViewType, TCustomDrawArea ACustomDrawArea)
{
  return(FCustomDrawingStyle[(int)AViewType][(int)ACustomDrawArea]);
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawTableViewDemoMainForm::SetCustomDrawingStyles()
{
  FGridBrushMasterCell = new TBrush();
  FGridBrushMasterGroupCell = new TBrush();
  FGridBrushMasterFooterCell = new TBrush();
  FGridBrushMasterColumnHeader = new TBrush();
  FGridBrushMasterPartBackground = new TBrush();
  FGridBrushMasterIndicatorCell = new TBrush();

  FGridBrushDetailCell = new TBrush();
  FGridBrushDetailGroupCell = new TBrush();
  FGridBrushDetailFooterCell = new TBrush();
  FGridBrushDetailColumnHeader = new TBrush();
  FGridBrushDetailPartBackground = new TBrush();
  FGridBrushDetailIndicatorCell = new TBrush();

  FIndicatorImageIndex[(int)vtMaster] = 0;
  FIndicatorImageIndex[(int)vtDetail] = 0;

  for (int i=0; i<2; i++)
    for (int j=0; j<6; j++)
      FBkImages[i][j] = bkiTile;

  FGridBrushMasterCell->Bitmap = FTileBitmap;
  FGridBrushMasterGroupCell->Bitmap = FTileBitmap;
  FGridBrushMasterColumnHeader->Bitmap = FTileBitmap;
  FGridBrushMasterFooterCell->Bitmap = FTileBitmap;
  FGridBrushMasterPartBackground->Bitmap = FTileBitmap;
  FGridBrushMasterIndicatorCell->Bitmap = FTileBitmap;

  FGridBrushDetailCell->Bitmap = FTileBitmap;
  FGridBrushDetailGroupCell->Bitmap = FTileBitmap;
  FGridBrushDetailColumnHeader->Bitmap = FTileBitmap;
  FGridBrushDetailFooterCell->Bitmap = FTileBitmap;
  FGridBrushDetailPartBackground->Bitmap = FTileBitmap;
  FGridBrushDetailIndicatorCell->Bitmap = FTileBitmap;

  FColorScheme[(int)vtMaster][0] = csBlue;
  FColorScheme[(int)vtDetail][1] = csBlue;
  for (int i=0; i<2; i++)
    for (int j=0; j<6; j++)
      FCustomDrawingStyle[i][j] = cdsGradient;
}
//---------------------------------------------------------------------------

int __fastcall TCustomDrawTableViewDemoMainForm::GetIndicatorImageIndex(TViewType AViewType)
{
  return(FIndicatorImageIndex[(int)AViewType]);
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawTableViewDemoMainForm::SetIndicatorImageIndex(TViewType AViewType, const int Value)
{
  if (FIndicatorImageIndex[(int)AViewType] != Value) {
    FIndicatorImageIndex[(int)AViewType] = Value;
    switch (AViewType) {
      case vtMaster: {
        tvCars->LayoutChanged(false);
        tvCars->Painter->Invalidate();
      }; break;
      case vtDetail: {
        tvOrders->LayoutChanged(false);
        tvOrders->Painter->Invalidate();
      }; break;
    };
  };
}
//---------------------------------------------------------------------------

TBkImage __fastcall TCustomDrawTableViewDemoMainForm::GetCustomBkImage(TViewType AViewType, TCustomDrawArea ACustomDrawArea)
{
  return(FBkImages[(int)AViewType][(int)ACustomDrawArea]);
}
//---------------------------------------------------------------------------

Graphics::TBitmap* __fastcall TCustomDrawTableViewDemoMainForm::GetImage(TBkImage AValue)
{
  switch (AValue) {
    case bkiTile: return(FTileBitmap);
    case bkiSky: return(FSkyBitmap);
    case bkiEgypt: return(FEgyptBitmap);
    case bkiMyFace: return(FMyFaceBitmap);
  };
  return (NULL);
};
//---------------------------------------------------------------------------

void __fastcall TCustomDrawTableViewDemoMainForm::SetCustomBkImage(TViewType AViewType, TCustomDrawArea ACustomDrawArea, TBkImage AValue)
{
  if(AValue != bkiUserDefined) {
    if(FBkImages[(int)AViewType][(int)ACustomDrawArea] != AValue) {
      FBkImages[(int)AViewType][(int)ACustomDrawArea] = AValue;
      switch (AViewType) {
        case vtMaster: {
          switch (ACustomDrawArea) {
            case cdaCell: FGridBrushMasterCell->Bitmap = GetImage(AValue); break;
            case cdaGroupCell: FGridBrushMasterGroupCell->Bitmap = GetImage(AValue); break;
            case cdaColumnHeader: FGridBrushMasterColumnHeader->Bitmap = GetImage(AValue); break;
            case cdaFooterCell: FGridBrushMasterFooterCell->Bitmap = GetImage(AValue); break;
            case cdaIndicatorCell: FGridBrushMasterIndicatorCell->Bitmap = GetImage(AValue); break;
            case cdaPartBackGround: FGridBrushMasterPartBackground->Bitmap = GetImage(AValue); break;
          };
          tvCars->LayoutChanged(false);
          tvCars->Painter->Invalidate();
        }; break;
        case vtDetail: {
          switch (ACustomDrawArea) {
            case cdaCell: FGridBrushDetailCell->Bitmap = GetImage(AValue); break;
            case cdaGroupCell: FGridBrushDetailGroupCell->Bitmap = GetImage(AValue); break;
            case cdaColumnHeader: FGridBrushDetailColumnHeader->Bitmap = GetImage(AValue); break;
            case cdaFooterCell: FGridBrushDetailFooterCell->Bitmap = GetImage(AValue); break;
            case cdaIndicatorCell: FGridBrushDetailIndicatorCell->Bitmap = GetImage(AValue); break;
            case cdaPartBackGround: FGridBrushDetailPartBackground->Bitmap = GetImage(AValue); break;
          };
          tvOrders->LayoutChanged(false);
          tvOrders->Painter->Invalidate();
        }; break;
      };
    };
  }
  else {
    FBkImages[(int)AViewType][(int)ACustomDrawArea] = AValue;
    switch (AViewType) {
      case vtMaster: {
        tvCars->LayoutChanged(false);
        tvCars->Painter->Invalidate();
      }; break;
      case vtDetail: {
        tvOrders->LayoutChanged(false);
        tvOrders->Painter->Invalidate();
      }; break;
    };
  };
}
//---------------------------------------------------------------------------

CustomDrawTableViewDemoTypesH::TColorScheme __fastcall TCustomDrawTableViewDemoMainForm::GetCustomColorScheme(TViewType AViewType, TCustomDrawArea ACustomDrawArea)
{
  return(FColorScheme[(int)AViewType][(int)ACustomDrawArea]);
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawTableViewDemoMainForm::SetCustomColorScheme(TViewType AViewType, TCustomDrawArea ACustomDrawArea, const CustomDrawTableViewDemoTypesH::TColorScheme Value)
{
  if(FColorScheme[(int)AViewType][(int)ACustomDrawArea] != Value) {
    FColorScheme[(int)AViewType][(int)ACustomDrawArea] = Value;
    switch (AViewType) {
      case vtMaster: {
        tvCars->LayoutChanged(false);
        tvCars->Painter->Invalidate();
      }; break;
      case vtDetail: {
        tvOrders->LayoutChanged(false);
        tvOrders->Painter->Invalidate();
      }; break;
    };
  };
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawTableViewDemoMainForm::SetUserDefineBitmap(TViewType AViewType, TCustomDrawArea ACustomDrawArea, Graphics::TBitmap* Value)
{
  switch (AViewType) {
    case vtMaster: {
      switch (ACustomDrawArea) {
        case cdaCell: FGridBrushMasterCell->Bitmap = Value; break;
        case cdaGroupCell: FGridBrushMasterGroupCell->Bitmap = Value; break;
        case cdaColumnHeader: FGridBrushMasterColumnHeader->Bitmap = Value; break;
        case cdaFooterCell: FGridBrushMasterFooterCell->Bitmap = Value; break;
        case cdaIndicatorCell: FGridBrushMasterIndicatorCell->Bitmap = Value; break;
        case cdaPartBackGround: FGridBrushMasterPartBackground->Bitmap = Value; break;
      };
    };
    case vtDetail: {
      switch (ACustomDrawArea) {
        case cdaCell: FGridBrushDetailCell->Bitmap = Value; break;
        case cdaGroupCell: FGridBrushDetailGroupCell->Bitmap = Value; break;
        case cdaColumnHeader: FGridBrushDetailColumnHeader->Bitmap = Value; break;
        case cdaFooterCell: FGridBrushDetailFooterCell->Bitmap = Value; break;
        case cdaIndicatorCell: FGridBrushDetailIndicatorCell->Bitmap = Value; break;
        case cdaPartBackGround: FGridBrushMasterPartBackground->Bitmap = Value; break;
      };
    };
  };
}
//---------------------------------------------------------------------------

TFont* __fastcall TCustomDrawTableViewDemoMainForm::GetFont(TViewType AViewType, TCustomDrawArea ACustomDrawArea)
{
  return(FFonts[(int)AViewType][(int)ACustomDrawArea]);
}
//---------------------------------------------------------------------------


void __fastcall TCustomDrawTableViewDemoMainForm::SetFont(TViewType AViewType, TCustomDrawArea ACustomDrawArea, const TFont* Value)
{
  FFonts[(int)AViewType][(int)ACustomDrawArea]->Assign((TPersistent*)Value);
  switch (AViewType) {
    case vtMaster: {
      tvCars->LayoutChanged(false);
      tvCars->Painter->Invalidate();
    }; break;
    case vtDetail: {
      tvOrders->LayoutChanged(false);
      tvOrders->Painter->Invalidate(); 
    }; break;
  };
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawTableViewDemoMainForm::InitFonts()
{
  for(int i=0; i < 2; i++)
    for(int j=0; j < 6; j++)
      FFonts[i][j] = new TFont();
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawTableViewDemoMainForm::tvCarsCustomDrawCell(
      TcxCustomGridTableView *Sender, TcxCanvas *ACanvas,
      TcxGridTableDataCellViewInfo *AViewInfo, bool &ADone)
{
  String ATextToDraw = AViewInfo->GridRecord->DisplayTexts[AViewInfo->Item->Index];
  TRect ARec = AViewInfo->Bounds;
  ACanvas->Canvas->Font->Assign(FFonts[(int)vtMaster][(int)cdaCell]);
  switch(FCustomDrawingStyle[(int)vtMaster][(int)cdaCell]) {
    case cdsBkImage: {
      ACanvas->Brush->Bitmap = NULL;
      ACanvas->Brush->Assign(FGridBrushMasterCell);
      ACanvas->FillRect(ARec, NULL, False);
    }; break;
    case cdsGradient: {
      bool AIsVertical = (FColorScheme[(int)vtMaster][(int)cdaCell] == csGrey) || (FColorScheme[(int)vtMaster][(int)cdaCell] == csGold);
      DrawGradient(ACanvas->Canvas, ARec, ColorScheme[(int)FColorScheme[(int)vtMaster][(int)cdaCell]][1], ColorScheme[(int)FColorScheme[(int)vtMaster][(int)cdaCell]][0], 40, AIsVertical);
    }; break;
    case cdsDependsOnData: {
      ACanvas->Canvas->Brush->Style = bsSolid;
      ACanvas->Canvas->Brush->Color = clBlueLight;

	  String val = AViewInfo->GridRecord->DisplayTexts[tvCarsCategory->Index];
	  if (val == "SPORTS")
        ACanvas->Canvas->Font->Color = clRed;
      else
      if (val == "SALOON")
        ACanvas->Canvas->Font->Color = clBlue;
      else
      if (val == "TRUCK")
        ACanvas->Canvas->Font->Color = clGreen;
      ACanvas->Canvas->FillRect(ARec);
    }; break;
  };

  SetBkMode(ACanvas->Canvas->Handle, TRANSPARENT);
  ADone = !((TCustomDrawingStyles() << cdsDefaultDrawing << cdsDependsOnData).Contains(FCustomDrawingStyle[(int)vtMaster][(int)cdaCell]));
  if (ADone) {
    InflateRect(&ARec, -cxGridCellTextOffset, -cxGridCellTextOffset);
    ACanvas->DrawTexT(ATextToDraw, ARec, 0, true);
  }
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawTableViewDemoMainForm::tvCarsCustomDrawGroupCell(
      TcxCustomGridTableView *Sender, TcxCanvas *ACanvas,
      TcxGridTableCellViewInfo *AViewInfo, bool &ADone)
{
  String ATextToDraw = AViewInfo->Text;
  TRect ARec = AViewInfo->Bounds;
  ACanvas->Canvas->Font->Assign(FFonts[(int)vtMaster][(int)cdaGroupCell]);
  switch (FCustomDrawingStyle[(int)vtMaster][(int)cdaGroupCell]) {
    case cdsBkImage: {
      ACanvas->Brush->Bitmap = NULL;
      ACanvas->Brush->Assign(FGridBrushMasterGroupCell);
      ACanvas->FillRect(ARec, NULL, False);
    }; break;
    case cdsGradient: {
      bool AIsVertical = (FColorScheme[(int)vtMaster][(int)cdaGroupCell] == csGrey) || (FColorScheme[(int)vtMaster][(int)cdaGroupCell] == csGold);
      DrawGradient(ACanvas->Canvas, ARec, ColorScheme[(int)FColorScheme[(int)vtMaster][(int)cdaGroupCell]][1], ColorScheme[(int)FColorScheme[(int)vtMaster][(int)cdaGroupCell]][0], 40, AIsVertical);
    }; break;
  };
  ADone = !(TCustomDrawingStyles() << cdsDefaultDrawing).Contains(FCustomDrawingStyle[(int)vtMaster][(int)cdaGroupCell]);
  if(ADone) {
    SetBkMode(ACanvas->Canvas->Handle, TRANSPARENT);
    ARec.Left = ARec.Left + (AViewInfo->RecordViewInfo->ExpandButtonBounds.Right - AViewInfo->RecordViewInfo->ExpandButtonBounds.Left) + 10;
    ARec.Top = ARec.Top + cxGridCellTextOffset;
    ACanvas->DrawTexT(ATextToDraw, ARec, 0, true);
  };
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawTableViewDemoMainForm::tvCarsCustomDrawColumnHeader(
      TcxGridTableView *Sender, TcxCanvas *ACanvas,
      TcxGridColumnHeaderViewInfo *AViewInfo, bool &ADone)
{
  String ATextToDraw = AViewInfo->Text;
  TRect ARec = AViewInfo->Bounds;
  ACanvas->Canvas->Font->Assign(FFonts[(int)vtMaster][(int)cdaColumnHeader]);
  switch(FCustomDrawingStyle[(int)vtMaster][(int)cdaColumnHeader]) {
    case cdsBkImage: {
      ACanvas->Brush->Bitmap = NULL;
      ACanvas->Brush->Assign(FGridBrushMasterColumnHeader);
      ACanvas->FillRect(ARec, NULL, False);
    }; break;
    case cdsGradient: {
      bool AIsVertical = (FColorScheme[(int)vtMaster][(int)cdaColumnHeader] == csGrey) || (FColorScheme[(int)vtMaster][(int)cdaColumnHeader] == csGold);
      DrawGradient(ACanvas->Canvas, ARec, ColorScheme[(int)FColorScheme[(int)vtMaster][(int)cdaColumnHeader]][1], ColorScheme[(int)FColorScheme[(int)vtMaster][(int)cdaColumnHeader]][0], 40, AIsVertical);
    }; break;
  };

  SetBkMode(ACanvas->Canvas->Handle, TRANSPARENT);
  InflateRect(&ARec, -cxGridCellTextOffset, -cxGridCellTextOffset);
  ACanvas->DrawTexT(ATextToDraw, ARec, 0, true);

  ADone = FCustomDrawingStyle[(int)vtMaster][(int)cdaColumnHeader] != cdsDefaultDrawing;
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawTableViewDemoMainForm::tvCarsCustomDrawFooterCell(
      TcxGridTableView *Sender, TcxCanvas *ACanvas,
      TcxGridColumnHeaderViewInfo *AViewInfo, bool &ADone)
{
  String ATextToDraw = AViewInfo->Text;
  TRect ARec = AViewInfo->Bounds;
  ACanvas->Canvas->Font->Assign(FFonts[(int)vtMaster][(int)cdaFooterCell]);
  switch(FCustomDrawingStyle[(int)vtMaster][(int)cdaFooterCell]) {
    case cdsBkImage: {
      ACanvas->Brush->Bitmap = NULL;
      ACanvas->Brush->Assign(FGridBrushMasterFooterCell);
      ACanvas->Canvas->FillRect(ARec);
    }; break;
    case cdsGradient: {
      bool AIsVertical = (FColorScheme[(int)vtMaster][(int)cdaFooterCell] == csGrey) || (FColorScheme[(int)vtMaster][(int)cdaFooterCell] == csGold);
      DrawGradient(ACanvas->Canvas, ARec, ColorScheme[(int)FColorScheme[(int)vtMaster][(int)cdaFooterCell]][1],
      ColorScheme[(int)FColorScheme[(int)vtMaster][(int)cdaFooterCell]][0], 40, AIsVertical);
    }; break;
  };

  SetBkMode(ACanvas->Canvas->Handle, TRANSPARENT);
  InflateRect(&ARec, -cxGridCellTextOffset, -cxGridCellTextOffset);
  ACanvas->DrawTexT(ATextToDraw, ARec, 0, true);

  ADone = FCustomDrawingStyle[(int)vtMaster][(int)cdaFooterCell] != cdsDefaultDrawing;
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawTableViewDemoMainForm::tvCarsCustomDrawPartBackground(
      TcxCustomGridTableView *Sender, TcxCanvas *ACanvas,
      TcxCustomGridCellViewInfo *AViewInfo, bool &ADone)
{
  TRect ARec = AViewInfo->Bounds;
  ACanvas->Canvas->Font->Assign(FFonts[(int)vtMaster][(int)cdaPartBackGround]);
  switch(FCustomDrawingStyle[(int)vtMaster][(int)cdaPartBackGround]) {
    case cdsBkImage: {
      ACanvas->Brush->Bitmap = NULL;
      ACanvas->Brush->Assign(FGridBrushMasterPartBackground);
      ACanvas->Canvas->FillRect(ARec);
    }; break;
    case cdsGradient: {
      bool AIsVertical = (FColorScheme[(int)vtMaster][(int)cdaPartBackGround] == csGrey) ||
        (FColorScheme[(int)vtMaster][(int)cdaPartBackGround] == csGold);
      DrawGradient(ACanvas->Canvas, ARec, ColorScheme[(int)FColorScheme[(int)vtMaster][(int)cdaPartBackGround]][1],
         ColorScheme[(int)FColorScheme[(int)vtMaster][(int)cdaPartBackGround]][0], 40, AIsVertical);
    }; break;
  };

  ADone = FCustomDrawingStyle[(int)vtMaster][(int)cdaPartBackGround] != cdsDefaultDrawing;
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawTableViewDemoMainForm::tvOrdersCustomDrawCell(
      TcxCustomGridTableView *Sender, TcxCanvas *ACanvas,
      TcxGridTableDataCellViewInfo *AViewInfo, bool &ADone)
{
  String ATextToDraw = AViewInfo->GridRecord->DisplayTexts[AViewInfo->Item->Index];
  TRect ARec = AViewInfo->Bounds;
  ACanvas->Canvas->Font->Assign(FFonts[(int)vtDetail][(int)cdaCell]);
  switch(FCustomDrawingStyle[(int)vtDetail][(int)cdaCell]) {
    case cdsBkImage: {
      ACanvas->Brush->Bitmap = NULL;
      ACanvas->Brush->Assign(FGridBrushDetailCell);
      ACanvas->Canvas->FillRect(ARec);
    }; break;
    case cdsGradient: {
      bool AIsVertical = (FColorScheme[(int)vtDetail][(int)cdaCell] == csGrey) || (FColorScheme[(int)vtDetail][(int)cdaCell] == csGold);
      DrawGradient(ACanvas->Canvas, ARec, ColorScheme[(int)FColorScheme[(int)vtDetail][(int)cdaCell]][1], ColorScheme[(int)FColorScheme[(int)vtDetail][(int)cdaCell]][0], 40, AIsVertical);
    }; break;
    case cdsDependsOnData: {
      ACanvas->Canvas->Brush->Style = bsSolid;
      ACanvas->Canvas->Brush->Color = clBlueLight;

	  String val = AViewInfo->GridRecord->DisplayTexts[tvOrdersPaymentType->Index];
      if (val == "Master Card")
        ACanvas->Canvas->Font->Color = clRed;
      else
      if (val == "American Express")
        ACanvas->Canvas->Font->Color = clBlue;
      else
      if (val == "Cash")
        ACanvas->Canvas->Font->Color = clGreen;
      if (val == "Visa Card")
        ACanvas->Canvas->Font->Color = clFuchsia;
      ACanvas->Canvas->FillRect(ARec);
    }; break;
  };

  SetBkMode(ACanvas->Canvas->Handle, TRANSPARENT);

  ADone = !((TCustomDrawingStyles() << cdsDefaultDrawing << cdsDependsOnData).Contains(FCustomDrawingStyle[(int)vtDetail][(int)cdaCell]));
  if (ADone) {
    InflateRect(&ARec, -cxGridCellTextOffset, -cxGridCellTextOffset);
    ACanvas->DrawTexT(ATextToDraw, ARec, 0, true);
  }
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawTableViewDemoMainForm::tvOrdersCustomDrawGroupCell(
      TcxCustomGridTableView *Sender, TcxCanvas *ACanvas,
      TcxGridTableCellViewInfo *AViewInfo, bool &ADone)
{
  String ATextToDraw = AViewInfo->Text;
  TRect ARec = AViewInfo->Bounds;
  ACanvas->Canvas->Font->Assign(FFonts[(int)vtDetail][(int)cdaGroupCell]);
  switch (FCustomDrawingStyle[(int)vtDetail][(int)cdaGroupCell]) {
    case cdsBkImage: {
      ACanvas->Brush->Bitmap = NULL;
      ACanvas->Brush->Assign(FGridBrushDetailGroupCell);
      ACanvas->FillRect(ARec, NULL, False);
    }; break;
    case cdsGradient: {
      bool AIsVertical = (FColorScheme[(int)vtDetail][(int)cdaGroupCell] == csGrey) || (FColorScheme[(int)vtDetail][(int)cdaGroupCell] == csGold);
      DrawGradient(ACanvas->Canvas, ARec, ColorScheme[(int)FColorScheme[(int)vtDetail][(int)cdaGroupCell]][1], ColorScheme[(int)FColorScheme[(int)vtDetail][(int)cdaGroupCell]][0], 40, AIsVertical);
    }; break;
  };
  ADone = !(TCustomDrawingStyles() << cdsDefaultDrawing).Contains(FCustomDrawingStyle[(int)vtDetail][(int)cdaGroupCell]);
  if(ADone) {
    SetBkMode(ACanvas->Canvas->Handle, TRANSPARENT);
    ARec.Left = ARec.Left + (AViewInfo->RecordViewInfo->ExpandButtonBounds.Right - AViewInfo->RecordViewInfo->ExpandButtonBounds.Left) + 10;
    ARec.Top = ARec.Top + cxGridCellTextOffset;
    ACanvas->DrawTexT(ATextToDraw, ARec, 0, true);
  };
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawTableViewDemoMainForm::tvOrdersCustomDrawColumnHeader(
      TcxGridTableView *Sender, TcxCanvas *ACanvas,
      TcxGridColumnHeaderViewInfo *AViewInfo, bool &ADone)
{
  String ATextToDraw = AViewInfo->Text;
  TRect ARec = AViewInfo->Bounds;
  ACanvas->Canvas->Font->Assign(FFonts[(int)vtDetail][(int)cdaColumnHeader]);
  switch(FCustomDrawingStyle[(int)vtDetail][(int)cdaColumnHeader]) {
    case cdsBkImage: {
      ACanvas->Brush->Bitmap = NULL;
      ACanvas->Brush->Assign(FGridBrushDetailColumnHeader);
      ACanvas->Canvas->FillRect(ARec);
    }; break;
    case cdsGradient: {
      bool AIsVertical = (FColorScheme[(int)vtDetail][(int)cdaColumnHeader] == csGrey) ||
        (FColorScheme[(int)vtDetail][(int)cdaColumnHeader] == csGold);
      DrawGradient(ACanvas->Canvas, ARec, ColorScheme[(int)FColorScheme[(int)vtDetail][(int)cdaColumnHeader]][1],
        ColorScheme[(int)FColorScheme[(int)vtDetail][(int)cdaColumnHeader]][0], 40, AIsVertical);
    }; break;
    case cdsDependsOnData: {
      ACanvas->Canvas->Brush->Style = bsSolid;
      ACanvas->Canvas->Brush->Color = clBlueLight;
      ACanvas->Canvas->FillRect(ARec);
    }; break;
  };

  SetBkMode(ACanvas->Canvas->Handle, TRANSPARENT);
  InflateRect(&ARec, -cxGridCellTextOffset, -cxGridCellTextOffset);
  ACanvas->DrawTexT(ATextToDraw, ARec, 0, true);

  ADone = FCustomDrawingStyle[(int)vtDetail][(int)cdaColumnHeader] != cdsDefaultDrawing;
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawTableViewDemoMainForm::tvOrdersCustomDrawFooterCell(
      TcxGridTableView *Sender, TcxCanvas *ACanvas,
      TcxGridColumnHeaderViewInfo *AViewInfo, bool &ADone)
{
  String ATextToDraw = AViewInfo->Text;
  TRect ARec = AViewInfo->Bounds;
  ACanvas->Canvas->Font->Assign(FFonts[(int)vtDetail][(int)cdaFooterCell]);
  switch (FCustomDrawingStyle[(int)vtDetail][(int)cdaFooterCell]) {
    case cdsBkImage: {
      ACanvas->Brush->Bitmap = NULL;
      ACanvas->Brush->Assign(FGridBrushDetailFooterCell);
      ACanvas->Canvas->FillRect(ARec);
    }; break;
    case cdsGradient: {
      bool AIsVertical = (FColorScheme[(int)vtDetail][(int)cdaFooterCell] == csGrey) ||
        (FColorScheme[(int)vtDetail][(int)cdaFooterCell] == csGold);
      DrawGradient(ACanvas->Canvas, ARec, ColorScheme[(int)FColorScheme[(int)vtDetail][(int)cdaFooterCell]][1],
        ColorScheme[(int)FColorScheme[(int)vtDetail][(int)cdaFooterCell]][0], 40, AIsVertical);
    }; break;
  };

  SetBkMode(ACanvas->Canvas->Handle, TRANSPARENT);
  InflateRect(&ARec, -cxGridCellTextOffset, -cxGridCellTextOffset);
  ACanvas->DrawTexT(ATextToDraw, ARec, 0, true);

  ADone = FCustomDrawingStyle[(int)vtDetail][(int)cdaFooterCell] != cdsDefaultDrawing;
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawTableViewDemoMainForm::tvOrdersCustomDrawPartBackground(
      TcxCustomGridTableView *Sender, TcxCanvas *ACanvas,
      TcxCustomGridCellViewInfo *AViewInfo, bool &ADone)
{
  TRect ARec = AViewInfo->Bounds;
  ACanvas->Canvas->Font->Assign(FFonts[(int)vtDetail][(int)cdaPartBackGround]);
  switch (FCustomDrawingStyle[(int)vtDetail][(int)cdaPartBackGround]) {
    case cdsBkImage: {
      ACanvas->Brush->Bitmap = NULL;
      ACanvas->Brush->Assign(FGridBrushDetailPartBackground);
      ACanvas->Canvas->FillRect(ARec);
    }; break;
    case cdsGradient: {
      bool AIsVertical = (FColorScheme[(int)vtDetail][(int)cdaPartBackGround] == csGrey) ||
        (FColorScheme[(int)vtDetail][(int)cdaPartBackGround] == csGold);
      DrawGradient(ACanvas->Canvas, ARec, ColorScheme[(int)FColorScheme[(int)vtDetail][(int)cdaPartBackGround]][1],
        ColorScheme[(int)FColorScheme[(int)vtDetail][(int)cdaPartBackGround]][0], 40, AIsVertical);
    }; break;
  };

  ADone = FCustomDrawingStyle[(int)vtDetail][(int)cdaPartBackGround] != cdsDefaultDrawing;
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawTableViewDemoMainForm::FormCreate(
      TObject *Sender)
{
  InitFonts();

  FSkyBitmap = new Graphics::TBitmap();

  LoadImageFromRes(FSkyBitmap, "SKY");

  FEgyptBitmap = new Graphics::TBitmap();
  LoadImageFromRes(FEgyptBitmap, "EGYPT");

  FMyFaceBitmap = new Graphics::TBitmap();
  LoadImageFromRes(FMyFaceBitmap, "MYFACE");

  FTileBitmap = new Graphics::TBitmap();
  LoadImageFromRes(FTileBitmap, "TILE");

  FBitMap = new Graphics::TBitmap();
  SetCustomDrawingStyles();
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawTableViewDemoMainForm::FormDestroy(
      TObject *Sender)
{
  for(int i=0; i < 2; i++)
    for(int j=0; j < 6; j++)
      delete FFonts[i][j];
  delete FBitMap;
  delete FEgyptBitmap;
  delete FMyFaceBitmap;
  delete FTileBitmap;
  delete FSkyBitmap;

  delete FGridBrushMasterCell;
  delete FGridBrushMasterFooterCell;
  delete FGridBrushMasterColumnHeader;
  delete FGridBrushMasterPartBackground;
  delete FGridBrushMasterIndicatorCell;

  delete FGridBrushDetailCell;
  delete FGridBrushDetailFooterCell;
  delete FGridBrushDetailColumnHeader;
  delete FGridBrushDetailPartBackground;
  delete FGridBrushDetailIndicatorCell;
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawTableViewDemoMainForm::tvOrdersCustomDrawIndicatorCell(
      TcxGridTableView *Sender, TcxCanvas *ACanvas,
      TcxCustomGridIndicatorItemViewInfo *AViewInfo, bool &ADone)
{
  TRect ARec;
  TcxIndicatorKind AIndicatorKind;

  ARec = AViewInfo->Bounds;
  ACanvas->Canvas->Font->Assign(FFonts[(int)vtDetail][(int)cdaIndicatorCell]);

  if (!dynamic_cast<TcxGridIndicatorRowItemViewInfo*>(AViewInfo))
    AIndicatorKind = ikNone;
  else
    AIndicatorKind = ((TcxGridIndicatorRowItemViewInfo*)AViewInfo)->IndicatorKind;

  switch (FCustomDrawingStyle[(int)vtDetail][(int)cdaIndicatorCell]) {
    case cdsBkImage: {
      ACanvas->Brush->Bitmap = NULL;
      ACanvas->Brush->Assign(FGridBrushMasterIndicatorCell);
      ACanvas->FillRect(ARec, NULL, False);
    }; break;
    case cdsGradient:  {
      bool AIsVertical = (FColorScheme[(int)vtDetail][(int)cdaIndicatorCell] == csGrey) || (FColorScheme[(int)vtDetail][(int)cdaIndicatorCell] == csGold);
      DrawGradient(ACanvas->Canvas, ARec, ColorScheme[(int)FColorScheme[(int)vtDetail][(int)cdaGroupCell]][1], ColorScheme[(int)FColorScheme[(int)vtDetail][(int)cdaIndicatorCell]][0], 40, AIsVertical);
    }; break;
  }
  if (AIndicatorKind != ikNone) {
	int X = (int)((ARec.Left + ARec.Right - imIndicatorImages->Width) / 2);
	int Y = (int)((ARec.Top + ARec.Bottom - imIndicatorImages->Height) / 2);
    imIndicatorImages->Draw(ACanvas->Canvas, X, Y, IndicatorImageIndex[vtDetail], true);
  };

  ADone = !(TCustomDrawingStyles() << cdsDefaultDrawing).Contains(FCustomDrawingStyle[(int)vtDetail][(int)cdaIndicatorCell]);
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawTableViewDemoMainForm::tvCarsCustomDrawIndicatorCell(
      TcxGridTableView *Sender, TcxCanvas *ACanvas,
      TcxCustomGridIndicatorItemViewInfo *AViewInfo, bool &ADone)
{
  TRect ARec;
  TcxIndicatorKind AIndicatorKind;

  ARec = AViewInfo->Bounds;
  ACanvas->Canvas->Font->Assign(FFonts[(int)vtMaster][(int)cdaIndicatorCell]);

  if (!dynamic_cast<TcxGridIndicatorRowItemViewInfo*>(AViewInfo))
    AIndicatorKind = ikNone;
  else
    AIndicatorKind = ((TcxGridIndicatorRowItemViewInfo*)AViewInfo)->IndicatorKind;

  switch (FCustomDrawingStyle[(int)vtMaster][(int)cdaIndicatorCell]) {
    case cdsBkImage: {
      ACanvas->Brush->Bitmap = NULL;
      ACanvas->Brush->Assign(FGridBrushMasterIndicatorCell);
      ACanvas->FillRect(ARec, NULL, False);
    }; break;
    case cdsGradient:  {
      bool AIsVertical = (FColorScheme[(int)vtMaster][(int)cdaIndicatorCell] == csGrey) || (FColorScheme[(int)vtMaster][(int)cdaIndicatorCell] == csGold);
      DrawGradient(ACanvas->Canvas, ARec, ColorScheme[(int)FColorScheme[(int)vtMaster][(int)cdaGroupCell]][1], ColorScheme[(int)FColorScheme[(int)vtMaster][(int)cdaIndicatorCell]][0], 40, AIsVertical);
    }; break;
  }
  if (AIndicatorKind != ikNone) {
	int X = (int)((ARec.Left + ARec.Right - imIndicatorImages->Width) / 2);
	int Y = (int)((ARec.Top + ARec.Bottom - imIndicatorImages->Height) / 2);
    imIndicatorImages->Draw(ACanvas->Canvas, X, Y, IndicatorImageIndex[vtMaster], true);
  };

  ADone = !(TCustomDrawingStyles() << cdsDefaultDrawing).Contains(FCustomDrawingStyle[(int)vtMaster][(int)cdaIndicatorCell]);
}
//---------------------------------------------------------------------------

