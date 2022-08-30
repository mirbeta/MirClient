//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "CustomDrawDemoMain.h"
#include "CustomDrawDemoData.h"
#include "CustomDrawDemoTypes.h"
#include "CustomDrawDemoEditor.h"
#include "cxLookAndFeelPainters.hpp"
#include "cxCustomData.hpp"

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "dxCore"
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
#pragma link "cxCheckBox"
#pragma link "cxCurrencyEdit"
#pragma link "cxDBLookupComboBox"
#pragma link "cxDBTL"
#pragma link "cxEditRepositoryItems"
#pragma link "cxInplaceContainer"
#pragma link "cxMaskEdit"
#pragma link "cxMemo"
#pragma link "cxTL"
#pragma link "cxTLData"
#pragma link "cxTextEdit"
#pragma resource "*.dfm"
TCustomDrawDemoMainForm *CustomDrawDemoMainForm;

/* TcxCustomDrawInfo implementation*/
__fastcall TcxCustomDrawInfo::TcxCustomDrawInfo(void)
{
  FBitmaps = new TList();
  LoadResourceBitmaps();
  FDefaultFont = new TFont();
  FCustomDrawData = new TList();
}

void TcxCustomDrawInfo::LoadResourceBitmaps()
{
  Graphics::TBitmap *ABitmap;
  for (int I = 0; I < BkImageCount; I++){
    ABitmap = new Graphics::TBitmap();
    LoadImageFromRes(ABitmap, BkImageResNames[I]);
    FBitmaps->Add(ABitmap);
  }
}

//---------------------------------------------------------------------------

__fastcall TcxCustomDrawInfo::~TcxCustomDrawInfo(void)
{
  for (int I = 0; I < FCustomDrawData->Count; I++)
    delete (TcxItemCustomDrawInfo*)FCustomDrawData->Items[I];
  for (int I = 0; I < FBitmaps->Count; I++)
    delete (Graphics::TBitmap*)FBitmaps->Items[I];
  delete FCustomDrawData;
  delete FBitmaps;
  delete FDefaultFont;
}
//---------------------------------------------------------------------------

void TcxCustomDrawInfo::AddNewItem(TCustomDrawArea ADrawArea,
  TcxItemCustomDrawType AItemType)
{
  FCustomDrawData->Add(new TcxItemCustomDrawInfo(this, ADrawArea, AItemType));
}
//---------------------------------------------------------------------------

TcxItemCustomDrawInfo* TcxCustomDrawInfo::GetItemByIndex(int AIndex)
{
  return ((TcxItemCustomDrawInfo*)FCustomDrawData->Items[AIndex]);
}
//---------------------------------------------------------------------------

Graphics::TBitmap* TcxCustomDrawInfo::GetBkBitmap(TBkImage ABkImage)
{
  return (Graphics::TBitmap*)FBitmaps->Items[int(ABkImage)];
}
//---------------------------------------------------------------------------

int TcxCustomDrawInfo::GetCount(void)
{
  return FCustomDrawData->Count;
}
//---------------------------------------------------------------------------

TcxItemCustomDrawInfo* TcxCustomDrawInfo::GetItem(TCustomDrawArea ADrawArea)
{
  TcxItemCustomDrawInfo *Result = NULL;
  for (int I = 0; I < FCustomDrawData->Count; I++)
    if (((TcxItemCustomDrawInfo*)FCustomDrawData->Items[I])->DrawArea == ADrawArea){
      Result = ((TcxItemCustomDrawInfo*)FCustomDrawData->Items[I]);
      break;
    }
  return Result;
}
//---------------------------------------------------------------------------

/*TcxItemCustomDrawInfo implementation*/

__fastcall TcxItemCustomDrawInfo::TcxItemCustomDrawInfo(TcxCustomDrawInfo *AOwner,
  TCustomDrawArea ADrawArea, TcxItemCustomDrawType AItemType)
{
  FOwner = AOwner;
  FDrawArea = ADrawArea;
  FItemType = AItemType;
  if (FOwner != NULL)
    FBitmap = AOwner->Bitmaps[TBkImage(0)];
  else
    FBitmap = NULL;
  if (FOwner != NULL)
    FFont = AOwner->DefaultFont;
  else
    FBitmap = NULL;
  FBkImageType = (TBkImage)0;
  FDrawingStyle = (TCustomDrawingStyle)0;
  FColorScheme = (TcxColorScheme)0;
  FIsBitmapAssigned = false;
  FIsFontAssigned = false;
}
//---------------------------------------------------------------------------

__fastcall TcxItemCustomDrawInfo::~TcxItemCustomDrawInfo()
{
  if (FIsBitmapAssigned)
    delete FBitmap;
  if (FIsFontAssigned)
    delete FFont;
}

//---------------------------------------------------------------------------

Graphics::TBitmap* TcxItemCustomDrawInfo::GetBitmap()
{
  if ((FBkImageType != bkiUserDefined || !FIsBitmapAssigned) &&
    (FOwner != NULL))
    return FOwner->Bitmaps[FBkImageType];
  else
    return FBitmap;
}
//---------------------------------------------------------------------------

TFont* TcxItemCustomDrawInfo::GetFont()
{
  return FFont;
}
//---------------------------------------------------------------------------

void TcxItemCustomDrawInfo::SetBitmap(Graphics::TBitmap *Value)
{
  if (FIsBitmapAssigned)
    delete FBitmap;
  FBitmap = Value;
  FIsBitmapAssigned = true;
  FBkImageType = bkiUserDefined;
}
//---------------------------------------------------------------------------

void TcxItemCustomDrawInfo::SetFont(TFont *Value)
{
  if (FIsFontAssigned)
    delete FFont;
  FFont = Value;
  FIsFontAssigned = true;
}
//---------------------------------------------------------------------------

/*TCustomDrawDemoMainForm implementation*/

__fastcall TCustomDrawDemoMainForm::TCustomDrawDemoMainForm(TComponent* Owner)
  : TDemoBasicMainForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawDemoMainForm::FormShow(TObject *Sender)
{
  cxDBTreeList->FullExpand();
  CustomDrawDemoEditorForm->Show();

/* remove/add the closing slash on this line to disable/enable the following code *

  ShowMessage("WARNING: tutorial not completed. First, please apply the steps "
    "shown in the doc file");

//*/

}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawDemoMainForm::FormCreate(TObject *Sender)
{
  TDemoBasicMainForm::FormCreate(Sender);
  FCustomDrawInfo = new TcxCustomDrawInfo;
  AddCustomDrawInfos();
  AdjustCustomDrawItems();
}

bool __fastcall TCustomDrawDemoMainForm::IsNativeDefaultStyle()
{
  return(false);
}

void TCustomDrawDemoMainForm::AddCustomDrawInfos()
{
  FCustomDrawInfo->AddNewItem(cdaBackground, itNormal);
  FCustomDrawInfo->AddNewItem(cdaBandHeader, itText);
  FCustomDrawInfo->AddNewItem(cdaCell, itCell);
  FCustomDrawInfo->AddNewItem(cdaCellsGroup, itNormal);
  FCustomDrawInfo->AddNewItem(cdaFooter, itNormal);
  FCustomDrawInfo->AddNewItem(cdaFooterCell, itText);
  FCustomDrawInfo->AddNewItem(cdaGroupFooter, itNormal);
  FCustomDrawInfo->AddNewItem(cdaHeader, itNormal);
  FCustomDrawInfo->AddNewItem(cdaHeaderCell, itText);
  FCustomDrawInfo->AddNewItem(cdaIndentCell, itNormal);
  FCustomDrawInfo->AddNewItem(cdaIndicatorCell, itNormal);
  FCustomDrawInfo->AddNewItem(cdaPreview, itCell);
}

void TCustomDrawDemoMainForm::AdjustCustomDrawItems()
{
  FCustomDrawInfo->Items[cdaHeaderCell]->DrawingStyle = cdsBkImage;
  FCustomDrawInfo->Items[cdaBackground]->BkImageType = bkiEgypt;
  FCustomDrawInfo->Items[cdaIndentCell]->BkImageType = bkiEgypt;
  FCustomDrawInfo->Items[cdaFooter]->BkImageType = bkiMyFace;
  FCustomDrawInfo->Items[cdaFooterCell]->BkImageType = bkiMyFace;
  FCustomDrawInfo->Items[cdaPreview]->BkImageType = bkiMyFace;
  FCustomDrawInfo->Items[cdaHeaderCell]->DrawingStyle = cdsGradient;
  FCustomDrawInfo->Items[cdaBandHeader]->DrawingStyle = cdsGradient;
  FCustomDrawInfo->Items[cdaBandHeader]->ColorScheme = csBlue;
  FCustomDrawInfo->Items[cdaIndicatorCell]->DrawingStyle = cdsGradient;
  FCustomDrawInfo->Items[cdaGroupFooter]->BkImageType = bkiMyFace;
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawDemoMainForm::FormDestroy(TObject *Sender)
{
  delete FCustomDrawInfo;
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawDemoMainForm::actCustomizationFormExecute(
  TObject *Sender)
{
  CustomDrawDemoEditorForm->Show();
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawDemoMainForm::cxDBTreeListCustomDrawBackground(
  TcxCustomTreeList *Sender, TcxCanvas *ACanvas, TcxTreeListCustomCellViewInfo *AViewInfo, bool &ADone)
{
/* remove/add the closing slash on this line to disable/enable the following code*/

  ADone = DrawItem(FCustomDrawInfo->Items[cdaBackground], ACanvas, AViewInfo->BoundsRect);

//*/
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawDemoMainForm::cxDBTreeListCustomDrawBandHeader(
  TcxCustomTreeList *Sender, TcxCanvas *ACanvas, TcxTreeListBandCellViewInfo *AViewInfo,
  bool &ADone)
{
/* remove/add the closing slash on this line to disable/enable the following code*/

TCustomDrawArea ADrawArea[4] = {cdaHeader, cdaCellsGroup, cdaGroupFooter, cdaFooter};
ADone = DrawItem(FCustomDrawInfo->Items[ADrawArea[AViewInfo->Part]], ACanvas, AViewInfo->BoundsRect);

//*/
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawDemoMainForm::cxDBTreeListCustomDrawFooterCell(
  TcxCustomTreeList *Sender, TcxCanvas *ACanvas, TcxTreeListFooterCellViewInfo *AViewInfo,
  bool &ADone)
{
/* remove/add the closing slash on this line to disable/enable the following code*/

  if (AViewInfo->Hidden) return;
  
  TcxItemCustomDrawInfo *AItem = FCustomDrawInfo->Items[cdaFooterCell];
  if (AItem->DrawingStyle == cdsDefaultDrawing)  return;
  ADone = DrawItem(AItem, ACanvas, AViewInfo->BoundsRect);
  ACanvas->Font = AItem->Font;
  ACanvas->Brush->Style = bsClear;
  ACanvas->DrawTexT(AViewInfo->Text, cxRectInflate(AViewInfo->BoundsRect, -2, -2), 0, true);

//*/

}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawDemoMainForm::cxDBTreeListCustomDrawHeaderCell(
  TcxCustomTreeList *Sender, TcxCanvas *ACanvas, TcxTreeListHeaderCellViewInfo *AViewInfo,
  bool &ADone)
{
/* remove/add the closing slash on this line to disable/enable the following code*/

  ADone = DrawHeaderItem(FCustomDrawInfo->Items[cdaHeaderCell], ACanvas, AViewInfo, Sender);

//*/
}
//---------------------------------------------------------------------------

void TCustomDrawDemoMainForm::DrawTreeLines(TcxTreeListIndentCellViewInfo *AViewInfo,
  TcxCanvas *ACanvas)
{
  if (AViewInfo->Lines.Contains(ilVertUp) ||
    AViewInfo->Lines.Contains(ilVertDown))
    cxFillHalfToneRect(ACanvas->Canvas, AViewInfo->VertTreeLine,
      AViewInfo->ViewParams.Color, cxDBTreeList->OptionsView->TreeLineColor);
  if (AViewInfo->Lines.Contains(ilHorz))
    cxFillHalfToneRect(ACanvas->Canvas, AViewInfo->HorzTreeLine,
      AViewInfo->ViewParams.Color, cxDBTreeList->OptionsView->TreeLineColor);
}

void TCustomDrawDemoMainForm::DrawRectWithBorders(TcxCanvas *ACanvas,
  TcxBorders ABorders, TColor AColor, const TRect &ARect)
{
  ACanvas->Brush->Color = AColor;
  if (ABorders.Contains(bLeft))
    ACanvas->FillRect(cxRect(ARect.Left, ARect.Top, ARect.Left + 1, ARect.Bottom), NULL, False);
  if (ABorders.Contains(bRight))
    ACanvas->FillRect(cxRect(ARect.Right - 1, ARect.Top, ARect.Right, ARect.Bottom), NULL, False);
  if (ABorders.Contains(bTop))
    ACanvas->FillRect(cxRect(ARect.Left, ARect.Top, ARect.Right, ARect.Top + 1), NULL, False);
  if (ABorders.Contains(bBottom))
    ACanvas->FillRect(cxRect(ARect.Left, ARect.Bottom - 1, ARect.Right, ARect.Bottom), NULL, False);
}

void __fastcall TCustomDrawDemoMainForm::cxDBTreeListCustomDrawIndentCell(
  TcxCustomTreeList *Sender, TcxCanvas *ACanvas, TcxTreeListIndentCellViewInfo *AViewInfo,
  bool &ADone)
{
/* remove/add the closing slash on this line to disable/enable the following code*/

  ADone = DrawItem(FCustomDrawInfo->Items[cdaIndentCell], ACanvas, AViewInfo->BoundsRect);
  if (!ADone)  return;
  DrawRectWithBorders(ACanvas, AViewInfo->Borders,
    ((TcxDBTreeList*)Sender)->OptionsView->GridLineColor, AViewInfo->BoundsRect);
  DrawTreeLines(AViewInfo, ACanvas);

  if (AViewInfo->Button){
    TRect ARect = AViewInfo->GlyphRect;
    DrawItem(FCustomDrawInfo->Items[cdaIndentCell], ACanvas, ARect);
    ACanvas->FrameRect(ARect, clBtnText, 1, TcxBorders()<<bLeft<<bTop<<bRight<<bBottom, False);
    int ASize = ARect.Right - ARect.Left - 2 * 2;
    int X = (ARect.Left + ARect.Right) >> 1;
    int Y = (ARect.Top + ARect.Bottom) >> 1;
    ACanvas->Brush->Color = clBtnText;
    ACanvas->FillRect(Rect(X - (ASize >> 1), Y, X + (ASize >> 1) + 1, Y + 1), NULL, False);
    if (!AViewInfo->IsExpanded)
      ACanvas->FillRect(Rect(X, Y - (ASize >> 1), X + 1, Y + (ASize >> 1) + 1), NULL, False);
  }

//*/
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawDemoMainForm::cxDBTreeListCustomDrawIndicatorCell(
  TcxCustomTreeList *Sender, TcxCanvas *ACanvas, TcxTreeListIndicatorCellViewInfo *AViewInfo,
  bool &ADone)
{
/* remove/add the closing slash on this line to disable/enable the following code*/
  TcxItemCustomDrawInfo* ATempCustomDrawItem = FCustomDrawInfo->Items[cdaIndicatorCell];
  if (ATempCustomDrawItem->DrawingStyle == cdsDefaultDrawing)
    return;

  ADone = DrawIndicatorItem(ATempCustomDrawItem, ACanvas, AViewInfo->BoundsRect);
  if (AViewInfo->Kind == ikNone) return;
  TRect ARect = AViewInfo->BoundsRect;
  int X = (ARect.Left + ARect.Right - cxIndicatorImages()->Width) >> 1;
  int Y = (ARect.Top + ARect.Bottom - cxIndicatorImages()->Height) >> 1;
  cxIndicatorImages()->Draw(ACanvas->Canvas, X, Y, (int)AViewInfo->Kind - 1, true);

//*/
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawDemoMainForm::cxDBTreeListCustomDrawPreview(
  TcxCustomTreeList *Sender, TcxCanvas *ACanvas, TcxTreeListEditCellViewInfo *AViewInfo,
  bool &ADone)
{
/* remove/add the closing slash on this line to disable/enable the following code*/

  ADone = DrawCellItem(FCustomDrawInfo->Items[cdaPreview], ACanvas, AViewInfo, Sender);

//*/
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawDemoMainForm::cxDBTreeListCustomDrawCell(
  TcxCustomTreeList *Sender, TcxCanvas *ACanvas, TcxTreeListEditCellViewInfo *AViewInfo,
  bool &ADone)
{
/* remove/add the closing slash on this line to disable/enable the following code*/

  ADone = DrawCellItem(FCustomDrawInfo->Items[cdaCell], ACanvas, AViewInfo, Sender);

//*/
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawDemoMainForm::cxDBTreeListCustomDrawHeader(
  TcxCustomTreeList *Sender, TcxCanvas *ACanvas, TcxTreeListHeaderCellViewInfo *AViewInfo,
  bool &ADone)
{
/* remove/add the closing slash on this line to disable/enable the following code*/

  ADone = DrawHeaderItem(FCustomDrawInfo->Items[cdaBandHeader], ACanvas, AViewInfo, Sender);

//*/
}
//---------------------------------------------------------------------------

void TCustomDrawDemoMainForm::OwnerDrawCell(TcxTreeListEditCellViewInfo *AViewInfo,
  TcxCanvas *ACanvas, TColor ALinesColor, TFont *AFont)
{
  ACanvas->Pen->Color = ALinesColor;
  ACanvas->Brush->Style = bsClear;
  ACanvas->Font = AFont;
  TRect ARect = cxRectInflate(AViewInfo->BoundsRect, 0, 1, 0, 0);
  ACanvas->Canvas->Rectangle(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
  if (dynamic_cast<TcxCustomTextEditViewInfo*>(AViewInfo->EditViewInfo) != 0)
    if (dynamic_cast<TcxCustomCheckBoxViewInfo*>(AViewInfo->EditViewInfo) != 0)
      ACanvas->DrawTexT(VarToStr(AViewInfo->DisplayValue),
        cxRectInflate(AViewInfo->BoundsRect, -2, -2), 0, true);
    else
      ACanvas->DrawTexT(
        ((TcxCustomTextEditViewInfo*)AViewInfo->EditViewInfo)->Text,
        cxRectInflate(AViewInfo->BoundsRect, -2, -2), 0, true);

}

bool TCustomDrawDemoMainForm::DrawCellItem(TcxItemCustomDrawInfo *AItem,
  TcxCanvas *ACanvas, TcxTreeListEditCellViewInfo *AViewInfo, TObject *Sender)
{
/* remove/add the closing slash on this line to disable/enable the following code*/

  bool Result = false;
  AViewInfo->Transparent = (AViewInfo->ViewParams.Bitmap != NULL) &&
    (!AViewInfo->ViewParams.Bitmap->Empty);
  if (AViewInfo->Selected || AItem->DrawingStyle == cdsDefaultDrawing)
    return Result;
  if (AItem->DrawingStyle == cdsDependsOnData){
    TcxStyle *AStyle;
	if ((int)AViewInfo->Node->Values[cxDBTreeListVACANCY->ItemIndex])
      AStyle = CustomDrawDemoDataDM->styVacancy;
    else
	  AStyle = CustomDrawDemoDataDM->styNoVacancy;
    ACanvas->Brush->Color = AStyle->Color;
    ACanvas->FillRect(AViewInfo->BoundsRect, NULL, False);
    OwnerDrawCell(AViewInfo, ACanvas, AStyle->TextColor, AStyle->Font);
    Result = true;
  }
  else{
    Result = DrawItem(AItem, ACanvas, AViewInfo->BoundsRect);
    if (AItem->OwnerTextDraw)
      OwnerDrawCell(AViewInfo, ACanvas,
        ((TcxTreeList*)Sender)->OptionsView->GridLineColor, AItem->Font);
    else{
      AViewInfo->Transparent = true;
      Result = false;
    }
  }
  return Result;

//*/
}
//---------------------------------------------------------------------------

bool TCustomDrawDemoMainForm::DrawHeaderItem(TcxItemCustomDrawInfo *AItem,
  TcxCanvas *ACanvas, TcxTreeListHeaderCellViewInfo *AViewInfo, TObject *Sender)
{
/* remove/add the closing slash on this line to disable/enable the following code*/

  bool Result = false;
  if (AItem->DrawingStyle == cdsDefaultDrawing)
    return Result;
  Result = DrawIndicatorItem(AItem, ACanvas, AViewInfo->BoundsRect);
  ACanvas->Font = AItem->Font;
  ACanvas->Brush->Style = bsClear;
  ACanvas->DrawTexT(AViewInfo->Text, cxRectInflate(AViewInfo->TextBounds, -2, 0), cxAlignCenter, true);

  if (dynamic_cast<TcxTreeListHeaderCellViewInfo*>(AViewInfo) != 0 &&
	((TcxTreeListHeaderCellViewInfo*)AViewInfo)->SortOrder != Dxcore::soNone ){
	bool AAscendingSorting =
	  ((TcxTreeListHeaderCellViewInfo*)AViewInfo)->SortOrder == soAscending;
	int ASign = 2 * (Byte)AAscendingSorting - 1;
	int AWidth = 7;
	int AHeight = 8;
	TRect ARect = ((TcxTreeListHeaderCellViewInfo*)AViewInfo)->SortMarkBounds;
    int X = (ARect.Left + ARect.Right) >> 1;
    if (AWidth%2 == 0) X--;
    int Y;
    if (AAscendingSorting)
      Y = (ARect.Top + ARect.Bottom - AHeight) >> 1;
    else
      Y = ((ARect.Top + ARect.Bottom + AHeight) >> 1) - 1;

    int ALeftSide = AWidth >> 1;
    if (AWidth%2 == 0) ALeftSide--;
    int ARightSide = AWidth >> 1;
    ACanvas->Pen->Color = clBtnShadow;
    ACanvas->MoveTo(X + ARightSide, Y + ASign * (AHeight - 2));
    ACanvas->LineTo(X + ARightSide - ALeftSide, Y);
    ACanvas->LineTo(X + ARightSide, Y + ASign * (AHeight - 1));
    ACanvas->LineTo(X - ALeftSide, Y + ASign * (AHeight - 1));
    ACanvas->LineTo(X, Y);
    ACanvas->LineTo(X - ALeftSide, Y + ASign * (AHeight - (int)(AWidth%2!=0)));
  }
  if (AViewInfo->State == cxbsPressed)
	 ACanvas->InvertRect(AViewInfo->BoundsRect);
  return Result;

//*/
}
//---------------------------------------------------------------------------

bool TCustomDrawDemoMainForm::DrawItem(TcxItemCustomDrawInfo *AItem,
  TcxCanvas *ACanvas, const TRect &R)
{
/* remove/add the closing slash on this line to disable/enable the following code*/

  switch (AItem->DrawingStyle) {
    case cdsBkImage:
       ACanvas->FillRect(R, AItem->Bitmap, False);
       break;
    case cdsGradient:
       DrawGradient(ACanvas->Canvas, R,
        ColorScheme[(int)AItem->ColorScheme][1],
        ColorScheme[(int)AItem->ColorScheme][0], 40,
        (int)AItem->ColorScheme > 1);
       break;
  };
  return (AItem->DrawingStyle != cdsDefaultDrawing);

//*/
}
//---------------------------------------------------------------------------

bool TCustomDrawDemoMainForm::DrawIndicatorItem(TcxItemCustomDrawInfo *AItem, TcxCanvas *ACanvas,
  const TRect &R)
{
   bool Result = DrawItem(AItem, ACanvas, R);
   ACanvas->FrameRect(cxRectInflate(R, -1, -1), clBtnText, 1, TcxBorders()<<bLeft<<bTop<<bRight<<bBottom, False);
   return Result;
}
//---------------------------------------------------------------------------


void __fastcall TCustomDrawDemoMainForm::cxDBTreeListDragOver(
      TObject *Sender, TObject *Source, int X, int Y, TDragState State,
      bool &Accept)
{
 //       
}
//---------------------------------------------------------------------------

