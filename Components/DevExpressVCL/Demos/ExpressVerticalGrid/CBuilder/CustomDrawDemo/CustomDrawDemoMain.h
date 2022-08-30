//---------------------------------------------------------------------------

#ifndef CustomDrawDemoMainH
#define CustomDrawDemoMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxLookAndFeels.hpp"
#include "DemoBasicMain.h"
#include <ActnList.hpp>
#include <ComCtrls.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
#include "cxControls.hpp"
#include "cxDBVGrid.hpp"
#include "cxEdit.hpp"
#include "cxEditRepositoryItems.hpp"
#include "cxGraphics.hpp"
#include "cxInplaceContainer.hpp"
#include "cxStyles.hpp"
#include "cxVGrid.hpp"
#include "CustomDrawDemoUtils.h"
#include "cxVGridViewInfo.hpp"
//---------------------------------------------------------------------------
class TcxItemCustomDrawInfo;

class TcxCustomDrawInfo {
private:
  TControl* FControl;
  TList *FBitmaps;
  TFont *FDefaultFont;
  TList *FCustomDrawData;
  Graphics::TBitmap* GetBkBitmap(TBkImage ABkImage);
  int GetCount(void);
  TcxItemCustomDrawInfo* GetItem(TCustomDrawArea ADrawArea);
  void LoadResourceBitmaps();
public:
  __fastcall TcxCustomDrawInfo(TControl* AControl);
  __fastcall ~TcxCustomDrawInfo();
  void AddNewItem(TCustomDrawArea ADrawArea, TcxItemCustomDrawType AItemType);
  TcxItemCustomDrawInfo* GetItemByIndex(int AIndex);
  __property Graphics::TBitmap* Bitmaps[TBkImage ABkImage] = {read=GetBkBitmap};
  __property int Count = {read=GetCount};
  __property TFont* DefaultFont = {read=FDefaultFont};
  __property TcxItemCustomDrawInfo* Items[TCustomDrawArea ADrawArea] = {read = GetItem};
  __property TControl* Control = {read=FControl};
};

class TcxItemCustomDrawInfo {
private:
  TcxCustomDrawInfo *FOwner;
  Graphics::TBitmap *FBitmap;
  TBkImage FBkImageType;
  TCustomDrawArea FDrawArea;
  TCustomDrawingStyle FDrawingStyle;
  TcxColorScheme FColorScheme;
  TFont *FFont;
  bool FIsBitmapAssigned;
  bool FIsFontAssigned;
  bool FOwnerTextDraw;
  TcxItemCustomDrawType FItemType;
  Graphics::TBitmap* GetBitmap();
  TFont* GetFont();
  void SetBitmap(Graphics::TBitmap *Value);
  void SetFont(TFont *Value);
public:
  __fastcall TcxItemCustomDrawInfo(TcxCustomDrawInfo *AOwner, TCustomDrawArea ADrawArea,
    TcxItemCustomDrawType AItemType);
  __fastcall ~TcxItemCustomDrawInfo();
  __property TcxCustomDrawInfo *Owner = {read=FOwner};
  __property Graphics::TBitmap *Bitmap = {read=GetBitmap, write=SetBitmap};
  __property TBkImage BkImageType = {read=FBkImageType, write=FBkImageType};
  __property TCustomDrawArea DrawArea = {read=FDrawArea};
  __property TCustomDrawingStyle DrawingStyle = {read=FDrawingStyle, write=FDrawingStyle};
  __property TcxColorScheme ColorScheme = {read=FColorScheme, write=FColorScheme};
  __property TFont *Font = {read=GetFont, write=SetFont};
  __property TcxItemCustomDrawType ItemType = {read=FItemType};
  __property bool OwnerTextDraw = {read=FOwnerTextDraw, write=FOwnerTextDraw};
};

class TCustomDrawDemoMainForm : public TDemoBasicMainForm
{
__published:	// IDE-managed Components
  TImageList *ImageList;
  TcxEditRepository *cxEditRepository1;
  TcxEditRepositoryMaskItem *eriTelephoneMaskEdit;
  TcxDBVerticalGrid *cxDBVerticalGrid;
  TcxCategoryRow *cxDBVerticalGridOrderInfo;
  TcxDBEditorRow *cxDBVerticalGridPurchaseDate;
  TcxDBEditorRow *cxDBVerticalGridTime;
  TcxDBEditorRow *cxDBVerticalGridPaymentType;
  TcxDBEditorRow *cxDBVerticalGridPaymentAmount;
  TcxDBEditorRow *cxDBVerticalGridQuantity;
  TcxCategoryRow *cxDBVerticalGridCustomerInfo;
  TcxCategoryRow *cxDBVerticalGridCommonCustomerInfo;
  TcxDBEditorRow *cxDBVerticalGridFirstName;
  TcxDBEditorRow *cxDBVerticalGridLastName;
  TcxDBEditorRow *cxDBVerticalGridSpouse;
  TcxDBEditorRow *cxDBVerticalGridPrefix;
  TcxDBEditorRow *cxDBVerticalGridTitle;
  TcxCategoryRow *cxDBVerticalGridCustomerContacts;
  TcxCategoryRow *cxDBVerticalGridPhonesAndFaxes;
  TcxDBEditorRow *cxDBVerticalGridFaxPhone;
  TcxDBEditorRow *cxDBVerticalGridHomePhone;
  TcxDBEditorRow *cxDBVerticalGridState;
  TcxCategoryRow *cxDBVerticalGridCategoryAddress;
  TcxDBEditorRow *cxDBVerticalGridCity;
  TcxDBEditorRow *cxDBVerticalGridZipCode;
  TcxDBEditorRow *cxDBVerticalGridAddress;
  TcxDBEditorRow *cxDBVerticalGridEmail;
  TcxDBEditorRow *cxDBVerticalGridOccupation;
  TcxDBEditorRow *cxDBVerticalGridCustomer;
  TcxDBEditorRow *cxDBVerticalGridCompany;
  TcxCategoryRow *cxDBVerticalGridCarInfo;
  TcxCategoryRow *cxDBVerticalGridCar;
  TcxDBEditorRow *cxDBVerticalGridTrademark;
  TcxDBEditorRow *cxDBVerticalGridModel;
  TcxCategoryRow *cxDBVerticalGridMPG;
  TcxDBEditorRow *cxDBVerticalGridMPG_City;
  TcxDBEditorRow *cxDBVerticalGridMPG_Highway;
  TcxCategoryRow *cxDBVerticalGridEngine;
  TcxDBEditorRow *cxDBVerticalGridHP;
  TcxDBEditorRow *cxDBVerticalGridLiter;
  TcxDBEditorRow *cxDBVerticalGridCyl;
  TcxCategoryRow *cxDBVerticalGridNotes;
  TcxDBEditorRow *cxDBVerticalGridCars_Description;
  TcxCategoryRow *cxDBVerticalGridTransmission;
  TcxDBEditorRow *cxDBVerticalGridTransmissSpeedCount;
  TcxDBEditorRow *cxDBVerticalGridTransmissAutomatic;
  TcxCategoryRow *cxDBVerticalGridOthers;
  TcxDBEditorRow *cxDBVerticalGridCategory;
  TcxDBEditorRow *cxDBVerticalGridHyperlink;
  TcxDBEditorRow *cxDBVerticalGridPrice;
  TcxDBEditorRow *cxDBVerticalGridPicture;
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall FormShow(TObject *Sender);
  void __fastcall cxDBVerticalGridDrawBackground(TObject *Sender,
          TcxCanvas *ACanvas, const TRect &R,
          const TcxViewParams &AViewParams, bool &Done);
  void __fastcall cxDBVerticalGridDrawRowHeader(TObject *Sender,
          TcxCanvas *ACanvas, TcxvgPainter *APainter,
          TcxCustomRowHeaderInfo *AHeaderViewInfo, bool &Done);
  void __fastcall cxDBVerticalGridDrawValue(TObject *Sender,
          TcxCanvas *ACanvas, TcxvgPainter *APainter,
          TcxRowValueInfo *AValueInfo, bool &Done);
  void __fastcall LayoutStyleClick(TObject *Sender);
  void __fastcall actCustomizationFormExecute(TObject *Sender);
private:	// User declarations
  TcxCustomDrawInfo* FCustomDrawInfo;
  void __fastcall AddCustomDrawInfos();
  void __fastcall AdjustCustomDrawItems();
  bool __fastcall DrawCellItem(TcxItemCustomDrawInfo* AItem, TcxCanvas* ACanvas,
      TcxRowValueInfo* AValueInfo, TcxvgPainter* APainter, TObject* Sender);
  bool __fastcall DrawHeaderItem(TcxItemCustomDrawInfo* AItem, TcxCanvas* ACanvas,
      TcxCustomRowHeaderInfo* AHeaderViewInfo, TcxvgPainter* APainter, TObject* Sender);
  bool __fastcall DrawItem(TcxItemCustomDrawInfo *AItem, TcxCanvas *ACanvas,
    const TRect &R, bool AHorizontal);
  bool DrawCategoryItem(TcxItemCustomDrawInfo* AItem, TcxCanvas* ACanvas,
      TcxCategoryRowHeaderInfo* ACategoryViewInfo, TcxvgPainter* APainter, TObject* Sender);
  bool  __fastcall DrawBackgroundItem(TcxItemCustomDrawInfo* AItem,TcxCanvas* ACanvas,
    const TRect R, TcxViewParams AViewParams, TObject* Sender);
  void __fastcall DrawIndents(TcxCustomRowHeaderInfo* AHeaderViewInfo,
    TCustomDrawArea ACustomDrawArea, TcxCanvas* ACanvas, TcxViewParams AViewParams,
    TcxvgPainter* APainter, TIndentInfoList* AIndentInfoList);
  void __fastcall DrawCellsLines(TcxCanvas* ACanvas, TColor AColor, TRect ARect);
  void __fastcall DrawDefaultLines(TcxCanvas* ACanvas, TColor AColor,
      TcxCustomRowHeaderInfo* AHeaderViewInfo, TRect ARect, TLineInfos ALineInfos);
  void __fastcall DrawRightLine(TcxCanvas* ACanvas, TColor AColor,
      TRect ARect);
  TLineInfos __fastcall GetAdditionalLines(TcxDBVerticalGrid* Sender, TcxCustomRowHeaderInfo* AHeaderViewInfo);
  bool __fastcall OwnerDrawText(TcxCanvas* ACanvas, TcxRowValueInfo* AValueInfo,
    TColor ALineColor, TFont *AFont);
public:		// User declarations
  __fastcall TCustomDrawDemoMainForm(TComponent* Owner);
  __property TcxCustomDrawInfo *CustomDrawInfo = {read=FCustomDrawInfo};
};

class TControlAccess : public TControl {
public:
__property Font;
};

//---------------------------------------------------------------------------
extern PACKAGE TCustomDrawDemoMainForm *CustomDrawDemoMainForm;
//---------------------------------------------------------------------------
#endif
