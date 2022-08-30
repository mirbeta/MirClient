//---------------------------------------------------------------------------

#ifndef CustomDrawTableViewDemoMainH
#define CustomDrawTableViewDemoMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxClasses.hpp"
#include "cxControls.hpp"
#include "cxCustomData.hpp"
#include "cxData.hpp"
#include "cxDBData.hpp"
#include "cxEdit.hpp"
#include "cxFilter.hpp"
#include "cxGraphics.hpp"
#include "cxGrid.hpp"
#include "cxGridCustomTableView.hpp"
#include "cxGridCustomView.hpp"
#include "cxGridDBTableView.hpp"
#include "cxGridLevel.hpp"
#include "cxGridTableView.hpp"
#include "cxStyles.hpp"
#include <ActnList.hpp>
#include <ComCtrls.hpp>
#include <DB.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
#include <Dialogs.hpp>
#include "CustomDrawTableViewDemoTypes.h"
#include "cxLookAndFeels.hpp"
#include "BaseForm.h"
#include "cxBlobEdit.hpp"
#include "cxDataStorage.hpp"
#include "cxGridCardView.hpp"
#include "cxImageComboBox.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxTimeEdit.hpp"
#include "cxCurrencyEdit.hpp"
#include "CarsData.h"
#include "CarsDataForGrid.h"
//---------------------------------------------------------------------------
class TCustomDrawTableViewDemoMainForm : public TfmBaseForm
{
__published:  // IDE-managed Components
  TcxGrid *cxgCars;
  TcxGridDBTableView *tvCars;
  TcxGridDBColumn *tvCarsTrademark;
  TcxGridDBColumn *tvCarsModel;
  TcxGridDBColumn *tvCarsHP;
  TcxGridDBColumn *tvCarsTorque;
  TcxGridDBColumn *tvCarsCyl;
  TcxGridDBColumn *tvCarsTransmissSpeedCount;
  TcxGridDBColumn *tvCarsMPG_City;
  TcxGridDBColumn *tvCarsMPG_Highway;
  TcxGridDBColumn *tvCarsCategory;
  TcxGridDBColumn *tvCarsDescription;
  TcxGridDBColumn *tvCarsPicture;
  TcxGridDBColumn *tvCarsPrice;
  TcxGridDBTableView *tvOrders;
  TcxGridDBColumn *tvOrdersPurchaseDate;
  TcxGridDBColumn *tvOrdersTime;
  TcxGridDBColumn *tvOrdersPaymentType;
  TcxGridDBColumn *tvOrdersPaymentAmount;
  TcxGridDBColumn *tvOrdersQuantity;
  TcxGridLevel *lvCars;
  TcxGridLevel *lvOrders;
  TMenuItem *miOptions;
  TMenuItem *miCustomDrawStylesEditor;
  TOpenDialog *OpenDialog;
  TFontDialog *FontDialog;
  TImageList *imPaymentType;
  TImageList *imIndicatorImages;
  void __fastcall FormShow(TObject *Sender);
  void __fastcall miCustomDrawEditorClick(TObject *Sender);
  void __fastcall tvCarsCustomDrawCell(TcxCustomGridTableView *Sender,
          TcxCanvas *ACanvas, TcxGridTableDataCellViewInfo *AViewInfo,
          bool &ADone);
  void __fastcall tvCarsCustomDrawColumnHeader(TcxGridTableView *Sender,
          TcxCanvas *ACanvas, TcxGridColumnHeaderViewInfo *AViewInfo,
          bool &ADone);
  void __fastcall tvCarsCustomDrawFooterCell(TcxGridTableView *Sender,
          TcxCanvas *ACanvas, TcxGridColumnHeaderViewInfo *AViewInfo,
          bool &ADone);
  void __fastcall tvCarsCustomDrawPartBackground(TcxCustomGridTableView *Sender,
          TcxCanvas *ACanvas, TcxCustomGridCellViewInfo *AViewInfo,
          bool &ADone);
  void __fastcall tvOrdersCustomDrawCell(TcxCustomGridTableView *Sender,
          TcxCanvas *ACanvas, TcxGridTableDataCellViewInfo *AViewInfo,
          bool &ADone);
  void __fastcall tvOrdersCustomDrawColumnHeader(TcxGridTableView *Sender,
          TcxCanvas *ACanvas, TcxGridColumnHeaderViewInfo *AViewInfo,
          bool &ADone);
  void __fastcall tvOrdersCustomDrawFooterCell(TcxGridTableView *Sender,
          TcxCanvas *ACanvas, TcxGridColumnHeaderViewInfo *AViewInfo,
          bool &ADone);
  void __fastcall tvOrdersCustomDrawPartBackground(
          TcxCustomGridTableView *Sender, TcxCanvas *ACanvas,
          TcxCustomGridCellViewInfo *AViewInfo, bool &ADone);
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall FormDestroy(TObject *Sender);
  void __fastcall tvCarsCustomDrawGroupCell(TcxCustomGridTableView *Sender,
          TcxCanvas *ACanvas, TcxGridTableCellViewInfo *AViewInfo,
          bool &ADone);
  void __fastcall tvOrdersCustomDrawGroupCell(
          TcxCustomGridTableView *Sender, TcxCanvas *ACanvas,
          TcxGridTableCellViewInfo *AViewInfo, bool &ADone);
  void __fastcall tvOrdersCustomDrawIndicatorCell(TcxGridTableView *Sender,
          TcxCanvas *ACanvas,
          TcxCustomGridIndicatorItemViewInfo *AViewInfo, bool &ADone);
  void __fastcall tvCarsCustomDrawIndicatorCell(TcxGridTableView *Sender,
          TcxCanvas *ACanvas,
          TcxCustomGridIndicatorItemViewInfo *AViewInfo, bool &ADone);
private:  // User declarations
    TCustomDrawingStyleArr FCustomDrawingStyle;
    TColorSchemes FColorScheme;
    Graphics::TBitmap *FSkyBitmap, *FEgyptBitmap, *FMyFaceBitmap, *FTileBitmap;
    TBrush *FGridBrushMasterCell,
    *FGridBrushMasterFooterCell,
    *FGridBrushMasterColumnHeader,
    *FGridBrushMasterGroupCell,
    *FGridBrushMasterPartBackground,

    *FGridBrushDetailCell,
    *FGridBrushDetailGroupCell,
    *FGridBrushDetailFooterCell,
    *FGridBrushDetailColumnHeader,
    *FGridBrushDetailPartBackground,
    *FGridBrushMasterIndicatorCell,
    *FGridBrushDetailIndicatorCell;
    Graphics::TBitmap* FBitMap;
    TFonts FFonts;
    TBkImages FBkImages;
    int FIndicatorImageIndex[2];
  protected:
    void __fastcall SetCustomDrawingStyle(TViewType AViewType, TCustomDrawArea ACustomDrawArea, const TCustomDrawingStyle Value);
    TCustomDrawingStyle __fastcall GetCustomDrawingStyle(TViewType AViewType, TCustomDrawArea ACustomDrawArea);
    void __fastcall SetCustomDrawingStyles();
    TBkImage __fastcall GetCustomBkImage(TViewType AViewType, TCustomDrawArea ACustomDrawArea);
    void __fastcall SetCustomBkImage(TViewType AViewType, TCustomDrawArea ACustomDrawArea, TBkImage AValue);
    CustomDrawTableViewDemoTypesH::TColorScheme __fastcall GetCustomColorScheme(TViewType AViewType, TCustomDrawArea ACustomDrawArea);
    void __fastcall SetCustomColorScheme(TViewType AViewType, TCustomDrawArea ACustomDrawArea, const CustomDrawTableViewDemoTypesH::TColorScheme Value);
    void __fastcall SetUserDefineBitmap(TViewType AViewType, TCustomDrawArea ACustomDrawArea, Graphics::TBitmap* Value);
    TFont* __fastcall GetFont(TViewType AViewType, TCustomDrawArea ACustomDrawArea);
    void __fastcall SetFont(TViewType AViewType, TCustomDrawArea ACustomDrawArea, const TFont* Value);
    void __fastcall InitFonts();
    Graphics::TBitmap* __fastcall TCustomDrawTableViewDemoMainForm::GetImage(TBkImage AValue);
    int __fastcall GetIndicatorImageIndex(TViewType AViewType);
    void __fastcall SetIndicatorImageIndex(TViewType AViewType, const int Value);
  public:
    __property TCustomDrawingStyle CustomDrawingStyle[TViewType AViewType][TCustomDrawArea ACustomDrawArea] = {read=GetCustomDrawingStyle, write=SetCustomDrawingStyle};
    __property TBkImage CustomBkImage[TViewType AViewType][TCustomDrawArea ACustomDrawArea] = {read=GetCustomBkImage, write=SetCustomBkImage};
    __property CustomDrawTableViewDemoTypesH::TColorScheme CustomColorScheme[TViewType AViewType][TCustomDrawArea ACustomDrawArea] = {read=GetCustomColorScheme, write=SetCustomColorScheme};
    __property Graphics::TBitmap* UserDefindedBitmap[TViewType AViewType][TCustomDrawArea ACustomDrawArea] = {write=SetUserDefineBitmap};
    __property TFont* Font[TViewType AViewType][TCustomDrawArea ACustomDrawArea] = {read=GetFont, write=SetFont};
    __property int IndicatorImageIndex[TViewType AViewType] = {read=GetIndicatorImageIndex, write=SetIndicatorImageIndex};
    __fastcall TCustomDrawTableViewDemoMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TCustomDrawTableViewDemoMainForm *CustomDrawTableViewDemoMainForm;
//---------------------------------------------------------------------------
#endif
