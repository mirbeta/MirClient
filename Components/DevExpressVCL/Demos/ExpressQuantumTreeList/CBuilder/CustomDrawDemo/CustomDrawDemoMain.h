//---------------------------------------------------------------------------

#ifndef CustomDrawDemoMainH
#define CustomDrawDemoMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "dxCore.hpp"
#include "cxClasses.hpp"
#include "cxControls.hpp"
#include "cxCustomData.hpp"
#include "cxData.hpp"
#include "cxDBData.hpp"
#include "cxEdit.hpp"
#include "cxFilter.hpp"
#include "cxGraphics.hpp"
#include "cxStyles.hpp"
#include <ActnList.hpp>
#include <ComCtrls.hpp>
#include <DB.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
#include "cxLookAndFeels.hpp"
#include "DemoBasicMain.h"
#include "CustomDrawDemoTypes.h"
#include "cxCheckBox.hpp"
#include "cxCurrencyEdit.hpp"
#include "cxDBLookupComboBox.hpp"
#include "cxDBTL.hpp"
#include "cxEditRepositoryItems.hpp"
#include "cxInplaceContainer.hpp"
#include "cxMaskEdit.hpp"
#include "cxMemo.hpp"
#include "cxTL.hpp"
#include "cxTLData.hpp"
#include "cxTextEdit.hpp"
//---------------------------------------------------------------------------

class TcxItemCustomDrawInfo;

class TcxCustomDrawInfo {
private:
  TList *FBitmaps;
  TFont *FDefaultFont;
  TList *FCustomDrawData;
  Graphics::TBitmap* GetBkBitmap(TBkImage ABkImage);
  int GetCount(void);
  TcxItemCustomDrawInfo* GetItem(TCustomDrawArea ADrawArea);
  void LoadResourceBitmaps();
public:
  __fastcall TcxCustomDrawInfo();
  __fastcall ~TcxCustomDrawInfo();
  void AddNewItem(TCustomDrawArea ADrawArea, TcxItemCustomDrawType AItemType);
  TcxItemCustomDrawInfo* GetItemByIndex(int AIndex);
  __property Graphics::TBitmap* Bitmaps[TBkImage ABkImage] = {read=GetBkBitmap};
  __property int Count = {read=GetCount};
  __property TFont* DefaultFont = {read=FDefaultFont};
  __property TcxItemCustomDrawInfo* Items[TCustomDrawArea ADrawArea] = {read = GetItem};
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

//---------------------------------------------------------------------------
class TCustomDrawDemoMainForm : public TDemoBasicMainForm
{
__published:	// IDE-managed Components
  TcxEditRepository *cxEditRepository1;
  TcxEditRepositoryMaskItem *eriTelephoneMaskEdit;
  TcxDBTreeList *cxDBTreeList;
  TcxDBTreeListColumn *cxDBTreeListID;
  TcxDBTreeListColumn *cxDBTreeListPARENTID;
  TcxDBTreeListColumn *cxDBTreeListNAME;
  TcxDBTreeListColumn *cxDBTreeListPHONE;
  TcxDBTreeListColumn *cxDBTreeListFAX;
  TcxDBTreeListColumn *cxDBTreeListBUDGET;
  TcxDBTreeListColumn *cxDBTreeListVACANCY;
  TcxDBTreeListColumn *cxDBTreeListManager;
  TcxDBTreeListColumn *cxDBTreeListManagerPhone;
  TcxDBTreeListColumn *cxDBTreeListManagerEmail;
  TcxDBTreeListColumn *cxDBTreeListManagerAdress;
  void __fastcall FormShow(TObject *Sender);
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall FormDestroy(TObject *Sender);
  void __fastcall actCustomizationFormExecute(TObject *Sender);
  void __fastcall cxDBTreeListCustomDrawBackground(TcxCustomTreeList *Sender,
    TcxCanvas *ACanvas, TcxTreeListCustomCellViewInfo *AViewInfo, bool &ADone);
  void __fastcall cxDBTreeListCustomDrawBandHeader(TcxCustomTreeList *Sender,
    TcxCanvas *ACanvas, TcxTreeListBandCellViewInfo *AViewInfo,
    bool &ADone);
  void __fastcall cxDBTreeListCustomDrawFooterCell(TcxCustomTreeList *Sender,
    TcxCanvas *ACanvas, TcxTreeListFooterCellViewInfo *AViewInfo,
    bool &ADone);
  void __fastcall cxDBTreeListCustomDrawHeaderCell(TcxCustomTreeList *Sender,
    TcxCanvas *ACanvas, TcxTreeListHeaderCellViewInfo *AViewInfo,
    bool &ADone);
  void __fastcall cxDBTreeListCustomDrawIndentCell(TcxCustomTreeList *Sender,
    TcxCanvas *ACanvas, TcxTreeListIndentCellViewInfo *AViewInfo,
    bool &ADone);
  void __fastcall cxDBTreeListCustomDrawIndicatorCell(TcxCustomTreeList *Sender,
    TcxCanvas *ACanvas, TcxTreeListIndicatorCellViewInfo *AViewInfo,
    bool &ADone);
  void __fastcall cxDBTreeListCustomDrawPreview(TcxCustomTreeList *Sender,
    TcxCanvas *ACanvas, TcxTreeListEditCellViewInfo *AViewInfo,
    bool &ADone);
  void __fastcall cxDBTreeListCustomDrawCell(TcxCustomTreeList *Sender,
    TcxCanvas *ACanvas, TcxTreeListEditCellViewInfo *AViewInfo,
    bool &ADone);
  void __fastcall cxDBTreeListCustomDrawHeader(TcxCustomTreeList *Sender,
    TcxCanvas *ACanvas, TcxTreeListHeaderCellViewInfo *AViewInfo,
    bool &ADone);
        void __fastcall cxDBTreeListDragOver(TObject *Sender,
          TObject *Source, int X, int Y, TDragState State, bool &Accept);
private:
  TcxCustomDrawInfo *FCustomDrawInfo;
  void AddCustomDrawInfos();
  void AdjustCustomDrawItems();
  bool DrawCellItem(TcxItemCustomDrawInfo *AItem, TcxCanvas *ACanvas,
    TcxTreeListEditCellViewInfo *AViewInfo, TObject *Sender);
  bool DrawHeaderItem(TcxItemCustomDrawInfo *AItem, TcxCanvas *ACanvas,
	TcxTreeListHeaderCellViewInfo *AViewInfo, TObject *Sender);
  bool DrawItem(TcxItemCustomDrawInfo *AItem, TcxCanvas *ACanvas,
	const TRect &R);
  bool DrawIndicatorItem(TcxItemCustomDrawInfo *AItem, TcxCanvas *ACanvas,
	const TRect &R);
  void DrawTreeLines(TcxTreeListIndentCellViewInfo *AViewInfo, TcxCanvas *ACanvas);
  void DrawRectWithBorders(TcxCanvas *ACanvas, TcxBorders ABorders,
    TColor AColor, const TRect &ARect);
  void OwnerDrawCell(TcxTreeListEditCellViewInfo *AViewInfo, TcxCanvas *ACanvas,
    TColor ALinesColor, TFont *AFont);
public:
  __property TcxCustomDrawInfo *CustomDrawInfo = {read=FCustomDrawInfo};
  __fastcall TCustomDrawDemoMainForm(TComponent* Owner);
  virtual bool __fastcall IsNativeDefaultStyle();
};
//---------------------------------------------------------------------------
extern PACKAGE TCustomDrawDemoMainForm *CustomDrawDemoMainForm;
//---------------------------------------------------------------------------
#endif
