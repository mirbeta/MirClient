//---------------------------------------------------------------------------

#ifndef GridMenuViewsDemoMainH
#define GridMenuViewsDemoMainH
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
#include "cxGridCustomPopupMenu.hpp"
#include "cxGridPopupMenu.hpp"
#include "cxLookAndFeels.hpp"
#include "BaseForm.h"
#include "cxBlobEdit.hpp"
#include "cxCalc.hpp"
#include "cxCalendar.hpp"
#include "cxDataStorage.hpp"
#include "cxDBLookupComboBox.hpp"
#include "cxGridCardView.hpp"
#include "cxImageComboBox.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxSpinEdit.hpp"
#include "CarsData.h"
#include "CarsDataForGrid.h"
//---------------------------------------------------------------------------
class TGridMenuViewsDemoMainForm : public TfmBaseForm
{
__published:  // IDE-managed Components
  TcxGrid *Grid;
  TcxGridDBTableView *tvOrders;
  TcxGridDBColumn *tvOrdersCustomerID;
  TcxGridDBColumn *tvOrdersProductID;
  TcxGridDBColumn *tvOrdersPurchaseDate;
  TcxGridDBColumn *tvOrdersPurchaseMonth;
  TcxGridDBColumn *tvOrdersPaymentType;
  TcxGridDBColumn *tvOrdersPaymentAmount;
  TcxGridDBColumn *tvOrdersDescription;
  TcxGridDBColumn *tvOrdersQuantity;
  TcxGridLevel *lvOrders;
  TMenuItem *miOptions;
  TMenuItem *CustomizePopupmenus1;
  TMenuItem *miUseBuiltInPopupMenu;
  TMenuItem *miAddCopyToClipboard;
  TMenuItem *miUseCustomPopupMenu;
  TcxGridPopupMenu *GridPopupMenu;
  TPopupMenu *PopupMenu;
  TMenuItem *miInsert;
  TMenuItem *miDelete;
  void __fastcall FormShow(TObject *Sender);
  void __fastcall miCopyToClipboardClick(TObject *Sender);
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall GridMenuPopup(TComponent *ASenderMenu,
      TcxCustomGridHitTest *AHitTest, int X, int Y);
  void __fastcall miDeleteClick(TObject *Sender);
  void __fastcall miInsertClick(TObject *Sender);
  void __fastcall miUseBuiltInPopupMenuClick(TObject *Sender);
  void __fastcall miAddCopyToClipboardClick(TObject *Sender);
  void __fastcall miUseCustomPopupMenuClick(TObject *Sender);
private:  // User declarations
  int GetSummaryItemIndexByColumn(TcxDataSummaryItems *ASummaryItems,
    TcxGridColumn *AColumn);
  Variant GetFooterSummaryValue(TcxGridFooterCellHitTest *AHitTest);
  Variant GetGroupFooterSummaryValue(TcxGridGroupFooterCellHitTest *AHitTest);
public:   // User declarations
  TMenuItem *FMenuItem;
  void InsertMenuItem();
  __fastcall TGridMenuViewsDemoMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TGridMenuViewsDemoMainForm *GridMenuViewsDemoMainForm;
//---------------------------------------------------------------------------
#endif
