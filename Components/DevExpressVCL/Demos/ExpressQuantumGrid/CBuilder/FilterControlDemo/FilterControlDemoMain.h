//---------------------------------------------------------------------------

#ifndef FilterControlDemoMainH
#define FilterControlDemoMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxButtons.hpp"
#include "cxClasses.hpp"
#include "cxControls.hpp"
#include "cxCustomData.hpp"
#include "cxData.hpp"
#include "cxDBData.hpp"
#include "cxDBEditRepository.hpp"
#include "cxEdit.hpp"
#include "cxFilter.hpp"
#include "cxFilterControl.hpp"
#include "cxDBFilterControl.hpp"
#include "cxGraphics.hpp"
#include "cxGrid.hpp"
#include "cxGridCustomPopupMenu.hpp"
#include "cxGridCustomTableView.hpp"
#include "cxGridCustomView.hpp"
#include "cxGridDBTableView.hpp"
#include "cxGridLevel.hpp"
#include "cxGridPopupMenu.hpp"
#include "cxGridTableView.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "cxStyles.hpp"
#include <ActnList.hpp>
#include <ComCtrls.hpp>
#include <DB.hpp>
#include <DBGrids.hpp>
#include <Dialogs.hpp>
#include <ExtCtrls.hpp>
#include <Grids.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
#include "BaseForm.h"
#include "cxCalc.hpp"
#include "cxCalendar.hpp"
#include "cxDataStorage.hpp"
#include "cxDBLookupComboBox.hpp"
#include "cxGridCardView.hpp"
#include "cxImageComboBox.hpp"
#include "cxPC.hpp"
#include "CarsData.h"
#include "CarsDataForGrid.h"
#include "cxSpinEdit.hpp"
#include "cxTimeEdit.hpp"
//---------------------------------------------------------------------------
enum TcxLocate {ltNone, ltLeft, ltTop};

class TFilterControlDemoMainForm : public TfmBaseForm
{
__published:  // IDE-managed Components
  TSplitter *Splitter;
	TcxPageControl *PageControl;
	TcxTabSheet *tsGrid4DisplayStyle;
  TcxGrid *Grid;
  TcxGridDBTableView *tvOrders;
  TcxGridDBColumn *tvOrdersCustomerID;
  TcxGridDBColumn *tvOrdersProductID;
  TcxGridDBColumn *tvOrdersPurchaseDate;
  TcxGridDBColumn *tvOrdersTime;
  TcxGridDBColumn *tvOrdersPaymentAmount;
  TcxGridDBColumn *tvOrdersPaymentType;
  TcxGridDBColumn *tvOrdersQuantity;
  TcxGridLevel *lvOrders;
	TcxTabSheet *tsStandardDisplayStyle;
  TLabel *Label1;
  TDBGrid *DBGrid;
  TPanel *pnlFilterDialog;
  TcxFilterControl *cxFilterControl;
  TPanel *pnlButtons;
  TcxButton *btnLoad;
  TcxButton *btnSaveAs;
  TcxButton *btnApply;
  TcxButton *btnOK;
  TcxButton *btnCancel;
  TcxDBFilterControl *cxDBFilterControl;
  TMenuItem *miOptions;
  TMenuItem *miFilterControl;
  TMenuItem *miFilterControlPosition;
  TMenuItem *miFilterControlNone;
  TMenuItem *miFilterControlLeft;
  TMenuItem *miFilterControlTop;
  TMenuItem *N2;
  TcxGridPopupMenu *cxGridPopupMenu1;
  TOpenDialog *OpenDialog;
  TSaveDialog *SaveDialog;
  TPopupMenu *PopupMenu;
  TMenuItem *miShowFilterPanel;
  TMenuItem *miFilterPnlNever;
  TMenuItem *miFilterPnlNeverNonEmpty;
  TMenuItem *miFilterPnlAlways;
  TMenuItem *miShowFilterBtn;
  TcxEditRepository *cxEditRepository;
  TcxEditRepositoryLookupComboBoxItem *CustomersEditorItem;
  TcxEditRepositoryLookupComboBoxItem *CarsEditorItem;
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall cxFilterControlApplyFilter(TObject *Sender);
  void __fastcall cxDBFilterControlApplyFilter(TObject *Sender);
  void __fastcall btnLoadClick(TObject *Sender);
  void __fastcall btnSaveAsClick(TObject *Sender);
  void __fastcall btnApplyClick(TObject *Sender);
  void __fastcall PageControlChange(TObject *Sender);
  void __fastcall miFilterControlPosClick(TObject *Sender);
  void __fastcall miFilterControlModalClick(TObject *Sender);
  void __fastcall tvOrdersFilterControlDialogShow(TObject *Sender);
  void __fastcall tvOrdersDataControllerFilterChanged(TObject *Sender);
  void __fastcall miShowFilterPnlClick(TObject *Sender);
  void __fastcall miShowFilterBtnClick(TObject *Sender);
private:  // User declarations
   TcxCustomFilterControl* FFilterControl;
   TcxLocate FFilterControlAlign;
   TCloseEvent FCloseEvent;
   void __fastcall Relocate(TcxLocate ALocate);
  void __fastcall ShowButtons(bool AShow);
public:   // User declarations
  __fastcall TFilterControlDemoMainForm(TComponent* Owner);
   void __fastcall FilterDialogClose(System::TObject* Sender, TCloseAction &Action);
};
//---------------------------------------------------------------------------
extern PACKAGE TFilterControlDemoMainForm *FilterControlDemoMainForm;
//---------------------------------------------------------------------------
#endif
 