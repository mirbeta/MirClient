//---------------------------------------------------------------------------

#ifndef FixedDataRowsMainH
#define FixedDataRowsMainH
//---------------------------------------------------------------------------
  #include <SysUtils.hpp>
  #include <Classes.hpp>
  #include <Controls.hpp>
  #include <Menus.hpp>
  #include <DB.hpp>
  #include <StdCtrls.hpp>
  #include <BaseForm.h>
  #include <ComCtrls.hpp>
  #include <DBClient.hpp>
  #include <dxCore.hpp>
  #include <cxStyles.hpp>
  #include <cxCustomData.hpp>
  #include <cxGraphics.hpp>
  #include <cxFilter.hpp>
  #include <cxData.hpp>
  #include <cxDataStorage.hpp>
  #include <cxEdit.hpp>
  #include <cxGridLevel.hpp>
  #include <cxClasses.hpp>
  #include <cxControls.hpp>
  #include <cxGridCustomView.hpp>
  #include <cxGridCustomTableView.hpp>
  #include <cxGridTableView.hpp>
  #include <cxGrid.hpp>
  #include <cxEditRepositoryItems.hpp>
  #include <cxLookAndFeels.hpp>
  #include <cxLookAndFeelPainters.hpp>
  #include <cxGridCardView.hpp>
  #include <cxNavigator.hpp>
  #include <cxContainer.hpp>
  #include <cxGroupBox.hpp>
  #include <ActnList.hpp>
  #include <cxCheckBox.hpp>
  #include <cxLabel.hpp>
  #include <cxTextEdit.hpp>
  #include <cxMaskEdit.hpp>
  #include <cxSpinEdit.hpp>
  #include <cxDropDownEdit.hpp>
  #include <cxImageComboBox.hpp>
  #include <cxDBData.hpp>
  #include <cxGridDBTableView.hpp>
  #include <XPMan.hpp>
  #include <CarsDataForGrid.h>
  #include <cxRadioGroup.hpp>
  #include <cxTrackBar.hpp>
//---------------------------------------------------------------------------
class TfrmMain : public TfmBaseForm
{
__published: // IDE-managed Components
	TcxEditRepository* erMain;
	TcxEditRepositoryImageItem* erMainFlag;
	TcxGroupBox* cxGroupBox1;
	TActionList* alAction;
	TAction* actClearFindOnClose;
	TAction* actShowClearButton;
	TAction* actShowCloseButton;
	TAction* actShowFindButton;
	TAction* actHighlightSearchResults;
	TAction* actUseDelayedSearch;
	TDataSource* dsOrders;
	TClientDataSet* cdsOrders;
	TAutoIncField* cdsOrdersID;
	TIntegerField* cdsOrdersCustomerID;
	TIntegerField* cdsOrdersProductID;
	TDateTimeField* cdsOrdersPurchaseDate;
	TStringField* cdsOrdersPaymentType;
	TIntegerField* cdsOrdersQuantity;
	TCurrencyField* cdsOrdersUnitPrice;
	TStringField* cdsOrdersCompanyName;
	TCurrencyField* cdsOrdersPaymentAmount;
	TDataSource* dsCustomers;
	TClientDataSet* cdsCustomers;
	TIntegerField* cdsCustomersID;
	TStringField* cdsCustomersCompany;
	TAction* actUseExtendedSyntax;
	TcxGrid* Grid;
	TcxGridDBTableView* TableView;
	TcxGridDBColumn* TableViewCompanyName;
	TcxGridDBColumn* TableViewCarName;
	TcxGridDBColumn* TableViewQuantity;
	TcxGridDBColumn* TableViewUnitPrice;
	TcxGridDBColumn* TableViewPaymentAmount;
	TcxGridDBColumn* TableViewPaymentType;
	TcxGridDBColumn* TableViewPurchaseDate;
	TcxGridLevel* GridLevel1;
	TStringField* cdsOrdersCarName;
	TcxGroupBox* gbPinClickAction;
	TcxRadioButton* rbShowPopup;
	TcxRadioButton* rbNone;
	TcxGroupBox* gbPinVisibility;
	TcxRadioButton* rbPinVisibilityNever;
	TcxRadioButton* rbPinVisibilityAlways;
	TcxRadioButton* rbPinVisibilityRowHover;
	TcxRadioButton* rbPinVisibilityHover;
	TcxRadioButton* rbFixRowToBottom;
	TcxRadioButton* rbFixRowToTop;
	TcxTrackBar* tbSeparatorWidth;
	TcxLabel* lbSeparatorWidth;
	void __fastcall FormCreate(TObject* Sender);
	void __fastcall PinVisibilityChanged(TObject* Sender);
	void __fastcall FixationCapabilityChanged(TObject* Sender);
	void __fastcall tbSeparatorWidthPropertiesChange(TObject* Sender);
protected: // User declarations
	void UpdateFixationCapability();
	void UpdatePinVisibility();
public:	// User declarations
	__fastcall TfrmMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
