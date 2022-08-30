//---------------------------------------------------------------------------

#ifndef FindPanelDemoMainH
#define FindPanelDemoMainH
//---------------------------------------------------------------------------
  #include <SysUtils.hpp>
  #include <Classes.hpp>
  #include <Controls.hpp>
  #include <Menus.hpp>
  #include <DB.hpp>
  #include <StdCtrls.hpp>
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
  #include <BaseForm.h>
//---------------------------------------------------------------------------
class TfrmMain : public TfmBaseForm
{
__published: // IDE-managed Components
	TcxEditRepository* erMain;
	TcxEditRepositoryImageItem* erMainFlag;
	TcxGroupBox* cxGroupBox1;
	TcxCheckBox* cbClearFindOnClose;
	TcxCheckBox* cbShowClearButton;
	TcxCheckBox* cbShowCloseButton;
	TcxCheckBox* cbShowFindButton;
	TcxCheckBox* cbHighlightSearchResults;
	TActionList* alAction;
	TAction* actClearFindOnClose;
	TAction* actShowClearButton;
	TAction* actShowCloseButton;
	TAction* actShowFindButton;
	TAction* actHighlightSearchResults;
	TMenuItem* miFindPanelOptions;
	TMenuItem* ClearFindOnClose1;
	TMenuItem* HighlightFindResult1;
	TMenuItem* miVisibleButtons;
	TMenuItem* ShowClearButton2;
	TMenuItem* ShowCloseButton2;
	TMenuItem* ShowFindButton2;
	TcxSpinEdit* seFindDelay;
	TcxLabel* lbSearchDelay;
	TcxImageComboBox* icbFindFilterColumns;
	TcxLabel* lbSearchableColumns;
	TcxComboBox* cbeFindPanelPosition;
	TcxLabel* lbFindPanelPosition;
	TcxComboBox* cbeDisplayMode;
	TcxLabel* lbDisplayMode;
	TcxCheckBox* cbUseDelayedSearch;
	TAction* actUseDelayedSearch;
	TMenuItem* UseDelayedFind1;
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
	TcxCheckBox* cbUseExtendedSyntax;
	TMenuItem* UseExtendedSyntax1;
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
	void __fastcall FormCreate(TObject* Sender);
	void __fastcall actClearFindOnCloseChange(TObject* Sender);
	void __fastcall actShowClearButtonChange(TObject* Sender);
	void __fastcall actShowCloseButtonChange(TObject* Sender);
	void __fastcall actShowFindButtonEChange(TObject* Sender);
	void __fastcall actHighlightFindResultChange(TObject* Sender);
	void __fastcall seFindDelayPropertiesChange(TObject* Sender);
	void __fastcall icbFindFilterColumnsPropertiesChange(TObject* Sender);
	void __fastcall cbFindPanelPositionPropertiesChange(TObject* Sender);
	void __fastcall cbDisplayModePropertiesChange(TObject* Sender);
	void __fastcall actUseDelayedSearchExecute(TObject* Sender);
	void __fastcall cdsOrdersCalcFields(TDataSet* DataSet);
	void __fastcall actUseExtendedSyntaxExecute(TObject* Sender);
protected: // User declarations
	void UpdateFindFilterColumns();
public:	// User declarations
	__fastcall TfrmMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
