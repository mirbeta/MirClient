//---------------------------------------------------------------------------

#ifndef FixedGroupsDemoMainH
#define FixedGroupsDemoMainH
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
  #include <dxToggleSwitch.hpp>
  #include <CarsDataForGrid.h>
  #include <BaseForm.h>
//---------------------------------------------------------------------------
class TfrmMain : public TfmBaseForm
{
__published: // IDE-managed Components
	TcxEditRepository* erMain;
    TcxEditRepositoryImageItem* erMainFlag;
    TcxGroupBox* gbOptions;
    TActionList* alAction;
    TAction* acFixedGroups;
    TMenuItem* miFixedGroupsOptions;
    TMenuItem* miFixedGroups;
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
    TdxToggleSwitch* tsFixedGroups;
	void __fastcall FormCreate(TObject* Sender);
	void __fastcall acFixedGroupsExecute(TObject* Sender);
	void __fastcall cdsOrdersCalcFields(TDataSet* DataSet);
protected: // User declarations
public:	// User declarations
	__fastcall TfrmMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
