//---------------------------------------------------------------------------

#ifndef CheckBoxMultiSelectMainH
#define CheckBoxMultiSelectMainH
//---------------------------------------------------------------------------
  #include <SysUtils.hpp>
  #include <Classes.hpp>
  #include <Controls.hpp>
  #include <Menus.hpp>
  #include <Variants.hpp>
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
  #include <cxRadioGroup.hpp>
  #include <CarsDataForGrid.h>
  #include <BaseForm.h>
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
	TcxGroupBox* gbCheckBoxPosition;
	TcxRadioButton* rbFirstColumn;
	TcxRadioButton* rbIndicator;
	TcxCheckBox* cbShowCheckBoxesDynamically;
	TcxCheckBox* cbClearSelectionOnClickOutsideSelection;
	TcxGroupBox* cxGroupBox2;
	TcxCheckBox* cbDataRowCheckBoxVisible;
	TcxCheckBox* cbGroupRowCheckBoxVisible;
	TcxCheckBox* cbColumnHeaderCheckBoxSelectorVisible;
	TcxCheckBox* cbPersistentSelection;
	void __fastcall FormCreate(TObject* Sender);
	void __fastcall cdsOrdersCalcFields(TDataSet* DataSet);
	void __fastcall cbPersistentSelectionPropertiesEditValueChanged(TObject* Sender);
	void __fastcall CheckBoxPositionChanged(TObject* Sender);
	void __fastcall cbGroupRowCheckBoxVisibilityChanged(TObject* Sender);
	void __fastcall cbColumnHeaderCheckBoxVisibilityChanged(TObject* Sender);
	void __fastcall cbShowCheckBoxesDynamicallyChanged(TObject* Sender);
	void __fastcall cbClearSelectionWithAClickOutsideChanged(TObject* Sender);
	void __fastcall cbDataRowCheckBoxVisibilityChanged(TObject* Sender);
	void __fastcall FormShow(TObject* Sender);
protected: // User declarations
	void UpdateCheckBoxesVisibility(TcxGridCheckBoxVisibilityOption AOption, bool AInclude);
public:	// User declarations
	__fastcall TfrmMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
