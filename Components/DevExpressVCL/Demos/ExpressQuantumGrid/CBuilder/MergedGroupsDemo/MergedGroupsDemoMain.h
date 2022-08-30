//---------------------------------------------------------------------------

#ifndef MergedGroupsDemoMainH
#define MergedGroupsDemoMainH
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
  #include <dxToggleSwitch.hpp> 
  #include <cxCheckGroup.hpp> 
  #include <dxmdaset.hpp> 
  #include <dxPSGlbl.hpp> 
  #include <dxPSUtl.hpp> 
  #include <dxPSEngn.hpp> 
  #include <dxPrnPg.hpp> 
  #include <dxBkgnd.hpp> 
  #include <dxWrap.hpp>
  #include <dxPrnDev.hpp> 
  #include <dxPSCompsProvider.hpp> 
  #include <dxPSFillPatterns.hpp> 
  #include <dxPSEdgePatterns.hpp>
  #include <dxPSPDFExportCore.hpp> 
  #include <dxPSPDFExport.hpp> 
  #include <cxDrawTextUtils.hpp> 
  #include <dxPSPrVwStd.hpp> 
  #include <dxPSPrVwAdv.hpp>
  #include <dxPSPrVwRibbon.hpp> 
  #include <dxPScxPageControlProducer.hpp> 
  #include <dxPScxGridLnk.hpp>
  #include <dxPScxGridLayoutViewLnk.hpp> 
  #include <dxPScxEditorProducers.hpp> 
  #include <dxPScxExtEditorProducers.hpp>
  #include <dxPSCore.hpp> 
  #include <dxPScxCommon.hpp> 
  #include <cxButtons.hpp> 
  #include <Forms.hpp> 
  #include <Windows.hpp> 
  #include <cxDBLookupComboBox.hpp>
//---------------------------------------------------------------------------
class TfrmMain : public TfmBaseForm
{
__published: // IDE-managed Components
    TcxEditRepository* erMain;
    TcxEditRepositoryImageItem* erMainFlag;
    TDataSource* dsOrders;
    TDataSource* dsCustomers;
    TcxGrid* Grid;
    TcxGridDBTableView* TableView;
    TcxGridDBColumn* TableViewCompanyName;
    TcxGridDBColumn* TableViewModel;
    TcxGridDBColumn* TableViewQuantity;
    TcxGridDBColumn* TableViewUnitPrice;
    TcxGridDBColumn* TableViewPaymentAmount;
    TcxGridDBColumn* TableViewPaymentType;
    TcxGridDBColumn* TableViewPurchaseDate;
    TcxGridLevel* GridLevel1;
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
    TStringField* cdsOrdersCarName;
    TClientDataSet* cdsCustomers;
    TIntegerField* cdsCustomersID;
    TStringField* cdsCustomersCompany;
    TcxGridDBColumn* TableViewTrademark;
	void __fastcall FormCreate(TObject* Sender);
	void __fastcall TableViewDataControllerGroupingChanged(TObject* Sender);
	void __fastcall cdsOrdersCalcFields(TDataSet* DataSet);
protected: // User declarations
public:	// User declarations
	__fastcall TfrmMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
