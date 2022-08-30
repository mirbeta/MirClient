//---------------------------------------------------------------------------

#ifndef FilterDropDownDemoMainH
#define FilterDropDownDemoMainH
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
  #include <dxFilterPopupWindow.hpp>
  #include <dxFilterValueContainer.hpp>
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
	TActionList* alAction;
	TcxGrid* Grid;
	TcxGridDBTableView* TableView;
	TcxGridLevel* GridLevel1;
	TcxGridDBColumn* TableViewRecId;
	TcxGridDBColumn* TableViewID;
	TcxGridDBColumn* TableViewTrademark;
	TcxGridDBColumn* TableViewName;
	TcxGridDBColumn* TableViewModification;
	TcxGridDBColumn* TableViewPrice;
	TcxGridDBColumn* TableViewMPGCity;
	TcxGridDBColumn* TableViewMPGHighway;
	TcxGridDBColumn* TableViewBodyStyleID;
	TcxGridDBColumn* TableViewCilinders;
	TcxGridDBColumn* TableViewSalesDate;
	TcxGridDBColumn* TableViewBodyStyle;
	TAction* acExcelModeApplyChangesImmediately;
	TAction* acExcelModeApplyChangesOnTabOrOKButtonClick;
	TAction* acExcelModeDateTimePageTypeTree;
	TAction* acExcelModeDateTimePageTypeList;
	TAction* acExcelModeNumericPageTypeRange;
	TAction* acExcelModeNumericPageTypeList;
	TAction* acClassicModeApplyChangesImmediately;
	TAction* acClassicModeApplyChangesOnButtonClick;
	TMenuItem* miClassicModeMultiSelect;
	TAction* acClassicModeMultiSelect;
	TAction* acExcelModeDateTimePageType;
	TAction* acExcelModeApplyChanges;
	TAction* acExcelModeNumericPageType;
	TAction* acClassicModeApplyChanges;
	TAction* acFilterPopupMode;
	TMenuItem* miFilterPopup;
	TMenuItem* miFilterPopupMode;
	TMenuItem* miFilterPopupModeClassic;
	TMenuItem* miFilterPopupModeExcel;
	TAction* acFilterPopupModeClassic;
	TAction* acFilterPopupModeExcel;
	TMenuItem* miExcelModeApplyChanges;
	TMenuItem* miExcelModeDateTimePage;
	TMenuItem* miExcelModeNumericPage;
	TMenuItem* miExcelModeApplyChangesImmediatly;
	TMenuItem* miExcelModeTabOrOKButtonClick;
	TMenuItem* miDateTimePageTree;
	TMenuItem* miDateTimePageList;
	TMenuItem* miNumericPageTree;
	TMenuItem* miNumericPageList;
	TMenuItem* miClassicModeApplyChanges;
	TMenuItem* miClassicModeApplyChangesImmediatly;
	TMenuItem* miClassicModeApplyChangesButtonClick;
	TMenuItem* miSeparator;

	void __fastcall acFilterPopupModeExecute(TObject* Sender);
	void __fastcall acExcelModeApplyChangesExecute(TObject* Sender);
	void __fastcall acDateTimePageTypeExecute(TObject* Sender);
	void __fastcall acNumericPageTypeExecute(TObject* Sender);
	void __fastcall acClassicModeApplyChangesExecute(TObject* Sender);
	void __fastcall acClassicModeMultiSelectExecute(TObject* Sender);
	void __fastcall acDoNothingExecute(TObject* Sender);
protected: // User declarations
	void UpdateFilterPopupActions();
public:	// User declarations
	__fastcall TfrmMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
