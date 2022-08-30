//---------------------------------------------------------------------------

#ifndef FilterDropDownDemoMainH
#define FilterDropDownDemoMainH
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
#include "cxStyles.hpp"
#include <ActnList.hpp>
#include <ComCtrls.hpp>
#include <DB.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
#include "cxLookAndFeels.hpp"
#include "DemoBasicMain.h"
#include "cxDBLookupComboBox.hpp"
#include "cxDBTL.hpp"
#include "cxEditRepositoryItems.hpp"
#include "cxInplaceContainer.hpp"
#include "cxMaskEdit.hpp"
#include "dxFilterValueContainer.hpp"
#include "dxFilterPopupWindow.hpp"
#include "cxTL.hpp"
#include "cxTLData.hpp"
//---------------------------------------------------------------------------
class TfrmMain : public TDemoBasicMainForm
{
__published: // IDE-managed Components
	TcxEditRepository* erMain;
	TcxEditRepositoryImageItem* erMainFlag;
	TcxDBTreeList* TreeList;
	TcxDBTreeListColumn* TreeListName;
	TcxDBTreeListColumn* TreeListModification;
	TcxDBTreeListColumn* TreeListPrice;
	TcxDBTreeListColumn* TreeListMPGCity;
	TcxDBTreeListColumn* TreeListMPGHighway;
	TcxDBTreeListColumn* TreeListCilinders;
	TcxDBTreeListColumn* TreeListSalesDate;
	TcxDBTreeListColumn* TreeListBodyStyle;
	TcxStyleRepository* cxStyleRepository1;
	TcxStyle* stMaroon;
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
	TMenuItem* miSeparator;

	void __fastcall TreeListStylesGetContentStyle(TcxCustomTreeList* Sender, TcxTreeListColumn* AColumn, TcxTreeListNode* ANode,
		TcxStyle* &AStyle);
	void __fastcall FormCreate(TObject* Sender);
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
