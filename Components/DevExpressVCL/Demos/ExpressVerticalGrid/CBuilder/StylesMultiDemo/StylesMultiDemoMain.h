//---------------------------------------------------------------------------

#ifndef StylesMultiDemoMainH
#define StylesMultiDemoMainH
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
#include "cxButtons.hpp"
#include "cxDBVGrid.hpp"
#include "cxInplaceContainer.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxStyleSheetEditor.hpp"
#include "cxVGrid.hpp"
#include <Dialogs.hpp>
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
enum TcxStyleRepositoryType {shtNone, shtPredefined, shtUserDefined};

class TStylesMultiDemoMainForm : public TDemoBasicMainForm
{
__published:	// IDE-managed Components
  TPanel *pnlLeft;
  TGroupBox *gbUserDefined;
  TcxButton *btnLoad;
  TcxButton *btnSave;
  TcxButton *btnEdit;
  TGroupBox *gbPredefined;
  TcxVerticalGrid *vgStyleSheets;
  TcxCategoryRow *vgStyleSheetsNone;
  TcxCategoryRow *vgStyleSheetsUserDefinedStyleSheets;
  TcxCategoryRow *vgStyleSheetsPredefinedStyleSheets;
  TPanel *pnlCurrentStyleSheet;
  TSplitter *Splitter;
  TcxDBVerticalGrid *cxDBVerticalGrid;
  TcxCategoryRow *cxDBVerticalGridOrderInfo;
  TcxDBEditorRow *cxDBVerticalGridPurchaseDate;
  TcxDBEditorRow *cxDBVerticalGridTime;
  TcxDBEditorRow *cxDBVerticalGridPaymentType;
  TcxDBEditorRow *cxDBVerticalGridPaymentAmount;
  TcxDBEditorRow *cxDBVerticalGridQuantity;
  TcxCategoryRow *cxDBVerticalGridCustomerInfo;
  TcxCategoryRow *cxDBVerticalGridCommonCustomerInfo;
  TcxDBEditorRow *cxDBVerticalGridFirstName;
  TcxDBEditorRow *cxDBVerticalGridLastName;
  TcxDBEditorRow *cxDBVerticalGridSpouse;
  TcxDBEditorRow *cxDBVerticalGridPrefix;
  TcxDBEditorRow *cxDBVerticalGridTitle;
  TcxCategoryRow *cxDBVerticalGridCustomerContacts;
  TcxCategoryRow *cxDBVerticalGridPhonesAndFaxes;
  TcxDBEditorRow *cxDBVerticalGridFaxPhone;
  TcxDBEditorRow *cxDBVerticalGridHomePhone;
  TcxCategoryRow *cxDBVerticalGridCategoryAddress;
  TcxDBEditorRow *cxDBVerticalGridState;
  TcxDBEditorRow *cxDBVerticalGridCity;
  TcxDBEditorRow *cxDBVerticalGridAddress;
  TcxDBEditorRow *cxDBVerticalGridZipCode;
  TcxDBEditorRow *cxDBVerticalGridEmail;
  TcxDBEditorRow *cxDBVerticalGridOccupation;
  TcxDBEditorRow *cxDBVerticalGridCustomer;
  TcxDBEditorRow *cxDBVerticalGridCompany;
  TOpenDialog *OpenDialog;
  TSaveDialog *SaveDialog;
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall actSaveToFileExecute(TObject *Sender);
  void __fastcall actLoadFromFileExecute(TObject *Sender);
  void __fastcall actEditStyleSheetExecute(TObject *Sender);
  void __fastcall FormActivate(TObject *Sender);
  void __fastcall OnGetDisplayText(
    TcxCustomEditorRowProperties *Sender, int ARecord,
    String &AText);
  void __fastcall vgStyleSheetsItemChanged(TObject *Sender,
    TcxCustomRow *AOldRow, int AOldCellIndex);
  void __fastcall actEditAndSaveStyleSheetUpdate(TObject *Sender);
  void __fastcall vgStyleSheetsStylesGetCategoryStyle(TObject *Sender,
    TcxCustomRow *ARow, TcxStyle *&AStyle);
  void __fastcall vgStyleSheetsDrawRowHeader(TObject *Sender,
    TcxCanvas *ACanvas, TcxvgPainter *APainter,
    TcxCustomRowHeaderInfo *AHeaderViewInfo, bool &Done);
private:
  TcxVerticalGridStyleSheet* __fastcall GetCurrentStyleSheet();
  void __fastcall CreateStyleSheetsList(TcxStyleRepositoryType AStyleRepositoryType);
  void __fastcall UpdateGridStyleSheets(TcxVerticalGridStyleSheet *AStyleSheet);
  void __fastcall ClearUserDefinedStyleSheets();
  void __fastcall LoadUserDefinedStyleSheets(TFileName AFileName);
  void __fastcall SaveUserDefinedStyleSheets(TFileName AFileName);
  void __fastcall SelectFistChild(TcxStyleRepositoryType AStyleRepositoryType);
public:		// User declarations
  __fastcall TStylesMultiDemoMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TStylesMultiDemoMainForm *StylesMultiDemoMainForm;
//---------------------------------------------------------------------------
#endif
