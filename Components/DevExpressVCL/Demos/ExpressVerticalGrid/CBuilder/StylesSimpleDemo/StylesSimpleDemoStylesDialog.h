//---------------------------------------------------------------------------

#ifndef StylesSimpleDemoStylesDialogH
#define StylesSimpleDemoStylesDialogH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxButtons.hpp"
#include "cxClasses.hpp"
#include "cxControls.hpp"
#include "cxEdit.hpp"
#include "cxEditRepositoryItems.hpp"
#include "cxGraphics.hpp"
#include "cxInplaceContainer.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxStyles.hpp"
#include "cxVGrid.hpp"
#include <ActnList.hpp>
//---------------------------------------------------------------------------
enum TStyles {sBackground, sCategory, sContent, sHeader, sInactive, sIncSearch, sSelection};

class TStylesSimpleDemoStylesDialogForm : public TForm
{
__published:	// IDE-managed Components
  TcxButton *btnRestore;
  TcxVerticalGrid *cxVerticalGrid;
  TcxMultiEditorRow *cxVerticalGridCaption;
  TcxEditorRow *cxVerticalGridBackground;
  TcxEditorRow *cxVerticalGridCategory;
  TcxEditorRow *cxVerticalGridContent;
  TcxEditorRow *cxVerticalGridHeader;
  TcxEditorRow *cxVerticalGridInactive;
  TcxEditorRow *cxVerticalGridIncSearch;
  TcxEditorRow *cxVerticalGridSelection;
  TcxEditRepository *cxEditRepository;
  TcxEditRepositoryMRUItem *cxEditRepositoryMRUItem;
  TActionList *ActionList1;
  TcxStyleRepository *cxStyleRepository1;
  TcxStyle *cxStyle1;
  TcxStyle *cxStyle2;
  TcxStyle *cxStyle3;
  TcxStyle *cxStyle4;
  TcxStyle *cxStyle5;
  TcxStyle *cxStyle6;
  TcxStyle *cxStyle7;
  TcxVerticalGridStyleSheet *cxVerticalGridStyleSheetDevExpress;
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall OnButtonClick(TObject *Sender);
  void __fastcall btnRestoreClick(TObject *Sender);
  void __fastcall OnEditValueChanged(TObject *Sender);
  void __fastcall cxVerticalGridStylesGetContentStyle(TObject *Sender,
    TcxCustomEditorRowProperties *AEditProp, bool AFocused,
    int ARecordIndex, TcxStyle *&AStyle);
  void __fastcall cxVerticalGridStylesGetHeaderStyle(TObject *Sender,
    TcxCustomRow *ARow, TcxStyle *&AStyle);
private:	// User declarations
  TNotifyEvent FRestoreDefaults;
  TcxStyle* __fastcall GetSelectedStyle();
  void __fastcall RefreshBinding();
  TcxStyle* __fastcall GetCurrentStyle(TStyles AStyleID);
  void __fastcall SetCurrentStyle(TcxStyle *AStyle, TStyles AStyleID);
public:		// User declarations
  __fastcall TStylesSimpleDemoStylesDialogForm(TComponent* Owner);
  __property TNotifyEvent RestoreDefaults = {read=FRestoreDefaults, write=FRestoreDefaults};
};
//---------------------------------------------------------------------------
extern PACKAGE TStylesSimpleDemoStylesDialogForm *StylesSimpleDemoStylesDialogForm;
//---------------------------------------------------------------------------
#endif
