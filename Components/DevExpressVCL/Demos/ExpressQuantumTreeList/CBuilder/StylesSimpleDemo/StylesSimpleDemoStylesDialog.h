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
#include "cxCustomData.hpp"
#include "cxGraphics.hpp"
#include "cxInplaceContainer.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxMRUEdit.hpp"
#include "cxStyles.hpp"
#include "cxTextEdit.hpp"
#include "cxTL.hpp"
#include "cxCheckBox.hpp"
#include "cxCurrencyEdit.hpp"
#include "cxDBLookupComboBox.hpp"
#include "cxMaskEdit.hpp"
//---------------------------------------------------------------------------
enum TStyles {sBackground, sBandbackground, sBandContent, sBandHeader, sColumnFooter,
  sColumnHeader, sContent, sContentEven,  sContentOdd, sFooter, sInactive, sIncSearch,
  sIndicator, sPreview, sSelection};

class TStylesSimpleDemoStylesDialogForm : public TForm
{
__published:	// IDE-managed Components
  TLabel *lscrip;
  TcxTreeList *cxTreeList;
  TcxTreeListColumn *tlcStyle;
  TcxTreeListColumn *tlcStyleNames;
  TcxButton *btnRestore;
  TcxStyleRepository *cxStyleRepository1;
  TcxStyle *cxStyle1;
  TcxStyle *cxStyle2;
  TcxStyle *cxStyle3;
  TcxStyle *cxStyle4;
  TcxStyle *cxStyle5;
  TcxStyle *cxStyle6;
  TcxStyle *cxStyle7;
  TcxStyle *cxStyle8;
  TcxStyle *cxStyle9;
  TcxStyle *cxStyle10;
  TcxStyle *cxStyle11;
  TcxStyle *cxStyle12;
  TcxStyle *cxStyle13;
  TcxTreeListStyleSheet *TreeListStyleSheetDevExpress;
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall tlcStyleNamesPropertiesButtonClick(TObject *Sender);
  void __fastcall btnRestoreClick(TObject *Sender);
  void __fastcall tlcStyleNamesPropertiesEditValueChanged(TObject *Sender);
private:
  TNotifyEvent FRestoreDefaults;
  TcxStyle* __fastcall GetSelectedStyle();
  void __fastcall RefreshBinding();
  TcxStyle* __fastcall GetCurrentStyle(TStyles AStyleID);
  void __fastcall SetCurrentStyle(TcxStyle *AStyle, TStyles AStyleID);
public:
  __property TNotifyEvent RestoreDefaults = {read=FRestoreDefaults, write=FRestoreDefaults};
  __fastcall TStylesSimpleDemoStylesDialogForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TStylesSimpleDemoStylesDialogForm *StylesSimpleDemoStylesDialogForm;
//---------------------------------------------------------------------------
#endif
