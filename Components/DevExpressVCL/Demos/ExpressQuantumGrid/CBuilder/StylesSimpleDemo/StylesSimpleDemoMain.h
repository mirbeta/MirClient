//---------------------------------------------------------------------------

#ifndef StylesSimpleDemoMainH
#define StylesSimpleDemoMainH
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
#include "cxGrid.hpp"
#include "cxGridCustomTableView.hpp"
#include "cxGridCustomView.hpp"
#include "cxGridDBTableView.hpp"
#include "cxGridLevel.hpp"
#include "cxGridTableView.hpp"
#include "cxStyles.hpp"
#include <ActnList.hpp>
#include <ComCtrls.hpp>
#include <DB.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
#include "cxGridCustomPopupMenu.hpp"
#include "cxGridPopupMenu.hpp"
#include "cxListBox.hpp"
#include <Buttons.hpp>
#include <ExtCtrls.hpp>
#include <ToolWin.hpp>
#include "cxContainer.hpp"
#include "cxLookAndFeels.hpp"
#include "BaseForm.h"
#include "cxButtons.hpp"
#include "cxCalendar.hpp"
#include "cxDataStorage.hpp"
#include "cxDBLookupComboBox.hpp"
#include "cxGridCardView.hpp"
#include "cxGroupBox.hpp"
#include "cxLookAndFeelPainters.hpp"
//---------------------------------------------------------------------------
class TStylesSimpleDemoMainForm : public TfmBaseForm
{
__published:  // IDE-managed Components
  TSplitter *Splitter;
  TcxGrid *cxGrid;
  TcxGridDBTableView *tvPersons;
  TcxGridDBColumn *tvPersonsID;
  TcxGridDBColumn *tvPersonsFIRSTNAME;
  TcxGridDBColumn *tvPersonsSECONDNAME;
  TcxGridDBColumn *tvPersonsBIRTHNAME;
  TcxGridDBColumn *tvPersonsDATEOFBIRTH;
  TcxGridDBColumn *tvPersonsBIRTHCOUNTRY;
  TcxGridDBColumn *tvPersonsLOCATIONOFBIRTH;
  TcxGridDBColumn *tvPersonsBIOGRAPHY;
  TcxGridLevel *lvPersons;
	TcxGroupBox *gbChangeStyles;
  TMemo *memHowTo;
  TcxListBox *ListBox;
  TPanel *pnlButtons;
	TcxButton *btnEdit;
	TcxButton *btnView;
  TcxGridPopupMenu *cxGridPopupMenu;
  TActionList *alMain;
  TAction *actHeader;
  TAction *actFooter;
  TAction *actIndicator;
  TAction *actGroupBox;
  TAction *actPreview;
  TMenuItem *miOptions;
  TMenuItem *miHeader;
  TMenuItem *miFooter;
  TMenuItem *miIndicator;
  TMenuItem *miGroupByBox;
  TMenuItem *miPreview;
  TImageList *ilMain;
  void __fastcall actHeaderExecute(TObject *Sender);
  void __fastcall actFooterExecute(TObject *Sender);
  void __fastcall actIndicatorExecute(TObject *Sender);
  void __fastcall actGroupBoxExecute(TObject *Sender);
  void __fastcall actPreviewExecute(TObject *Sender);
  void __fastcall btnEditClick(TObject *Sender);
  void __fastcall btnViewClick(TObject *Sender);
  void __fastcall FormCreate(TObject *Sender);
private:  // User declarations
  TcxStyle* GetSelectedStyle();
  void __fastcall RestoreDefaults(TObject *Sender);
  bool ChangeStyle(TcxStyle *AStyle);
  bool ChangeStyleBinding(TNotifyEvent ACallback);
public:   // User declarations
  __fastcall TStylesSimpleDemoMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TStylesSimpleDemoMainForm *StylesSimpleDemoMainForm;
//---------------------------------------------------------------------------
#endif
