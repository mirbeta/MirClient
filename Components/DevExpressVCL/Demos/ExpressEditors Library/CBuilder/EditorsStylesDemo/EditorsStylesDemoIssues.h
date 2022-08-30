//---------------------------------------------------------------------------

#ifndef EditorsStylesDemoIssuesH
#define EditorsStylesDemoIssuesH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxEdit.hpp"
#include "cxMemo.hpp"
#include "cxPropertiesStore.hpp"
#include "cxTextEdit.hpp"
#include "EditorsStylesDemoBase.h"
#include <ExtCtrls.hpp>
#include <Menus.hpp>
#include "cxCheckComboBox.hpp"
#include "cxCheckListBox.hpp"
#include "cxClasses.hpp"
#include "cxDBCheckComboBox.hpp"
#include "cxDBCheckListBox.hpp"
#include "cxDBEdit.hpp"
#include "cxDBLabel.hpp"
#include "cxDBLookupComboBox.hpp"
#include "cxDBLookupEdit.hpp"
#include "cxDBProgressBar.hpp"
#include "cxDBTrackBar.hpp"
#include "cxDropDownEdit.hpp"
#include "cxGroupBox.hpp"
#include "cxLabel.hpp"
#include "cxLookupEdit.hpp"
#include "cxMaskEdit.hpp"
#include "cxNavigator.hpp"
#include "cxProgressBar.hpp"
#include "cxSpinEdit.hpp"
#include "cxStyles.hpp"
#include "cxTrackBar.hpp"
#include <DB.hpp>
#include "cxDBNavigator.hpp"
//---------------------------------------------------------------------------
class TEditorsStylesDemoIssuesFrame : public TEditorsStylesDemoBaseFrame
{
__published:	// IDE-managed Components
  TPanel *pnlNotification;
  TcxLabel *lbNotification;
  TcxDBCheckListBox *chlbUsers;
  TPanel *Panel2;
  TcxDBLabel *lblDBIssueID;
  TcxDBCheckComboBox *chcbIDEs;
  TcxLabel *lbProject;
  TcxLabel *lblIssueID;
  TcxDBLookupComboBox *edProject;
  TcxLabel *lbCreator;
  TcxLabel *lbOwner;
  TcxDBLookupComboBox *edCreator;
  TcxDBLookupComboBox *edOwner;
  TcxGroupBox *gbStatus;
  TcxDBTrackBar *tbStatus;
  TcxLabel *lbNew;
  TcxLabel *lbPostponed;
  TcxLabel *lbFixed;
  TcxLabel *lbRejected;
  TcxGroupBox *gbProgress;
  TcxDBProgressBar *pgbProgress;
  TcxDBSpinEdit *seProgress;
  TcxLabel *lbCheckProgress;
  TcxLabel *lbProgress;
  TcxDBSpinEdit *seCheckProgress;
  TcxLabel *cxLabel1;
  TcxDBSpinEdit *seFirstTarget;
  TcxLabel *lbIDEs;
  TcxDBNavigator *cxDBNavigator1;
  TPanel *Panel1;
  TcxLabel *lbIssue;
  TcxDBTextEdit *edIssue;
  void __fastcall seFirstTargetPropertiesChange(TObject *Sender);
  void __fastcall seProgressPropertiesChange(TObject *Sender);
  void __fastcall seCheckProgressPropertiesChange(TObject *Sender);
        void __fastcall FormShow(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TEditorsStylesDemoIssuesFrame(TComponent* Owner);
  void __fastcall FillUsersCheckListBox();
  String __fastcall Name();
  String __fastcall BriefName();
  String StylesIniPath();
  String Description();
};
//---------------------------------------------------------------------------
extern PACKAGE TEditorsStylesDemoIssuesFrame *EditorsStylesDemoIssuesFrame;
//---------------------------------------------------------------------------
#endif
