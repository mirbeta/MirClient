//---------------------------------------------------------------------------

#ifndef AlertWindowDemoMainH
#define AlertWindowDemoMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "BaseForm.h"
#include "dxCore.hpp"
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxEdit.hpp"
#include "cxGraphics.hpp"
#include "cxListView.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "dxAlertWindow.hpp"
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
#include <ImgList.hpp>
#include <StrUtils.hpp>
#include <Menus.hpp>
#include "AlertWindowDemoOptions.h";

//---------------------------------------------------------------------------
class TAlertWindowDemoForm : public TfmBaseForm
{
__published:	// IDE-managed Components
	TcxImageList *ilMessages;
	TcxImageList *ilToolBar;
	TcxImageList *ilContactList_XP;
	TcxImageList *ilContactList_7;
	TcxListView *lvContactList;
	TBevel *bvSpacerLeft;
	TBevel *bvSpacerRight;
	TBevel *bvSpacerBottom;
	TdxAlertWindowManager *dxAlertWindowManager1;
	TPopupMenu *pmAlertWindow;
	TMenuItem *pmUnqueueMessage;
	TMenuItem *pmDeleteMessage;
	TMenuItem *pmCopyMessage;
	TTimer *tmGenerateMessage;
	void __fastcall miOptionsAlertWindowClick(TObject *Sender);
	void __fastcall tmGenerateMessageTimer(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall pmDeleteMessageClick(TObject *Sender);
	void __fastcall pmCopyMessageClick(TObject *Sender);
	void __fastcall pmAlertWindowPopup(TObject *Sender);
	void __fastcall lvContactListDblClick(TObject *Sender);
	void __fastcall pmUnqueueMessageClick(TObject *Sender);
	void __fastcall dxAlertWindowManager1ButtonClick(TObject *Sender, TdxAlertWindow *AAlertWindow,
          int AButtonIndex);
	void __fastcall FormShow(TObject *Sender);
	void __fastcall dxAlertWindowManager1Initialize(TObject *Sender, TdxAlertWindow *AAlertWindow);
	void __fastcall FormDestroy(TObject *Sender);

private:	// User declarations
	int FContactMessagesToGenerate;
	int *FContactMessageCounters;

	void NewMessage(int AContactIndex);
	TdxAlertWindow* FindWindowByCaption(const String ACaption);
	String FormatMessageText(int AMessageNumber, int AContactIndex);
	Boolean IsWindowForMessagesGroup(TdxAlertWindow* AAlertWindow);
	TdxAlertWindowMessage* GetCurrentMessage(TdxAlertWindow* AAlertWindow);
	void InitializeGenerateMessagesTimer(int ACount, Cardinal AInterval);
	void ShowNewMessage(const String AMessageText, int AContactIndex);
public:		// User declarations
	__fastcall TAlertWindowDemoForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TAlertWindowDemoForm *AlertWindowDemoForm;
//---------------------------------------------------------------------------
#endif
