//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "AlertWindowDemoMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxContainer"
#pragma link "cxControls"
#pragma link "cxEdit"
#pragma link "cxGraphics"
#pragma link "cxListView"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma link "dxAlertWindow"
#pragma resource "*.dfm"
TAlertWindowDemoForm *AlertWindowDemoForm;
//---------------------------------------------------------------------------
__fastcall TAlertWindowDemoForm::TAlertWindowDemoForm(TComponent* Owner)
	: TfmBaseForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TAlertWindowDemoForm::FormCreate(TObject *Sender)
{
  if (IsWinVistaOrLater)
	lvContactList->SmallImages = ilContactList_7;
  else
	lvContactList->SmallImages = ilContactList_XP;
  Randomize;
}
//---------------------------------------------------------------------------

TdxAlertWindow* TAlertWindowDemoForm::FindWindowByCaption(const String ACaption)
{
  int I;
  TdxAlertWindow *AAlertWindow;
  AAlertWindow = NULL;
  for (I = 0; I<= dxAlertWindowManager1->Count - 1; I++)
  {
	AAlertWindow = dxAlertWindowManager1->Items[I];
	if (IsWindowForMessagesGroup(AAlertWindow) && (AAlertWindow->MessageList->Items[0]->Caption == ACaption))
	  break;
	else
	  AAlertWindow = NULL;
  }
  return(AAlertWindow);
}
//---------------------------------------------------------------------------

String TAlertWindowDemoForm::FormatMessageText(int AMessageNumber, int AContactIndex)
{
  const String FormatTextMessage = "Message #%d from %s.\nNote: Every new message from %s will be displayed in this window. Use navigation buttons below to browse the message queue.";
  String ACaption;
  ACaption = lvContactList->Items->Item[AContactIndex]->Caption;
  TVarRec V[] = {AMessageNumber, ACaption, ACaption};
  return(Format(FormatTextMessage, V, 3));
}
//---------------------------------------------------------------------------

void TAlertWindowDemoForm::InitializeGenerateMessagesTimer(int ACount, Cardinal AInterval)
{
  FContactMessagesToGenerate = ACount;
  tmGenerateMessage->Interval = AInterval;
  tmGenerateMessage->Enabled = True;
}
//---------------------------------------------------------------------------

TdxAlertWindowMessage* TAlertWindowDemoForm::GetCurrentMessage(TdxAlertWindow* AAlertWindow)
{
  return(AAlertWindow->MessageList->Items[AAlertWindow->CurrentMessageIndex]);
}
//---------------------------------------------------------------------------

Boolean TAlertWindowDemoForm::IsWindowForMessagesGroup(TdxAlertWindow* AAlertWindow)
{
  return((AAlertWindow != NULL) && (AAlertWindow->Tag == 1) &&
	(AAlertWindow->VisibilityTransition != awvtHiding));
}
//---------------------------------------------------------------------------

void __fastcall TAlertWindowDemoForm::miOptionsAlertWindowClick(TObject *Sender)
{
  FormOptions->LoadWindowOptions(dxAlertWindowManager1);
  FormOptions->Show();
}
//---------------------------------------------------------------------------

void TAlertWindowDemoForm::ShowNewMessage(const String AMessageText, int AContactIndex)
{
  String ACaption;
  TdxAlertWindow* AAlertWindow;
  ACaption = lvContactList->Items->Item[AContactIndex]->Caption;
  AAlertWindow = FindWindowByCaption(ACaption);
  if (AAlertWindow == NULL)
	dxAlertWindowManager1->Show(ACaption, AMessageText, AContactIndex)->Tag = 1;
  else
  {
	AAlertWindow->MessageList->Add(ACaption, AMessageText, AContactIndex);
	AAlertWindow->RestartDisplayTimer();
  }
}
//---------------------------------------------------------------------------

void TAlertWindowDemoForm::NewMessage(int AContactIndex)
{
  if (lvContactList->Items->Count >= 0)
  {
	FContactMessageCounters[AContactIndex]++;
	ShowNewMessage(FormatMessageText(FContactMessageCounters[AContactIndex],
	  AContactIndex), AContactIndex);
  }
}
//---------------------------------------------------------------------------

void __fastcall TAlertWindowDemoForm::pmCopyMessageClick(TObject *Sender)
{
  Clipboard()->AsText = GetCurrentMessage((TdxAlertWindow*)pmAlertWindow->PopupComponent)->Text;
}
//---------------------------------------------------------------------------

void __fastcall TAlertWindowDemoForm::pmDeleteMessageClick(TObject *Sender)
{
  ((TdxAlertWindow*)pmAlertWindow->PopupComponent)->DeleteCurrentMessage();
}
//---------------------------------------------------------------------------


void __fastcall TAlertWindowDemoForm::tmGenerateMessageTimer(TObject *Sender)
{
  FContactMessagesToGenerate--;
  NewMessage(RandomRange(0, lvContactList->Items->Count));
  if (FContactMessagesToGenerate == 0)
  {
	tmGenerateMessage->Enabled = False;
	InitializeGenerateMessagesTimer(1, RandomRange(5000, 10000));
  }
}
//---------------------------------------------------------------------------

void __fastcall TAlertWindowDemoForm::pmAlertWindowPopup(TObject *Sender)
{
  pmUnqueueMessage->Enabled = (pmAlertWindow->PopupComponent->Tag == 1) &&
    (((TdxAlertWindow*)pmAlertWindow->PopupComponent)->MessageList->Count > 1);
}
//---------------------------------------------------------------------------

void __fastcall TAlertWindowDemoForm::lvContactListDblClick(TObject *Sender)
{
  if (lvContactList->ItemIndex > -1)
    NewMessage(lvContactList->ItemIndex);
}
//---------------------------------------------------------------------------


void __fastcall TAlertWindowDemoForm::pmUnqueueMessageClick(TObject *Sender)
{
  TdxAlertWindow* AAlertWindow;
  TdxAlertWindowMessage* AMessage;
  AnsiString S;
  AAlertWindow = (TdxAlertWindow*)pmAlertWindow->PopupComponent;
  AMessage = GetCurrentMessage(AAlertWindow);
  S = AMessage->Text;
  S = S.SubString(1, S.Pos("\n"));
  dxAlertWindowManager1->Show(AMessage->Caption, S, AMessage->ImageIndex);
  AAlertWindow->DeleteCurrentMessage();
}
//---------------------------------------------------------------------------

void __fastcall TAlertWindowDemoForm::dxAlertWindowManager1ButtonClick(TObject *Sender,
          TdxAlertWindow *AAlertWindow, int AButtonIndex)
{
  if (AButtonIndex == 0){
	AnsiString AMailTo = "mailto:" +
	  VarToStr(AAlertWindow->MessageList->Items[AAlertWindow->CurrentMessageIndex]->Caption);
	AMailTo = AnsiReplaceStr(AMailTo, " ", "%20");
	ShellExecute(Handle, "OPEN", AMailTo.c_str(), NULL, NULL, SW_SHOWMAXIMIZED);
  }
  else
	if (AButtonIndex == 1)
	  ShowMessage("Clicked!");
}
//---------------------------------------------------------------------------


void __fastcall TAlertWindowDemoForm::FormShow(TObject *Sender)
{
  FContactMessageCounters = new int[lvContactList->Items->Count];
  memset(FContactMessageCounters, 0, lvContactList->Items->Count * sizeof(int));
  InitializeGenerateMessagesTimer(2, 1000);
}
//---------------------------------------------------------------------------

void __fastcall TAlertWindowDemoForm::dxAlertWindowManager1Initialize(TObject *Sender,
		  TdxAlertWindow *AAlertWindow)
{
  const String FormatTextMessage = "%s\nFirst message sent: %s";
  TVarRec V[] = {AAlertWindow->MessageList->Items[0]->Text, DateTimeToStr(Now())};
  AAlertWindow->MessageList->Items[0]->Text = Format(FormatTextMessage, V, 2);
}
//---------------------------------------------------------------------------

void __fastcall TAlertWindowDemoForm::FormDestroy(TObject *Sender)
{
  delete(FContactMessageCounters);
}
//---------------------------------------------------------------------------

