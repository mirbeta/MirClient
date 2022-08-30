//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Shellapi.h"
#include "EBarsDemoRating.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TEBarsDemoRatingForm *EBarsDemoRatingForm;

const String OurEmail = "Support@devexpress.com";
const String EmailSubj = "EBars.Demos.";

//---------------------------------------------------------------------------
__fastcall TEBarsDemoRatingForm::TEBarsDemoRatingForm(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TEBarsDemoRatingForm::rgRateChange(TObject *Sender)
{
  if (rgRate->ItemIndex  != -1)
    btnSend->Enabled = true;
}
//---------------------------------------------------------------------------

void __fastcall TEBarsDemoRatingForm::AdjustMessageBody(String &ABody, String ASearchStr, String AReplaceStr)
{

  int APos = ABody.Pos(ASearchStr);
  while (APos != 0) {
    ABody.Delete(APos, ASearchStr.Length());
    ABody.Insert(AReplaceStr, APos);
    APos = ABody.Pos(ASearchStr);
  }
}
//---------------------------------------------------------------------------

void __fastcall TEBarsDemoRatingForm::btnSendClick(TObject *Sender)
{
  Screen->Cursor = crHourGlass;
  try {
    String ABody, ASubj;
    ASubj = EmailSubj + ChangeFileExt(ExtractFileName(Application->ExeName),"")+"-user rating";
    ABody = "Rate: " + IntToStr(rgRate->ItemIndex + 1);
    if (memRateDescrip->Text != "")
      ABody = ABody+"\r\n"+"\r\n"+"Description:"+ "\r\n" + memRateDescrip->Text;
    AdjustMessageBody(ABody, "%", "$prc$");
    AdjustMessageBody(ABody, "$prc$", "%25");
    AdjustMessageBody(ABody, "\r\n", "%0D%0A");
    AdjustMessageBody(ABody, "&", "%26");
    AdjustMessageBody(ABody, " ", "%20");
    AnsiString s = "mailto:" + OurEmail + "?subject=" + ASubj + "&body=" + ABody;
    ShellExecute(Handle, "OPEN", s.c_str(), NULL, NULL, SW_SHOWMAXIMIZED);
  }
  __finally {
    Screen->Cursor = crDefault;
    Close();
  }
}
//---------------------------------------------------------------------------
