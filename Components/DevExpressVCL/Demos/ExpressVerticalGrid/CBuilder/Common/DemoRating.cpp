//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "DemoRating.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxControls"
#pragma link "cxClasses"
#pragma link "cxLookAndFeels"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxButtons"
#pragma link "cxEdit"
#pragma link "cxMemo"
#pragma link "cxRadioGroup"
#pragma link "cxTextEdit"
#pragma link "cxGroupBox"
#pragma resource "*.dfm"
TDemoRatingForm *DemoRatingForm;

const
	AnsiString OurEmail = "Support@devexpress.com";
	AnsiString EmailSubj = "cxVerticalGrid.Demos.";
 
//---------------------------------------------------------------------------
__fastcall TDemoRatingForm::TDemoRatingForm(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TDemoRatingForm::rgRateChange(TObject *Sender)
{
  if (rgRate->ItemIndex  != -1)
    btnSend->Enabled = true;
}
//---------------------------------------------------------------------------


void __fastcall TDemoRatingForm::btnSendClick(TObject *Sender)
{
  Screen->Cursor = crHourGlass;
  try {
    String ABody, ASubj;
    ASubj = EmailSubj + ChangeFileExt(ExtractFileName(Application->ExeName),"")+"-user%20rating";
    ABody = "Rate: " + IntToStr(rgRate->ItemIndex + 1);
    if (memRateDescrip->Text != "")
      ABody = ABody+"\r\n"+"\r\n"+"Description:"+ "\r\n" + memRateDescrip->Text;
    AdjustMessageBody(ABody, "%", "$prc$");
    AdjustMessageBody(ABody, "$prc$", "%25");
    AdjustMessageBody(ABody, "\r\n", "%0D%0A");
    AdjustMessageBody(ABody, "&", "%26");
    AdjustMessageBody(ABody, " ", "%20");
    AnsiString s = "mailto:" + OurEmail + "?subject="+ASubj+"&body="+ABody;
    ShellExecute(Handle, "OPEN", s.c_str(), NULL, NULL, SW_SHOWMAXIMIZED);
  }
  __finally {
    Screen->Cursor = crDefault;
    Close();
  }
}
//---------------------------------------------------------------------------

void __fastcall TDemoRatingForm::AdjustMessageBody(String &ABody, String ASearchStr,
  String AReplaceStr)
{
  int APos = ABody.Pos(ASearchStr);
  while (APos != 0) {
    ABody.Delete(APos, ASearchStr.Length());
    ABody.Insert(AReplaceStr, APos);
    APos = ABody.Pos(ASearchStr);
  }
}
//---------------------------------------------------------------------------


