//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "Splash.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
__fastcall TfmSplash::TfmSplash(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfmSplash::FormCreate(TObject *Sender)
{
  const String CRLF = "\n";
  const String WarningText =
    "   If you do not own appropriate Control Libraries from Developer Express Inc., part of Reports " + CRLF +
    "cannot be used, because last ones are needed specific ReportItems for rendering from these Libraries." + CRLF +
    "   Mentioned above ReportItems are the part of ReportLinks for these Libraries." + CRLF +
     CRLF + CRLF +
    "   In case you own them, just uncomment directives placed top of Main.cpp and Main.h according " + CRLF +
    "to the following explanation:" +
	 CRLF +
     CRLF +
    "     - dxPScxSSLnk for ExpressSpreadSheet" + CRLF +
	"     - dxPSdxLC2Lnk for ExpressLayoutControl" + CRLF +
	"     - dxPScxCommon for ExpressEditors" + CRLF +
	"       (also needed for ExpressQuntumGrid, ExpressQuntumTree and ExpressVerticalGrid)" + CRLF +
    "     - dxPSExtCommon for ExpressExtendedEditors";
            
  Caption = Application->Title;
  lblText->Caption = WarningText;
}
//---------------------------------------------------------------------------
