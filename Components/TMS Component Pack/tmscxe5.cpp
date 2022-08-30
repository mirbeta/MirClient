//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
















USEFORMNS("AdvFindDialogForm.pas", Advfinddialogform, FindDialogForm);
USEFORMNS("AdvReplaceDialogForm.pas", Advreplacedialogform, ReplaceDialogForm);
//---------------------------------------------------------------------------
#pragma link "wininet.lib"
#pragma link "msimg32.lib"
#pragma link "gdiplus.lib"
#pragma package(smart_init)
//---------------------------------------------------------------------------

//   Package source.
//---------------------------------------------------------------------------

#pragma argsused
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
        return 1;
}
//---------------------------------------------------------------------------
