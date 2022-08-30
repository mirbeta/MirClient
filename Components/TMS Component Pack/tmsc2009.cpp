//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USEFORMNS("AdvReplaceDialogForm.pas", Advreplacedialogform, ReplaceDialogForm);
USEFORMNS("AdvFindDialogForm.pas", Advfinddialogform, FindDialogForm);
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
