//---------------------------------------------------------------------------

#include <basepch.h>
#pragma hdrstop
USEFORMNS("hotspoteditor.pas", Hotspoteditor, frmHSIEditor);
//---------------------------------------------------------------------------
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
