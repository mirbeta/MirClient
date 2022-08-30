//---------------------------------------------------------------------------

#include <basepch.h>
#pragma hdrstop
#pragma link "wininet.lib"
#pragma link "msimg32.lib"
#pragma link "gdiplus.lib"

USEFORMNS("AdvExplorerTreeviewStyles.pas", Advexplorertreeviewstyles, AdvExplorerTreeviewStyleForm);
USEFORMNS("AdvStyles.pas", Advstyles, AdvStyleForm);
USEFORMNS("AdvExplorerTreeviewEditor.pas", Advexplorertreevieweditor, ExpTreeviewEditor);
USEFORMNS("AdvDBComboBoxListEditor.pas", Advdbcomboboxlisteditor, ComboListEditor);
//---------------------------------------------------------------------------
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
 