//---------------------------------------------------------------------------

#include <basepch.h>
#pragma hdrstop

#pragma link "gdiplus.lib"
#pragma link "wininet.lib"

USEFORMNS("AdvExplorerTreeviewStyles.pas", Advexplorertreeviewstyles, AdvExplorerTreeviewStyleForm);
USEFORMNS("GDIPPictureContainerEditor.pas", Gdippicturecontainereditor, GDIPPictureContainerEditor);
USEFORMNS("AdvExplorerTreeviewEditor.pas", Advexplorertreevieweditor, ExpTreeviewEditor);
USEFORMNS("AdvStyles.pas", Advstyles, AdvStyleForm);
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
 