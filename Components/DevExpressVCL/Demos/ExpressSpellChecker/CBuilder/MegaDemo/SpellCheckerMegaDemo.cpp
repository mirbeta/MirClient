//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USEFORM("MegaDemoMain.cpp", fmMain);
USEFORM("AddDictionaryForm.cpp", fmAddDictionary);
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
			Application->Initialize();
			Application->CreateForm(__classid(TfmMain), &fmMain);
			Application->CreateForm(__classid(TfmAddDictionary), &fmAddDictionary);
			Application->Run();
        }
        catch (Exception &exception)
        {
			Application->ShowException(&exception);
        }
        return 0;
}
//---------------------------------------------------------------------------
