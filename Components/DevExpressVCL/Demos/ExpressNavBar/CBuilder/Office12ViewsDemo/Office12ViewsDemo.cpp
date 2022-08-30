//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("Office12ViewsDemo.res");
USEFORM("Office12ViewsMain.cpp", fmMain);
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
                 Application->Initialize();
                 Application->CreateForm(__classid(TfmMain), &fmMain);
                 Application->Run();
        }
        catch (Exception &exception)
        {
                 Application->ShowException(&exception);
        }
        return 0;
}
//---------------------------------------------------------------------------
