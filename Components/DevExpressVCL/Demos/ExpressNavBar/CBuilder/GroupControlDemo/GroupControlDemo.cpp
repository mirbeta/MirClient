//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("GroupControlDemo.res");
USEFORM("GroupControlMain.cpp", fmGroupControlMain);
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
                 Application->Initialize();
                 Application->CreateForm(__classid(TfmGroupControlMain), &fmGroupControlMain);
                 Application->Run();
        }
        catch (Exception &exception)
        {
                 Application->ShowException(&exception);
        }
        return 0;
}
//---------------------------------------------------------------------------
