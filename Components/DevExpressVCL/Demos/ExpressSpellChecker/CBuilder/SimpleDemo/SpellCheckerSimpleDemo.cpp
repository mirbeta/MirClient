//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("SpellCheckerSimpleDemo.res");
USEFORM("SimpeDemoMain.cpp", fmCV);
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
                 Application->Initialize();
                 Application->CreateForm(__classid(TfmCV), &fmCV);
                 Application->Run();
        }
        catch (Exception &exception)
        {
                 Application->ShowException(&exception);
        }
        return 0;
}
//---------------------------------------------------------------------------
