//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("AlphaBlendingDemo.res");
USEFORM("AlphaBlendingMain.cpp", fmAlphaBlendingMain);
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
                 Application->Initialize();
                 Application->CreateForm(__classid(TfmAlphaBlendingMain), &fmAlphaBlendingMain);
                 Application->Run();
        }
        catch (Exception &exception)
        {
                 Application->ShowException(&exception);
        }
        return 0;
}
//---------------------------------------------------------------------------
