//---------------------------------------------------------------------------
#include <vcl.h>
#include "Splash.h"
#pragma hdrstop
USERES("DBBasedExplorer.res");
USEFORM("main.cpp", fmMain);
USEFORM("Splash.cpp", fmSplash);
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
          Application->Initialize();
          Application->Title = "ExpressPrinting System DB Report Explorer";
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
