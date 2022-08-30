//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USEFORM("..\Common\AboutDemoForm.cpp", formAboutDemo);
USEFORM("..\Common\DemoBasicMain.cpp", frmDemoBasicMain);
USEFORM("OLAPBrowserMain.cpp", frmOlapBrowser);
USERES("OLAPBrowser.res");
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
                 Application->Initialize();
                 Application->CreateForm(__classid(TfrmOlapBrowser), &frmOlapBrowser);
                 Application->Run();
        }
        catch (Exception &exception)
        {
                 Application->ShowException(&exception);
        }
        return 0;
}
//---------------------------------------------------------------------------
