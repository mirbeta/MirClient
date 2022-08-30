//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USEFORM("..\Common\AboutDemoForm.cpp", formAboutDemo);
USEFORM("..\Common\DemoBasicMain.cpp", frmDemoBasicMain);
USEFORM("UnboundExternalMain.cpp", frmUnboundExternal);
USERES("UnboundExternalDemo.res");
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
                 Application->Initialize();
                 Application->CreateForm(__classid(TfrmUnboundExternal), &frmUnboundExternal);
                 Application->Run();
        }
        catch (Exception &exception)
        {
                 Application->ShowException(&exception);
        }
        return 0;
}
//---------------------------------------------------------------------------
