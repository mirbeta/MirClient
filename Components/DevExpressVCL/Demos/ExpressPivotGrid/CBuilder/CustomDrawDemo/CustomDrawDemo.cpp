//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("..\CustomDrawDemo\CustomDrawDemo.res");
USEFORM("..\Common\AboutDemoForm.cpp", formAboutDemo);
USEFORM("..\Common\DemoBasicDM.cpp", dmOrders); /* TDataModule: File Type */
USEFORM("..\Common\DemoBasicMain.cpp", frmDemoBasicMain);
USEFORM("CustomDrawMain.cpp", frmCustomDraw);
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
                 Application->Initialize();
                 Application->CreateForm(__classid(TdmOrders), &dmOrders);
                 Application->CreateForm(__classid(TfrmCustomDraw), &frmCustomDraw);
                 Application->Run();
        }
        catch (Exception &exception)
        {
                 Application->ShowException(&exception);
        }
        return 0;
}
//---------------------------------------------------------------------------
