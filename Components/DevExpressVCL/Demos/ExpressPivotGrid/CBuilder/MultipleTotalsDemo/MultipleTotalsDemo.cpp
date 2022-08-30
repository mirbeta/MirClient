//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("MultipleTotalsDemo.res");
USEFORM("..\Common\AboutDemoForm.cpp", formAboutDemo);
USEFORM("..\Common\DemoBasicDM.cpp", dmOrders); /* TDataModule: File Type */
USEFORM("..\Common\DemoBasicMain.cpp", frmDemoBasicMain);
USEFORM("MultipleTotalsMain.cpp", frmMultipleTotals);
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
                 Application->Initialize();
                 Application->CreateForm(__classid(TdmOrders), &dmOrders);
                 Application->CreateForm(__classid(TfrmMultipleTotals), &frmMultipleTotals);
                 Application->Run();
        }
        catch (Exception &exception)
        {
                 Application->ShowException(&exception);
        }
        return 0;
}
//---------------------------------------------------------------------------
