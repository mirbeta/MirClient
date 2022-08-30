//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("..\DataSetsDemo\DataSetsDemo.res");
USEFORM("..\Common\AboutDemoForm.cpp", formAboutDemo);
USEFORM("..\Common\DemoBasicDM.cpp", dmOrders); /* TDataModule: File Type */
USEFORM("..\Common\DemoBasicMain.cpp", frmDemoBasicMain);
USEFORM("DataSetsMain.cpp", frmDataSets);
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
                 Application->Initialize();
                 Application->CreateForm(__classid(TdmOrders), &dmOrders);
                 Application->CreateForm(__classid(TfrmDataSets), &frmDataSets);
                 Application->Run();
        }
        catch (Exception &exception)
        {
                 Application->ShowException(&exception);
        }
        return 0;
}
//---------------------------------------------------------------------------
