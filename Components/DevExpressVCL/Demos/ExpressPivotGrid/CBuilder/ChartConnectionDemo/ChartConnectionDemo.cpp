//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("ChartConnectionDemo.res");
USEFORM("..\Common\DemoBasicDM.cpp", dmOrders);
USEFORM("..\Common\DemoBasicMain.cpp", frmDemoBasicMain);
USEFORM("..\Common\AboutDemoForm.cpp", formAboutDemo);
USEFORM("ChartConnectionDemoMain.cpp", frmChartConnection);
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
			Application->Initialize();
	                Application->CreateForm(__classid(TdmOrders), &dmOrders);
			Application->CreateForm(__classid(TfrmChartConnection), &frmChartConnection);
			Application->Run();
        }
        catch (Exception &exception)
        {
                 Application->ShowException(&exception);
        }
        return 0;
}
//---------------------------------------------------------------------------
