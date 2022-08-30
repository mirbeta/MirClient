//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USEFORM("..\Common\DemoBasicMain.cpp", DemoBasicMainForm);
USEFORM("..\Common\AboutDemoForm.cpp", formAboutDemo);
USEFORM("FlowChartRLMain.cpp", FlowChartRLMainForm);
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
    try
    {
		Application->Initialize();
		Application->Title = "Report Link Demo - ExpressFlowChart";
        Application->CreateForm(__classid(TFlowChartRLMainForm), &FlowChartRLMainForm);
		Application->Run();
    }
    catch (Exception &exception)
	{
        Application->ShowException(&exception);
    }
    return 0;
}
//---------------------------------------------------------------------------
