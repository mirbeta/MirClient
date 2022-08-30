//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USEFORM("OrgChartRLMain.cpp", OrgChartRLMainForm);
USEFORM("..\Common\DemoBasicMain.cpp", DemoBasicMainForm);
USEFORM("..\Common\AboutDemoForm.cpp", formAboutDemo);
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
    try
    {
		Application->Initialize();
		Application->Title = "Report Links Demo - ExpressOrgChart & ExpressDBOrgChart";
        Application->CreateForm(__classid(TOrgChartRLMainForm), &OrgChartRLMainForm);
		Application->Run();
    }
    catch (Exception &exception)
    {
        Application->ShowException(&exception);
    }
    return 0;
}
//---------------------------------------------------------------------------
