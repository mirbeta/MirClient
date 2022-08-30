//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USEFORM("..\Common\DemoBasicDM.cpp", dmOrders);
USEFORM("..\Common\DemoBasicMain.cpp", frmDemoBasicMain);
USEFORM("..\Common\AboutDemoForm.cpp", formAboutDemo);
USEFORM("PrefilterByCodeDemoMain.cpp", fmPrefilterByCode);
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
		Application->Initialize();
                Application->CreateForm(__classid(TdmOrders), &dmOrders);
		Application->CreateForm(__classid(TfmPrefilterByCode), &fmPrefilterByCode);
		Application->Run();
        }
        catch (Exception &exception)
        {
		Application->ShowException(&exception);
        }
        return 0;
}
//---------------------------------------------------------------------------
