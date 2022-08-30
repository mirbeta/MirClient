//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("FeaturesDemo.res");
USEFORM("FeaturesMain.cpp", fmFeaturesMain);
USEFORM("..\Common\NavBarUtils.cpp", dmCommonData);
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
                 Application->Initialize();
                 Application->CreateForm(__classid(TdmCommonData), &dmCommonData);
                 Application->CreateForm(__classid(TfmFeaturesMain), &fmFeaturesMain);
                 Application->Run();
        }
        catch (Exception &exception)
        {
                 Application->ShowException(&exception);
        }
        return 0;
}
//---------------------------------------------------------------------------
