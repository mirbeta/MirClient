//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USERES("FilterDropDownDemo.res");
USEFORM("FilterDropDownDemoMain.cpp", frmMain);
USEFORM("..\Common\CarsData.cpp", dmCars);
USEFORM("..\Common\DemoRating.cpp", DemoRatingForm);
USEFORM("..\AboutDemoForm.cpp", formAboutDemo);
USEFORM("..\Common\DemoBasicMain.cpp", DemoBasicMainForm);
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
				 Application->Initialize();
				 Application->Title = "ExpressQuantumTreeList Filter Drop Down Demo";
				 Application->CreateForm(__classid(TdmCars), &dmCars);
				 Application->CreateForm(__classid(TfrmMain), &frmMain);
				 Application->Run();
		}
        catch (Exception &exception)
        {
                 Application->ShowException(&exception);
        }
        catch (...)
        {
                 try
                 {
                         throw Exception("");
                 }
                 catch (Exception &exception)
                 {
                         Application->ShowException(&exception);
                 }
        }
        return 0;
}
//---------------------------------------------------------------------------
