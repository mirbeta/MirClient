//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USERES("SimpleVerticalGridDemo.res");
USEFORM("..\Common\CarsData.cpp", dmCars);
USEFORM("SimpleVerticalGridDemoMain.cpp", SimpleVerticalGridDemoMainForm);
USEFORM("SimpleVerticalGridDemoData.cpp", SimpleVerticalGridDemoMainDM); /* TDataModule: File Type */
USEFORM("..\Common\DemoRating.cpp", DemoRatingForm);
USEFORM("..\Common\DemoBasicAbout.cpp", DemoBasicAboutForm);
USEFORM("..\Common\DemoBasicMain.cpp", DemoBasicMainForm);
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
     Application->Initialize();
     Application->HelpFile = "";
     Application->Title = "ExpressVerticalGrid SimpleVerticalGrid Demo";
	 Application->CreateForm(__classid(TdmCars), &dmCars);
     Application->CreateForm(__classid(TSimpleVerticalGridDemoMainDM), &SimpleVerticalGridDemoMainDM);
     Application->CreateForm(__classid(TSimpleVerticalGridDemoMainForm), &SimpleVerticalGridDemoMainForm);
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
