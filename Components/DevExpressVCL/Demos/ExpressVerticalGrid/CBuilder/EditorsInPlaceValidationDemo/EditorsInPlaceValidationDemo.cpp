//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USEFORM("..\Common\DemoBasicMain.cpp", DemoBasicMainForm);
USEFORM("..\Common\DemoBasicAbout.cpp", DemoBasicAboutForm);
USEFORM("..\Common\DemoRating.cpp", DemoRatingForm);
USEFORM("EditorsInPlaceValidationDemoData.cpp", EditorsInPlaceValidationDemoDataDM);
USEFORM("EditorsInPlaceValidationDemoMain.cpp", EditorsInPlaceValidationDemoMainForm);
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
     Application->Initialize();
     Application->HelpFile = "";
     Application->Title = "ExpressVerticalGrid EditorsInPlaceValidation Demo";
     Application->CreateForm(__classid(TEditorsInPlaceValidationDemoDataDM), &EditorsInPlaceValidationDemoDataDM);
		Application->CreateForm(__classid(TEditorsInPlaceValidationDemoMainForm), &EditorsInPlaceValidationDemoMainForm);
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
