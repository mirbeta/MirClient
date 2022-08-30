//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USEFORM("..\Common\DemoBasicMain.cpp", DemoBasicMainForm);
USEFORM("..\Common\AboutDemoForm.cpp", formAboutDemo);
USEFORM("..\Common\DemoRating.cpp", DemoRatingForm);
USEFORM("EditorsInPlaceValidationDemoMain.cpp", EditorsInPlaceValidationDemoMainForm);
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
     Application->Initialize();
     Application->HelpFile = "..\\..\\Help\\ExpressQuantumTreeList.hlp";
     Application->Title = "ExpressQuantumTreeList EditorsInPlaceValidation Demo";
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
