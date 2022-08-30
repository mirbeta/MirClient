//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USEFORM("DockingMegaDemo.cpp", fmMain);
USEFORM("DockingMegaDemoMain.cpp", DockingMegaDemoMainForm);
USEFORM("..\Common\EBarsUtils.cpp", dmCommonData);
USEFORM("..\Common\EBarsDemoRating.cpp", EBarsDemoRatingForm);
USEFORM("..\Common\AboutDemoForm.cpp", formAboutDemo);
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
     Application->Initialize();
     Application->Title = "ExpressBars DockingMega Demo";
     Application->CreateForm(__classid(TdmCommonData), &dmCommonData);
		Application->CreateForm(__classid(TDockingMegaDemoMainForm), &DockingMegaDemoMainForm);
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
