//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USERES("StylesMultiDemo.res");
USEFORM("StylesMultiDemoMain.cpp", StylesMultiDemoMainForm);
USEFORM("StylesMultiDemoData.cpp", StylesMultiDemoDataDM); /* TDataModule: File Type */
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
     Application->Title = "ExpressVerticalGrid StylesMulti Demo";
     Application->CreateForm(__classid(TStylesMultiDemoDataDM), &StylesMultiDemoDataDM);
     Application->CreateForm(__classid(TStylesMultiDemoMainForm), &StylesMultiDemoMainForm);
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
