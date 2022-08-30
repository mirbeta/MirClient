//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USERES("CustomDrawDemo.res");
USEUNIT("CustomDrawDemoUtils.cpp");
USEFORM("CustomDrawDemoMain.cpp", CustomDrawDemoMainForm);
USEFORM("..\Common\DemoRating.cpp", DemoRatingForm);
USEFORM("..\Common\DemoBasicAbout.cpp", DemoBasicAboutForm);
USEFORM("..\Common\DemoBasicMain.cpp", DemoBasicMainForm);
USEFORM("CustomDrawDemoEditor.cpp", CustomDrawDemoEditorForm);
USEFORM("CustomDrawDemoData.cpp", CustomDrawDemoDataDM); /* TDataModule: File Type */
USERC("CustomDrawDemoImages.rc");
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
     Application->Initialize();
     Application->Title = "ExpressVerticalGrid CustomDraw Demo";
     Application->CreateForm(__classid(TCustomDrawDemoDataDM), &CustomDrawDemoDataDM);
     Application->CreateForm(__classid(TCustomDrawDemoMainForm), &CustomDrawDemoMainForm);
     Application->CreateForm(__classid(TCustomDrawDemoEditorForm), &CustomDrawDemoEditorForm);
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
