//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USERES("GridModeDemo.res");
USEFORM("GridModeDemoMain.cpp", GridModeDemoMainForm);
USEFORM("GridModeDemoData.cpp", GridModeDemoDataDM); /* TDataModule: File Type */
USEFORM("..\BaseForm.cpp", fmBaseForm);
USEFORM("..\AboutDemoForm.cpp", formAboutDemo);
USEFORM("GridModeDemoTerminate.cpp",GridModeDemoTerminateForm);
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
     Application->Initialize();
     Application->Title = "ExpressQuantumGrid GridMode Demo";
     Application->CreateForm(__classid(TGridModeDemoDataDM), &GridModeDemoDataDM);
     Application->CreateForm(__classid(TGridModeDemoMainForm), &GridModeDemoMainForm);
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
