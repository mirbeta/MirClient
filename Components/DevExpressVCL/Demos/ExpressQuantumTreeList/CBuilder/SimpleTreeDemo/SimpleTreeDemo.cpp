//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USERES("SimpleTreeDemo.res");
USEFORM("SimpleTreeDemoMain.cpp", SimpleTreeDemoMainForm);
USEFORM("SimpleTreeDemoData.cpp", SimpleTreeDemoDataDM); /* TDataModule: File Type */
USEFORM("..\Common\DemoRating.cpp", DemoRatingForm);
USEFORM("..\Common\AboutDemoForm.cpp", formAboutDemo);
USEFORM("..\Common\DemoBasicMain.cpp", DemoBasicMainForm);
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
     Application->Initialize();
     Application->HelpFile = "..\\..\\Help\\ExpressQuantumTreeList.hlp";
     Application->Title = "ExpressQuantumTreeList SimpleTreeDemo";
     Application->CreateForm(__classid(TSimpleTreeDemoDataDM), &SimpleTreeDemoDataDM);
     Application->CreateForm(__classid(TSimpleTreeDemoMainForm), &SimpleTreeDemoMainForm);
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
