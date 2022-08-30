//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USERES("StylesSimpleDemo.res");
USEFORM("StylesSimpleDemoMain.cpp", StylesSimpleDemoMainForm);
USEFORM("StylesSimpleDemoData.cpp", StylesSimpleDemoDataDM); /* TDataModule: File Type */
USEFORM("..\Common\DemoRating.cpp", DemoRatingForm);
USEFORM("..\Common\AboutDemoForm.cpp", formAboutDemo);
USEFORM("..\Common\DemoBasicMain.cpp", DemoBasicMainForm);
USEFORM("StylesSimpleDemoEdit.cpp", StylesSimpleDemoEditForm);
USEFORM("StylesSimpleDemoStylesDialog.cpp", StylesSimpleDemoStylesDialogForm);
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
     Application->Initialize();
     Application->HelpFile = "..\\..\\Help\\ExpressQuantumTreeList.hlp";
     Application->Title = "ExpressQuantumTreeList StylesSimpleDemo";
     Application->CreateForm(__classid(TStylesSimpleDemoDataDM), &StylesSimpleDemoDataDM);
     Application->CreateForm(__classid(TStylesSimpleDemoMainForm), &StylesSimpleDemoMainForm);
     Application->CreateForm(__classid(TStylesSimpleDemoStylesDialogForm), &StylesSimpleDemoStylesDialogForm);
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
