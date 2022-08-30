//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USERES("ViewCardSimpleDemo.res");
USEFORM("ViewCardSimpleDemoMain.cpp", ViewCardSimpleDemoMainForm);
USEFORM("ViewCardSimpleDemoData.cpp", ViewCardSimpleDemoMainDM); /* TDataModule: File Type */
USEFORM("..\BaseForm.cpp", fmBaseForm);
USEFORM("..\AboutDemoForm.cpp", formAboutDemo);
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
	 Application->Initialize();
	 Application->Title = "ExpressQuantumGrid ViewCardSimple Demo";
	 Application->CreateForm(__classid(TViewCardSimpleDemoMainDM), &ViewCardSimpleDemoMainDM);
	 Application->CreateForm(__classid(TViewCardSimpleDemoMainForm), &ViewCardSimpleDemoMainForm);
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
