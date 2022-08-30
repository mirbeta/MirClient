//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USERES("ViewCardDemo.res");
USEFORM("ViewCardDemoMain.cpp", ViewCardDemoMainForm);
USEFORM("ViewCardDemoData.cpp", ViewCardDemoDataDM); /* TDataModule: File Type */
USEFORM("..\AboutDemoForm.cpp", formAboutDemo);
USEFORM("..\BaseForm.cpp", fmBaseForm);
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
     Application->Initialize();
     Application->Title = "ExpressQuantumGrid ViewCard Demo";
     Application->CreateForm(__classid(TViewCardDemoDataDM), &ViewCardDemoDataDM);
	 Application->CreateForm(__classid(TViewCardDemoMainForm), &ViewCardDemoMainForm);
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


