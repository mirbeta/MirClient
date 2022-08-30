//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USERES("Office11GroupRowStyleDemo.res");
USEFORM("Office11GroupRowStyleDemoMain.cpp", Office11GroupRowStyleDemoMainForm);
USEFORM("..\BaseForm.cpp", fmBaseForm);
USEFORM("..\AboutDemoForm.cpp", formAboutDemo);
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
     Application->Initialize();
     Application->Title = "ExpressQuantumGrid Office11GroupRowStyle Demo";
     Application->CreateForm(__classid(TOffice11GroupRowStyleDemoMainForm), &Office11GroupRowStyleDemoMainForm);
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


