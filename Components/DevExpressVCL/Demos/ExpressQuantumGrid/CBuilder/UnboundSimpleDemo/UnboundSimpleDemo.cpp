//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USERES("UnboundSimpleDemo.res");
USEFORM("UnboundSimpleDemoMain.cpp", UnboundSimpleDemoMainForm);
USEFORM("..\BaseForm.cpp", fmBaseForm);
USEFORM("..\AboutDemoForm.cpp", formAboutDemo);
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
     Application->Initialize();
     Application->Title = "ExpressQuantumGrid UnboundSimpleDemo Demo";
     Application->CreateForm(__classid(TUnboundSimpleDemoMainForm), &UnboundSimpleDemoMainForm);
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
