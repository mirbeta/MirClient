//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USERES("UnboundColumnsDemo.res");
USEFORM("UnboundColumnsDemoMain.cpp", UnboundColumnsDemoMainForm);
USEFORM("UnboundColumnsDemoData.cpp", UnboundColumnsDemoDataDM); /* TDataModule: File Type */
USEFORM("..\BaseForm.cpp", fmBaseForm);
USEFORM("..\AboutDemoForm.cpp", formAboutDemo);
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
     Application->Initialize();
     Application->Title = "ExpressQuantumGrid UnboundColumns Demo";
     Application->CreateForm(__classid(TUnboundColumnsDemoDataDM), &UnboundColumnsDemoDataDM);
     Application->CreateForm(__classid(TUnboundColumnsDemoMainForm), &UnboundColumnsDemoMainForm);
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


