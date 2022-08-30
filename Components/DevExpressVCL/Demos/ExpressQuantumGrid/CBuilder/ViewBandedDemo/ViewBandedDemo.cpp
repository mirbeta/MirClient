//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USERES("ViewBandedDemo.res");
USEFORM("ViewBandedDemoMain.cpp", ViewBandedDemoMainForm);
USEFORM("ViewBandedDemoData.cpp", ViewBandedDemoDataDM); /* TDataModule: File Type */
USEFORM("ViewBandeDemoBands.cpp", ViewBandeDemoBandsForm);
USEFORM("..\BaseForm.cpp", fmBaseForm);
USEFORM("..\AboutDemoForm.cpp", formAboutDemo);
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
     Application->Initialize();
     Application->Title = "ExpressQuantumGrid ViewBanded Demo";
     Application->CreateForm(__classid(TViewBandedDemoDataDM), &ViewBandedDemoDataDM);
     Application->CreateForm(__classid(TViewBandedDemoMainForm), &ViewBandedDemoMainForm);
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
