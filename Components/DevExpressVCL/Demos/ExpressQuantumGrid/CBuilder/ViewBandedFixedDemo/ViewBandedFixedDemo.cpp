//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USERES("ViewBandedFixedDemo.res");
USEFORM("ViewBandedFixedDemoData.cpp", ViewBandedFixedDemoDMMain); /* TDataModule: File Type */
USEFORM("ViewBandedFixedMain.cpp", ViewBandedFixedDemoMainForm);
USEFORM("..\BaseForm.cpp", fmBaseForm);
USEFORM("..\AboutDemoForm.cpp", formAboutDemo);
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
     Application->Initialize();
     Application->Title = "ExpressQuantumGrid ViewBandedFixed Demo";
     Application->CreateForm(__classid(TViewBandedFixedDemoDMMain), &ViewBandedFixedDemoDMMain);
     Application->CreateForm(__classid(TViewBandedFixedDemoMainForm), &ViewBandedFixedDemoMainForm);
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
