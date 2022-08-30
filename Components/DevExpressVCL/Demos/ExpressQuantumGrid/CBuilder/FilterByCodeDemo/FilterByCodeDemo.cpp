//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USERES("FilterByCodeDemo.res");
USEFORM("FilterByCodeDemoMain.cpp", FilterByCodeDemoMainForm);
USEFORM("FilterByCodeDemoData.cpp", FilterByCodeDemoMainDM);
USEFORM("..\BaseForm.cpp", fmBaseForm);
USEFORM("..\AboutDemoForm.cpp", formAboutDemo);
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
     Application->Initialize();
     Application->Title = "ExpressQuantumGrid FilterByCode Demo";
     Application->CreateForm(__classid(TFilterByCodeDemoMainDM), &FilterByCodeDemoMainDM);
     Application->CreateForm(__classid(TFilterByCodeDemoMainForm), &FilterByCodeDemoMainForm);
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
