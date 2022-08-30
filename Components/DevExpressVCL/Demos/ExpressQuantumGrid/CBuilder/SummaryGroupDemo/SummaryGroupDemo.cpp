//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USERES("SummaryGroupDemo.res");
USEFORM("SummaryGroupDemoMain.cpp", SummaryGroupDemoMainForm);
USEFORM("SummaryGroupDemoData.cpp", SummaryGroupDemoDataDM); /* TDataModule: File Type */
USEFORM("..\BaseForm.cpp", fmBaseForm);
USEFORM("..\AboutDemoForm.cpp", formAboutDemo);
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
     Application->Initialize();
     Application->Title = "ExpressQuantumGrid SummaryGroupDemo";
     Application->CreateForm(__classid(TSummaryGroupDemoDataDM), &SummaryGroupDemoDataDM);
     Application->CreateForm(__classid(TSummaryGroupDemoMainForm), &SummaryGroupDemoMainForm);
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
