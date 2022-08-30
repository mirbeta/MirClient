//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USERES("SummaryFooterDemo.res");
USEFORM("..\Common\CarsData.cpp", dmCars);
USEFORM("..\Common\CarsDataForGrid.cpp", dmGridCars);
USEFORM("SummaryFooterDemoMain.cpp", SummaryFooterDemoMainForm);
USEFORM("SummaryFooterDemoData.cpp", SummaryFooterDemoDataDM); /* TDataModule: File Type */
USEFORM("..\AboutDemoForm.cpp", formAboutDemo);
USEFORM("..\BaseForm.cpp", fmBaseForm);
USEFORM("SummaryFooterDemoEditSummary.cpp", SummaryFooterDemoEditSummaryForm);
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
     Application->Initialize();
     Application->Title = "ExpressQuantumGrid SummaryFooter Demo";
	 Application->CreateForm(__classid(TdmGridCars), &dmGridCars);
     Application->CreateForm(__classid(TSummaryFooterDemoDataDM), &SummaryFooterDemoDataDM);
     Application->CreateForm(__classid(TSummaryFooterDemoMainForm), &SummaryFooterDemoMainForm);
     Application->CreateForm(__classid(TSummaryFooterDemoEditSummaryForm), &SummaryFooterDemoEditSummaryForm);
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
