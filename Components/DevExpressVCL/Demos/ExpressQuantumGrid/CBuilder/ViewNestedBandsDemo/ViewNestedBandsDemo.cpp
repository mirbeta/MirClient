//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USERES("ViewNestedBandsDemo.res");
USEFORM("..\Common\CarsData.cpp", dmCars);
USEFORM("..\Common\CarsDataForGrid.cpp", dmGridCars);
USEFORM("ViewNestedBandsDemoMain.cpp", ViewNestedBandsDemoMainForm);
USEFORM("ViewNestedBandsDemoData.cpp", ViewNestedBandsDemoDataDM); /* TDataModule: File Type */
USEFORM("..\AboutDemoForm.cpp", formAboutDemo);
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
     Application->Initialize();
     Application->Title = "ExpressQuantumGrid ViewNestedBands Demo";
	 Application->CreateForm(__classid(TdmGridCars), &dmGridCars);
     Application->CreateForm(__classid(TViewNestedBandsDemoDataDM), &ViewNestedBandsDemoDataDM);
     Application->CreateForm(__classid(TViewNestedBandsDemoMainForm), &ViewNestedBandsDemoMainForm);
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


