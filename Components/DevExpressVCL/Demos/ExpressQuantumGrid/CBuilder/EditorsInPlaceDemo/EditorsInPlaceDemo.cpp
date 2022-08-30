//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USERES("EditorsInPlaceDemo.res");
USEFORM("..\Common\CarsData.cpp", dmCars);
USEFORM("..\Common\CarsDataForGrid.cpp", dmGridCars);
USEFORM("EditorsInPlaceDemoMain.cpp", EditorsInPlaceDemoMainForm);
USEFORM("EditorsInPlaceDemoData.cpp", EditorsInPlaceDemoDataDM); /* TDataModule: File Type */
USEFORM("..\AboutDemoForm.cpp", formAboutDemo);
USEFORM("EditorsInPlaceDemoCars.cpp", EditorsInPlaceDemoCarsForm);
USEFORM("EditorsInPlaceDemoCities.cpp", EditorsInPlaceDemoCitiesForm);
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
     Application->Initialize();
     Application->Title = "ExpressQuantumGrid EditorsInPlace Demo ";
	 Application->CreateForm(__classid(TdmGridCars), &dmGridCars);
     Application->CreateForm(__classid(TEditorsInPlaceDemoDataDM), &EditorsInPlaceDemoDataDM);
     Application->CreateForm(__classid(TEditorsInPlaceDemoMainForm), &EditorsInPlaceDemoMainForm);
     Application->CreateForm(__classid(TEditorsInPlaceDemoCarsForm), &EditorsInPlaceDemoCarsForm);
     Application->CreateForm(__classid(TEditorsInPlaceDemoCitiesForm), &EditorsInPlaceDemoCitiesForm);
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
