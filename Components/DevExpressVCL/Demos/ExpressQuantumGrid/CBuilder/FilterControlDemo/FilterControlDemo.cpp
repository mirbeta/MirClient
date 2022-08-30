//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("FilterControlDemo.res");
USEFORM("..\Common\CarsData.cpp", dmCars);
USEFORM("..\Common\CarsDataForGrid.cpp", dmGridCars);
USEFORM("FilterControlDemoMain.cpp", FilterControlDemoMainForm);
USEFORM("FilterControlDemoFilterDialog.cpp", FilterControlDemoFilterDialogForm);
USEFORM("..\BaseForm.cpp", fmBaseForm);
USEFORM("..\AboutDemoForm.cpp", formAboutDemo);
USEFORM("FilterControlDemoData.cpp", FilterControlDemoDataDM); /* TDataModule: DesignClass */
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
     Application->Initialize();
     Application->Title = "ExpressQuantumGrid FilterControlDemo";
	 Application->CreateForm(__classid(TdmGridCars), &dmGridCars);
     Application->CreateForm(__classid(TFilterControlDemoDataDM), &FilterControlDemoDataDM);
     Application->CreateForm(__classid(TFilterControlDemoMainForm), &FilterControlDemoMainForm);
     Application->CreateForm(__classid(TFilterControlDemoFilterDialogForm), &FilterControlDemoFilterDialogForm);
     Application->Run();
  }
  catch (Exception &exception)
  {
     Application->ShowException(&exception);
  }
  return 0;
}
//---------------------------------------------------------------------------
