//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USERES("CustomDrawTableViewDemo.res");
USEFORM("..\Common\CarsData.cpp", dmCars);
USEFORM("..\Common\CarsDataForGrid.cpp", dmGridCars);
USEFORM("CustomDrawTableViewDemoMain.cpp", CustomDrawTableViewDemoMainForm);
USEFORM("CustomDrawTableViewDemoData.cpp", CustomDrawTableViewDemoMainDM); /* TDataModule: File Type */
USEFORM("..\AboutDemoForm.cpp", formAboutDemo);
USEFORM("CustomDrawTableViewDemoStylesEditor.cpp", CustomDrawTableViewDemoStylesEditorForm);
USEUNIT("CustomDrawTableViewDemoTypes.cpp");
USERC("CustomDrawTableViewDemoImages.rc"); 
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
     Application->Initialize();
     Application->Title = "ExpressQuantumGrid CustomDrawTableView Demo";
	 Application->CreateForm(__classid(TdmGridCars), &dmGridCars);
     Application->CreateForm(__classid(TCustomDrawTableViewDemoMainForm), &CustomDrawTableViewDemoMainForm);
     Application->CreateForm(__classid(TCustomDrawTableViewDemoMainDM), &CustomDrawTableViewDemoMainDM);
     Application->CreateForm(__classid(TCustomDrawTableViewDemoStylesEditorForm), &CustomDrawTableViewDemoStylesEditorForm);
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
