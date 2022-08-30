//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USERES("MasterDetailMultiDemo.res");
USEFORM("..\AboutDemoForm.cpp", formAboutDemo);
USEFORM("..\BaseForm.cpp", fmBaseForm);
USEFORM("..\Common\FilmsDemoData.cpp", FilmsDemoDM);
USEFORM("MasterDetailMultiDemoMain.cpp", MasterDetailMultiDemoMainForm);
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
	 Application->Initialize();
	 Application->Title = "ExpressQuantumGrid MasterDetailMulti Demo ";
	 Application->CreateForm(__classid(TFilmsDemoDM), &FilmsDemoDM);
	 Application->CreateForm(__classid(TMasterDetailMultiDemoMainForm), &MasterDetailMultiDemoMainForm);
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
