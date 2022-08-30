//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USERES("MasterDetailTableDemo.res");
USEFORM("MasterDetailTableDemoMain.cpp", MasterDetailTableDemoMainForm);
USEFORM("..\Common\FilmsDemoData.cpp", FilmsDemoDM); /* TDataModule: File Type */
USEFORM("..\BaseForm.cpp", fmBaseForm);
USEFORM("..\AboutDemoForm.cpp", formAboutDemo);
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
     Application->Initialize();
     Application->Title = "ExpressQuantumGrid MasterDetailTableDemo";
     Application->CreateForm(__classid(TFilmsDemoDM), &FilmsDemoDM);
     Application->CreateForm(__classid(TMasterDetailTableDemoMainForm), &MasterDetailTableDemoMainForm);
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
