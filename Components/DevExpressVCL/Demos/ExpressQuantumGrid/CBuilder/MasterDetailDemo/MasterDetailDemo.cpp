//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USERES("MasterDetailDemo.res");
USEFORM("MasterDetailDemoMain.cpp", MasterDetailDemoMainForm);
USEFORM("..\Common\FilmsDemoData.cpp", FilmsDemoDM); /* TDataModule: File Type */
USEFORM("..\BaseForm.cpp", fmBaseForm);
USEFORM("..\AboutDemoForm.cpp", formAboutDemo);
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
     Application->Initialize();
     Application->Title = "ExpressQuantumGrid MasterDetailDemo";
     Application->CreateForm(__classid(TFilmsDemoDM), &FilmsDemoDM);
     Application->CreateForm(__classid(TMasterDetailDemoMainForm), &MasterDetailDemoMainForm);
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
