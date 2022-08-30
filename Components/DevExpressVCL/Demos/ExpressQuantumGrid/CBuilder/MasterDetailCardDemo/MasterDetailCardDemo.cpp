//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USERES("MasterDetailCardDemo.res");
USEFORM("MasterDetailCardDemoMain.cpp", MasterDetailCardDemoMainForm);
USEFORM("..\Common\FilterDemoData.cpp", FilmsDemoDM); /* TDataModule: File Type */
USEFORM("..\BaseForm.cpp", fmBaseForm);
USEFORM("..\AboutDemoForm.cpp", formAboutDemo);
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
     Application->Initialize();
     Application->Title = "ExpressQuantumGrid MasterDetailCardDemo";
     Application->CreateForm(__classid(TFilmsDemoDM), &FilmsDemoDM);
     Application->CreateForm(__classid(TMasterDetailCardDemoMainForm), &MasterDetailCardDemoMainForm);
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
