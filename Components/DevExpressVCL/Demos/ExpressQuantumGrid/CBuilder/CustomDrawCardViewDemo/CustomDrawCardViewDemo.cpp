//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USERES("CustomDrawCardViewDemo.res");
USEFORM("CustomDrawCardViewDemoMain.cpp", CustomDrawCardViewDemoMainForm);
USEFORM("..\Common\FilmsDemoData.cpp", FilmsDemoDM); /* TDataModule: File Type */
USEFORM("..\AboutDemoForm.cpp", formAboutDemo);
USEFORM("..\BaseForm.cpp", fmBaseForm);
USEUNIT("CustomDrawCardViewDemoTypes.cpp");
USERC("CustomDrawCardViewDemoImages.rc"); 
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
	 Application->Initialize();
     Application->Title = "ExpressQuantumGrid CustomDrawCardView Demo";
	 Application->CreateForm(__classid(TFilmsDemoDM), &FilmsDemoDM);
     Application->CreateForm(__classid(TCustomDrawCardViewDemoMainForm), &CustomDrawCardViewDemoMainForm);
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
