//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USERES("StylesCardViewDemo.res");
USEFORM("StylesCardViewDemoMain.cpp", StylesCardViewDemoMainForm);
USEFORM("StylesCardViewDemoData.cpp", StylesCardViewDemoMainDM); /* TDataModule: File Type */
USEFORM("..\BaseForm.cpp", fmBaseForm);
USEFORM("..\AboutDemoForm.cpp", formAboutDemo);
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
     Application->Initialize();
     Application->Title = "ExpressQuantumGrid StylesCardView Demo";
     Application->CreateForm(__classid(TStylesCardViewDemoMainDM), &StylesCardViewDemoMainDM);
     Application->CreateForm(__classid(TStylesCardViewDemoMainForm), &StylesCardViewDemoMainForm);
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


