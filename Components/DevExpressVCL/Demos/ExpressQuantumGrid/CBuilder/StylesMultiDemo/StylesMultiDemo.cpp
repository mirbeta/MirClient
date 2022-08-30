//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USERES("StylesMultiDemo.res");
USEFORM("StylesMultiDemoMain.cpp", StylesMultiDemoMainForm);
USEFORM("StylesMultiDemoData.cpp", StylesMultiDemoMainDM); /* TDataModule: File Type */
USEFORM("..\AboutDemoForm.cpp", formAboutDemo);
USEFORM("..\BaseForn.cpp", fmAboutDemo);
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
     Application->Initialize();
     Application->Title = "ExpressQuantumGrid StylesMulti Demo";
     Application->CreateForm(__classid(TStylesMultiDemoMainDM), &StylesMultiDemoMainDM);
     Application->CreateForm(__classid(TStylesMultiDemoMainForm), &StylesMultiDemoMainForm);
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

