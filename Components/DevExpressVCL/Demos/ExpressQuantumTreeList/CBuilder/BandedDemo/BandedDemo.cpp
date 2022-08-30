//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USERES("BandedDemo.res");
USEFORM("BandedDemoMain.cpp", BandedDemoMainForm);
USEFORM("BandedDemoData.cpp", BandedDemoDataDM); /* TDataModule: File Type */
USEFORM("..\Common\DemoRating.cpp", DemoRatingForm);
USEFORM("..\Common\AboutDemoForm.cpp", formAboutDemo);
USEFORM("..\Common\DemoBasicMain.cpp", DemoBasicMainForm);
USEFORM("BandedDemoBands.cpp", BandedDemoBandsForm);
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
     Application->Initialize();
     Application->HelpFile = "..\\..\\Help\\ExpressQuantumTreeList.hlp";
     Application->Title = "ExpressQuantumTreeList BandedDemo";
     Application->CreateForm(__classid(TBandedDemoDataDM), &BandedDemoDataDM);
     Application->CreateForm(__classid(TBandedDemoMainForm), &BandedDemoMainForm);
     Application->CreateForm(__classid(TBandedDemoBandsForm), &BandedDemoBandsForm);
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
