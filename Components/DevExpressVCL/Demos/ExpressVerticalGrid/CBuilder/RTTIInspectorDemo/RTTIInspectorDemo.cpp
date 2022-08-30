//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USERES("RTTIInspectorDemo.res");
USEUNIT("RTTIInspectorDemoPropEditors.cpp");
USEFORM("RTTIInspectorDemoMain.cpp", RTTIInspectorDemoMainForm);
USEFORM("RTTIInspectorDemoData.cpp", RTTIInspectorDemoMainDM); /* TDataModule: File Type */
USEFORM("CarsData.cpp", dmCars); /* TDataModule: File Type */
USEFORM("..\Common\DemoRating.cpp", DemoRatingForm);
USEFORM("..\Common\DemoBasicAbout.cpp", DemoBasicAboutForm);
USEFORM("..\Common\DemoBasicMain.cpp", DemoBasicMainForm);
USEFORM("RTTIInspectorDemoVGEditor.cpp", cxVerticalGridEditor);
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
     Application->Initialize();
     Application->HelpFile = "";
     Application->Title = "ExpressVerticalGrid RTTIInspector Demo";
	 Application->CreateForm(__classid(TdmCars), &dmCars);
     Application->CreateForm(__classid(TRTTIInspectorDemoMainDM), &RTTIInspectorDemoMainDM);
     Application->CreateForm(__classid(TRTTIInspectorDemoMainForm), &RTTIInspectorDemoMainForm);
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
