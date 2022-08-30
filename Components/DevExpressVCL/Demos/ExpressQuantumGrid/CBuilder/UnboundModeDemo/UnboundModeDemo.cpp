//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USERES("UnboundModeDemo.res");
USEFORM("UnboundModeDemoMain.cpp", UnboundModeDemoMainForm);
USEFORM("UnboundModeDemoCustomField.cpp", UnboundModeDemoCustomFieldForm);
USEFORM("UnboundModeDemoFastestSweepers.cpp", UnboundModeDemoFastestSweepersForm);
USEFORM("..\AboutDemoForm.cpp", formAboutDemo);
USEUNIT("UnboundModeDemoIntMinerField.cpp");
USEUNIT("UnboundModeDemoMinerCore.cpp");
USEUNIT("UnboundModeDemoMinerDataSource.cpp");
USEUNIT("UnboundModeDemoTypes.cpp");
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
     Application->Initialize();
     Application->Title = "ExpressQuantumGrid UnboundMode Demo";
     Application->CreateForm(__classid(TUnboundModeDemoMainForm), &UnboundModeDemoMainForm);
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
