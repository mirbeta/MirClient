//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USERES("EditorsLookupDemo.res");
USEFORM("EditorsLookupDemoMain.cpp", EditorsLookupDemoMainForm);
USEFORM("EditorsLookupDemoData.cpp", EditorsLookupDemoDataDM); /* TDataModule: File Type */
USEFORM("..\AboutDemoForm.cpp", formAboutDemo);
USEFORM("EditorsLookupDemoNewUser.cpp", EditorsLookupDemoNewUserForm);
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
     Application->Initialize();
     Application->Title = "ExpressQuantumGrid EditorsLookup Demo";
     Application->CreateForm(__classid(TEditorsLookupDemoDataDM), &EditorsLookupDemoDataDM);
     Application->CreateForm(__classid(TEditorsLookupDemoMainForm), &EditorsLookupDemoMainForm);
     Application->CreateForm(__classid(TEditorsLookupDemoNewUserForm), &EditorsLookupDemoNewUserForm);
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

