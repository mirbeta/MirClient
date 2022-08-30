//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USERES("EditorsMaskDemo.res");
USEFORM("EditorsMaskDemoMain.cpp", EditorsMaskDemoMainForm);
USEFORM("EditorsMaskDemoData.cpp", EditorsMaskDemoMainDM); /* TDataModule: File Type */
USEFORM("..\BaseForm.cpp", fmBaseForm);
USEFORM("..\AboutDemoForm.cpp", formAboutDemo);
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
     Application->Initialize();
     Application->Title = "ExpressQuantumGrid EditorsMaskDemo";
     Application->CreateForm(__classid(TEditorsMaskDemoMainDM), &EditorsMaskDemoMainDM);
     Application->CreateForm(__classid(TEditorsMaskDemoMainForm), &EditorsMaskDemoMainForm);
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
