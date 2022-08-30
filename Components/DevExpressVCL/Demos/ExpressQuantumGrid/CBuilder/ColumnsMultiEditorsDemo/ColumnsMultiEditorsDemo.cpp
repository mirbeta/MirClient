//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USERES("ColumnsMultiEditorsDemo.res");
USEFORM("ColumnsMultiEditorsDemoMain.cpp", ColumnsMultiEditorsDemoMainForm);
USEFORM("..\AboutDemoForm.cpp", formAboutDemo);
USEUNIT("ColumnsMultiEditorsDemoDS.cpp");
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
     Application->Initialize();
     Application->Title = "ExpressQuantumGrid ColumnsMultiEditors Demo";
     Application->CreateForm(__classid(TColumnsMultiEditorsDemoMainForm), &ColumnsMultiEditorsDemoMainForm);
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
