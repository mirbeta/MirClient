//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USERES("ColumnsMultiEditorsDemo.res");
USEFORM("ColumnsMultiEditorsDemoMain.cpp", ColumnsMultiEditorsDemoMainForm);
USEFORM("ColumnsMultiEditorsDemoData.cpp", ColumnsMultiEditorsDemoDataDM); /* TDataModule: File Type */
USEFORM("..\Common\DemoRating.cpp", DemoRatingForm);
USEFORM("..\Common\AboutDemoForm.cpp", formAboutDemo);
USEFORM("..\Common\DemoBasicMain.cpp", DemoBasicMainForm);
USEFORM("ColumnsMultiEditorsDemoPopup.cpp", ColumnsMultiEditorsDemoPopupForm);
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
     Application->Initialize();
     Application->HelpFile = "..\\..\\Help\\ExpressQuantumTreeList.hlp";
     Application->Title = "ExpressQuantumTreeList ColumnsMultiEditorsDemo";
     Application->CreateForm(__classid(TColumnsMultiEditorsDemoDataDM), &ColumnsMultiEditorsDemoDataDM);
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
