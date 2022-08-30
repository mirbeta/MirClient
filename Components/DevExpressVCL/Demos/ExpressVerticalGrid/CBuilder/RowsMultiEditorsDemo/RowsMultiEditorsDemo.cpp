//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USERES("RowsMultiEditorsDemo.res");
USEFORM("RowsMultiEditorsDemoMain.cpp", RowsMultiEditorsDemoMainForm);
USEFORM("RowsMultiEditorsDemoData.cpp", RowsMultiEditorsDemoDataDM); /* TDataModule: File Type */
USEFORM("..\Common\DemoRating.cpp", DemoRatingForm);
USEFORM("..\Common\DemoBasicAbout.cpp", DemoBasicAboutForm);
USEFORM("..\Common\DemoBasicMain.cpp", DemoBasicMainForm);
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
     Application->Initialize();
     Application->HelpFile = "";
     Application->Title = "ExpressVerticalGrid RowsMultiEditors Demo";
     Application->CreateForm(__classid(TRowsMultiEditorsDemoDataDM), &RowsMultiEditorsDemoDataDM);
     Application->CreateForm(__classid(TRowsMultiEditorsDemoMainForm), &RowsMultiEditorsDemoMainForm);
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
