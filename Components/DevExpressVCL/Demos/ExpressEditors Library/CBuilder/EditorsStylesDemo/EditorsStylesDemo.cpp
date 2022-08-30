//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USERES("EditorsStylesDemo.res");
USEFORM("EditorsStylesDemoMain.cpp", EditorsStylesDemoDemoMainForm);
USEFORM("EditorsStylesDemoBase.cpp", EditorsStylesDemoBaseFrame);
USEFORM("EditorsStylesDemoPlanets.cpp", EditorsStylesDemoPlanetsFrame);
USEFORM("..\BaseForm.cpp", fmBaseForm);
USEFORM("..\AboutDemoForm.cpp", formAboutDemo);
USEFORM("EditorsStylesDemoNoteBook.cpp", EditorsStylesDemoNoteBookFrame);
USEFORM("EditorsStylesDemoIssues.cpp", EditorsStylesDemoIssuesFrame);
USEFORM("EditorsStylesDemoRichEdit.cpp", EditorsStylesDemoRichEditFrame);
USEFORM("EditorsStylesDemoConvert.cpp", EditorsStylesDemoConvertFrame);
USEFORM("EditorsStylesDemoStylesPalette.cpp", EditorsStylesDemoStylesPaletteFrame);
USEFORM("EditorsStylesDemoData.cpp", EditorsStylesDemoDataDM); /* TDataModule: DesignClass */
USEUNIT("EditorsStylesDemoFrameControl.cpp");
USEUNIT("EditorsStylesDemoUtils.cpp");
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
     Application->Initialize();
     Application->HelpFile = "..\\..\\Help\\CXEXTEDITORS.HLP";
                 Application->Title = "ExpressEditors Library (Extended Component Features)";
                 Application->CreateForm(__classid(TEditorsStylesDemoDemoMainForm), &EditorsStylesDemoDemoMainForm);
                 Application->CreateForm(__classid(TEditorsStylesDemoDataDM), &EditorsStylesDemoDataDM);
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
