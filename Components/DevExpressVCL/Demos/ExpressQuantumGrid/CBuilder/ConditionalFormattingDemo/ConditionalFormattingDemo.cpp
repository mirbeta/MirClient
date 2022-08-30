//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USEFORM("ConditionalFormattingDemoData.cpp", ConditionalFormattingDemoMainDM);
USEFORM("ConditionalFormattingDemoMain.cpp", ConditionalFormattingDemoMainForm);
USEFORM("..\AboutDemoForm.cpp", formAboutDemo);
USEFORM("..\BaseForm.cpp", fmBaseForm);
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
     Application->Initialize();
     Application->Title = "ExpressQuantumGrid ConditionalFormatting Demo";
     Application->CreateForm(__classid(TConditionalFormattingDemoMainDM), &ConditionalFormattingDemoMainDM);
	 Application->CreateForm(__classid(TConditionalFormattingDemoMainForm), &ConditionalFormattingDemoMainForm);
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
