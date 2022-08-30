//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("SampleDocking.res");
USEFORM("SampleDockingListBox.cpp", SampleDockingListBoxFrame);
USEFORM("SampleDockingMain.cpp", SampleDockingMainForm);
USEFORM("SampleDockingRadioGroup.cpp", SampleDockingRadioGroupFrame);
USEFORM("SampleDockingRichText.cpp", SampleDockingRichTextFrame);
USEFORM("SampleDockingTreeView.cpp", SampleDockingTreeViewFrame);
USEFORM("..\Common\EBarsUtils.cpp", dmCommonData);
USEFORM("..\Common\EBarsDemoRating.cpp", EBarsDemoRatingForm);
USEFORM("..\Common\AboutDemoForm.cpp", formAboutDemo);
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
     Application->Initialize();
     Application->Title = "ExpressBars SampleDocking Demo";
     Application->CreateForm(__classid(TdmCommonData), &dmCommonData);
		Application->CreateForm(__classid(TSampleDockingMainForm), &SampleDockingMainForm);
		Application->Run();
  }
  catch (Exception &exception)
  {
     Application->ShowException(&exception);
  }
  return 0;
}
//---------------------------------------------------------------------------
