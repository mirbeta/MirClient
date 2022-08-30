//---------------------------------------------------------------------------

#include <vcl.h>
#include "InPlaceEditorsDemoSplash.h"
#pragma hdrstop
USERES("InPlaceEditorsDemo.res");
USEFORM("..\Common\EBarsUtils.cpp", dmCommonData);
USEFORM("InPlaceEditorsDemoCheckBoxes.cpp", frmCheckBoxes);
USEFORM("..\Common\EBarsDemoRating.cpp", EBarsDemoRatingForm);
USEFORM("InPlaceEditorsDemoValue.cpp", frmValueEditors);
USEFORM("InPlaceEditorsDemoMultiLineText.cpp", frmMultiLineTextEditors);
USEFORM("InPlaceEditorsDemoComboBoxes.cpp", frmComboBoxes);
USEFORM("InPlaceEditorsDemoFrameManager.cpp", EditorDemoBaseFrame);
USEFORM("InPlaceEditorsDemoimage.cpp", frmImageEditors);
USEFORM("InPlaceEditorsDemoText.cpp", frmTextEditors);
USEFORM("InPlaceEditorsDemoSplash.cpp", frmLoading);
USEFORM("InPlaceEditorsDemoMain.cpp", frmMain);
USEFORM("..\Common\AboutDemoForm.cpp", formAboutDemo);
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
    Application->Initialize();
    Application->Title = "ExpressBars InPlaceEditorsDemo";
    frmLoading = new TfrmLoading(NULL);
    frmLoading->Show();
    frmLoading->Update();
    Application->CreateForm(__classid(TdmCommonData), &dmCommonData);
		Application->CreateForm(__classid(TfrmMain), &frmMain);
		delete frmLoading;
    Application->Run();
  }
  catch (Exception &exception)
  {
    Application->ShowException(&exception);
  }
  return 0;
}
//---------------------------------------------------------------------------
