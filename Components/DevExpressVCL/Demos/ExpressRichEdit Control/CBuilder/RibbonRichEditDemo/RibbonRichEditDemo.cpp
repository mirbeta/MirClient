//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
#include <tchar.h>
//---------------------------------------------------------------------------
USEFORM("..\Common\AboutDemoForm.cpp", formAboutDemo);
USEFORM("..\Common\EBarsDemoRating.cpp", EBarsDemoRatingForm);
USEFORM("..\Common\EBarsUtils.cpp", dmCommonData);
USEFORM("..\Common\RibbonRichEditDemoOptions.cpp", RibbonDemoOptionsForm);
USEFORM("..\Common\RichEditControlBase.cpp", frmRichEditControlBase);
USEFORM("..\RibbonRichEditMainForm\RibbonRichEditDemoGallerySetup.cpp", ColorDialogSetupForm);
USEFORM("..\RibbonRichEditMainForm\RibbonRichEditMainForm.cpp", frmRibbonRichEditMain);
USEFORM("RibbonRichEditForm.cpp", frmRibbonRichEditForm);
//---------------------------------------------------------------------------
int WINAPI _tWinMain(HINSTANCE, HINSTANCE, LPTSTR, int)
{
	try
	{
		Application->Title = "Rich Edit Demo";
		Application->CreateForm(__classid(TdmCommonData), &dmCommonData);
		Application->CreateForm(__classid(TfrmRibbonRichEditForm), &frmRibbonRichEditForm);
		Application->CreateForm(__classid(TColorDialogSetupForm), &ColorDialogSetupForm);
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
