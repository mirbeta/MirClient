//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
#include <tchar.h>
//---------------------------------------------------------------------------
USEFORM("..\Common\RibbonRichEditDemoOptions.cpp", RibbonDemoOptionsForm);
USEFORM("..\Common\RichEditControlBase.cpp", frmRichEditControlBase);
USEFORM("..\RibbonRichEditMainForm\RibbonRichEditDemoGallerySetup.cpp", ColorDialogSetupForm);
//USEFORM("..\RibbonRichEditMainForm\RibbonRichEditMainForm.cpp", frmRibbonRichEditMain);
USEFORM("RichEditSimpleMailMerge.cpp", frmRichEditSimpleMailMerge);
//---------------------------------------------------------------------------
int WINAPI _tWinMain(HINSTANCE, HINSTANCE, LPTSTR, int)
{
	try
	{
		Application->Title = "Rich Edit Simple Mail Merge Demo";
		Application->CreateForm(__classid(TfrmRichEditSimpleMailMerge), &frmRichEditSimpleMailMerge);
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
