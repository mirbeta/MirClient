//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
#include <tchar.h>
//---------------------------------------------------------------------------
USEFORM("..\Common\RibbonRichEditDemoOptions.cpp", RibbonDemoOptionsForm);
USEFORM("..\Common\RichEditControlBase.cpp", frmRichEditControlBase);
USEFORM("RichEditMasterDetailMailMerge.cpp", frmRichEditMasterDetailMailMerge);
//---------------------------------------------------------------------------
int WINAPI _tWinMain(HINSTANCE, HINSTANCE, LPTSTR, int)
{
	try
	{
		Application->Title = "Rich Edit Master-Detail Mail Merge Demo";
		Application->CreateForm(__classid(TfrmRichEditMasterDetailMailMerge), &frmRichEditMasterDetailMailMerge);
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
