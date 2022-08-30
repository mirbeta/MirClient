//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
#include <tchar.h>
//---------------------------------------------------------------------------
USEFORM("..\Common\NotepadMainForm.cpp", frmNotepadMain);
USEFORM("..\Common\EBarsDemoRating.cpp", EBarsDemoRatingForm);
USEFORM("..\Common\EBarsUtils.cpp", dmCommonData); /* TDataModule: File Type */
USEFORM("..\Common\NotepadChildForm.cpp", frmNotepadChild);
USEFORM("..\Common\AboutDemoForm.cpp", formAboutDemo);
USEFORM("RibbonNotepadMainForm.cpp", frmRibbonNotepadMain);
USEFORM("RibbonNotepadDemoOptions.cpp", RibbonDemoOptionsForm);
USEFORM("RibbonNotepadChildForm.cpp", frmRibbonNotepadChild);
USEFORM("RibbonNotepadDemoGallerySetup.cpp", frmRibbonNotepadDemoGallerySetup);
//---------------------------------------------------------------------------
int WINAPI _tWinMain(HINSTANCE, HINSTANCE, LPTSTR, int)
{
	try
	{
		Application->Initialize();
		Application->MainFormOnTaskBar = true;
		Application->CreateForm(__classid(TdmCommonData), &dmCommonData);
		Application->CreateForm(__classid(TfrmRibbonNotepadMain), &frmRibbonNotepadMain);
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
