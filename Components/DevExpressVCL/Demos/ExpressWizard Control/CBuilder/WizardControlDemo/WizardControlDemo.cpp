//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
#include <tchar.h>
//---------------------------------------------------------------------------
USEFORM("WizardControlDemoMainForm.cpp", frmWizardControlDemoMain);
USEFORM("WizardControlDemoSetupForm.cpp", WizardControlDemoSetupForm);
//---------------------------------------------------------------------------
int WINAPI _tWinMain(HINSTANCE, HINSTANCE, LPTSTR, int)
{
	try
	{
		Application->Initialize();
		Application->CreateForm(__classid(TfrmWizardControlDemoMain), &frmWizardControlDemoMain);
		Application->CreateForm(__classid(TWizardControlDemoSetupForm), &WizardControlDemoSetupForm);
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
