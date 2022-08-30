//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
#include <tchar.h>
//---------------------------------------------------------------------------
USEFORM("LayoutControlRLMain.cpp", LayoutControlMainForm);
USEFORM("..\Common\DemoBasicMain.cpp", DemoBasicMainForm);
USEFORM("..\Common\AboutDemoForm.cpp", formAboutDemo);
USEFORM("DemoDM.cpp", dmDemo); /* TDataModule: File Type */
//---------------------------------------------------------------------------
int WINAPI _tWinMain(HINSTANCE, HINSTANCE, LPTSTR, int)
{
	try
	{
		Application->Initialize();
		Application->MainFormOnTaskBar = true;
		Application->CreateForm(__classid(TdmDemo), &dmDemo);
		Application->CreateForm(__classid(TLayoutControlMainForm), &LayoutControlMainForm);
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
