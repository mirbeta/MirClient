//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
#pragma package(smart_init)
#include <tchar.h>
//---------------------------------------------------------------------------
USEFORM("..\Common\DemoBasicMain.cpp", DemoBasicMainForm);
USEFORM("RangeControlDemoMain.cpp", RangeControlDemoMainForm);
USEFORM("..\Common\AboutDemoForm.cpp", formAboutDemo);
//---------------------------------------------------------------------------
int WINAPI _tWinMain(HINSTANCE, HINSTANCE, LPTSTR, int)
{
	try
	{
		Application->Initialize();
		Application->MainFormOnTaskBar = true;
		Application->CreateForm(__classid(TRangeControlDemoMainForm), &RangeControlDemoMainForm);
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
