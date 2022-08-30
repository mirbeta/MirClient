//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USEFORM("FrameAnimationDemoMain.cpp", fmFrameAnimationMain);
USEFORM("FrameAnimationDemoDM.cpp", DM); /* TDataModule: File Type */
USEFORM("..\Common\AboutDemoForm.cpp", formAboutDemo);
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
	try
	{
		Application->Initialize();
		Application->MainFormOnTaskBar = true;
		Application->Title = "FrameAnimationDemo";
		Application->CreateForm(__classid(TDM), &DM);
		Application->CreateForm(__classid(TfmFrameAnimationMain), &fmFrameAnimationMain);
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
