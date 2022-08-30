//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USEFORM("ServerModeDemoData.cpp", ServerModeDemoDataDM); /* TDataModule: File Type */
USEFORM("..\AboutDemoForm.cpp", formAboutDemo);
USEFORM("..\BaseForm.cpp", fmBaseForm);
USEFORM("ServerModeDemoMain.cpp", ServerModeDemoMainForm);
USEFORM("ServerModeDemoConnection.cpp", ServerModeDemoConnectionForm);
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
	try
	{
		Application->Initialize();
	#if __BORLANDC__ >= 0x700
		Application->MainFormOnTaskBar = true;
	#else
		SetApplicationMainFormOnTaskBar(Application, true);
	#endif
		Application->CreateForm(__classid(TServerModeDemoConnectionForm), &ServerModeDemoConnectionForm);
		Application->CreateForm(__classid(TServerModeDemoDataDM), &ServerModeDemoDataDM);
		Application->CreateForm(__classid(TServerModeDemoMainForm), &ServerModeDemoMainForm);
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
