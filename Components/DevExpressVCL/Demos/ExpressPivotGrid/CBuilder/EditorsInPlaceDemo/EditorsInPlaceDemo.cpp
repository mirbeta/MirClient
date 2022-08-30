//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USEFORM("..\Common\DemoBasicMain.cpp", frmDemoBasicMain);
USEFORM("..\Common\AboutDemoForm.cpp", formAboutDemo);
USEFORM("..\Common\DemoBasicDM.cpp", dmOrders); /* TDataModule: File Type */
USEFORM("EditorsInPlaceDemoMain.cpp", frmEditorsInPlace);
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
		Application->CreateForm(__classid(TdmOrders), &dmOrders);
		Application->CreateForm(__classid(TfrmEditorsInPlace), &frmEditorsInPlace);
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
