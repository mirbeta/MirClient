//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USERES("EditorsInPlaceValidationDemoMain.res");
USEFORM("EditorsInPlaceValidationDemoMain.cpp", EditorsInPlaceValidationDemoMainForm);
USEFORM("..\AboutDemoForm.cpp", formAboutDemo);
USEFORM("..\BaseForm.cpp", fmBaseForm);
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
	try
	{
		Application->Initialize();
		Application->Title = "ExpressQuantumGrid EditorsInPlaceValidation Demo";
		Application->CreateForm(__classid(TEditorsInPlaceValidationDemoMainForm), &EditorsInPlaceValidationDemoMainForm);
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
