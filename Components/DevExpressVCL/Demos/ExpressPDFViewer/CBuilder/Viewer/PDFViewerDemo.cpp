//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
#include <tchar.h>
//---------------------------------------------------------------------------
USEFORM("uExportToFileDialog.cpp", frmExportToFileDialog);
USEFORM("uPDFViewerDemo.cpp", frmPDFViewer);
USEFORM("uExportToBitmaps.cpp", frmExportToBitmaps);
USEFORM("..\Common\dxProgressDialog.cpp", frmProgress);
USEFORM("..\Common\AboutDemoForm.cpp", formAboutDemo);
//---------------------------------------------------------------------------
int WINAPI _tWinMain(HINSTANCE, HINSTANCE, LPTSTR, int)
{
	try
	{
		Application->Initialize();
		Application->MainFormOnTaskBar = true;
		Application->CreateForm(__classid(TfrmPDFViewer), &frmPDFViewer);
		Application->CreateForm(__classid(TfrmExportToFileDialog), &frmExportToFileDialog);
		Application->CreateForm(__classid(TfrmExportToBitmaps), &frmExportToBitmaps);
		Application->CreateForm(__classid(TfrmProgress), &frmProgress);
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
