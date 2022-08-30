//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USEFORM("ImageViewerDemoMain.cpp", ImageViewerDemoMainForm);
USEFORM("ImageViewerDemoResizeImage.cpp", ImageViewerDemoResizeImageForm);
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
	try
	{
		Application->Initialize();
                Application->Title = "ExpressEditors ImageViewer Demo";
		Application->CreateForm(__classid(TImageViewerDemoMainForm), &ImageViewerDemoMainForm);
		Application->CreateForm(__classid(TImageViewerDemoResizeImageForm), &ImageViewerDemoResizeImageForm);
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
