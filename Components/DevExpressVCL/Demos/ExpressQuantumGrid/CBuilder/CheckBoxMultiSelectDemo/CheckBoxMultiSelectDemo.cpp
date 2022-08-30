//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USERES("CheckBoxMultiSelectDemo.res");
USEFORM("..\Common\CarsDataForGrid.cpp", dmGridCars);
USEFORM("CheckBoxMultiSelectMain.cpp", frmMain);
USEFORM("..\AboutDemoForm.cpp", formAboutDemo);
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
				 Application->Initialize();
				 Application->Title = "ExpressQuantumGrid Web-Style Row Selection Demo";
				 Application->CreateForm(__classid(TdmGridCars), &dmGridCars);
				 Application->CreateForm(__classid(TfrmMain), &frmMain);
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
