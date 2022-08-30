//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USERES("ColumnsShareDemo.res");
USEFORM("ColumnsShareDemoMain.cpp", ColumnsShareDemoMainForm);
USEFORM("ColumnsShareDemoData.cpp", ColumnsShareDemoMainDM); /* TDataModule: File Type */
USEFORM("..\AboutDemoForm.cpp", formAboutDemo);
USEFORM("ColumnsShareDemoLookupCustomize.cpp", ColumnsShareDemoLookupCustomizeForm);
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
     Application->Initialize();
     Application->Title = "ExpressQuantumGrid Columns Share Demo";
     Application->CreateForm(__classid(TColumnsShareDemoMainDM), &ColumnsShareDemoMainDM);
     Application->CreateForm(__classid(TColumnsShareDemoMainForm), &ColumnsShareDemoMainForm);
     Application->CreateForm(__classid(TColumnsShareDemoLookupCustomizeForm), &ColumnsShareDemoLookupCustomizeForm);
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
