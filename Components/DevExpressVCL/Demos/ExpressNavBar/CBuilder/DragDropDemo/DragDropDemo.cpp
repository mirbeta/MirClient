//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("DragDropDemo.res");
USEFORM("DragDropMain.cpp", fmDragDropMain);
USEFORM("..\Common\NavBarUtils.cpp", dmCommonData);
//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
                 Application->Initialize();
                 Application->CreateForm(__classid(TdmCommonData), &dmCommonData);
                 Application->CreateForm(__classid(TfmDragDropMain), &fmDragDropMain);
                 Application->Run();
        }
        catch (Exception &exception)
        {
                 Application->ShowException(&exception);
        }
        return 0;
}
//---------------------------------------------------------------------------
