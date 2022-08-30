//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USERES("IssueList.res");
USEFORM("IssueListMain.cpp", IssueListMainForm);
USEFORM("IssueListData.cpp", dmMain); /* TDataModule: File Type */
USEFORM("..\BaseForm.cpp", fmBaseForm);
USEFORM("..\AboutDemoForm.cpp", formAboutDemo);
USEFORM("IssueListGrid.cpp", IssueListGridForm);
USEFORM("IssueListForm.cpp", frmBasic);
USEFORM("IssueListUsers.cpp", frmUsers);
USEFORM("IssueListDepartments.cpp", frmDepartments);
USEFORM("IssueListItems.cpp", frmItems);
USEFORM("IssueListProjects.cpp", frmProjects);
USEFORM("IssueListSchedule.cpp", frmSchedule);
USEFORM("IssueListTeams.cpp", frmTeams);
USEFORM("IssueListStyles.cpp", IssueListStylesForm);
USEFORM("IssueListStyleData.cpp", dmStyles); /* TDataModule: File Type */
USEUNIT("IssueListConst.cpp");
USEUNIT("IssueListFrames.cpp");

//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
                 Application->Initialize();
                 Application->CreateForm(__classid(TdmMain), &dmMain);
                 Application->CreateForm(__classid(TIssueListMainForm), &IssueListMainForm);
                 Application->CreateForm(__classid(TdmStyles), &dmStyles);
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
