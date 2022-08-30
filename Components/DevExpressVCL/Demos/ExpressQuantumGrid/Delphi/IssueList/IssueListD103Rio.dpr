program IssueListD103Rio;

uses
  Forms,
  IssueListMain in 'IssueListMain.pas' {IssueListMainForm},
  IssueListData in 'IssueListData.pas' {dmMain: TDataModule},
  IssueListConst in 'IssueListConst.pas',
  IssueListForm in 'IssueListForm.pas' {frmBasic: TForm},
  IssueListForms in 'IssueListForms.pas',
  IssueListUsers in 'IssueListUsers.pas' {frmUsers: TForm},
  IssueListItems in 'IssueListItems.pas' {frmItems: TForm},
  IssueListTeams in 'IssueListTeams.pas' {frmTeams: TForm},
  IssueListDepartments in 'IssueListDepartments.pas' {frmDepartments: TForm},
  IssueListProjects in 'IssueListProjects.pas' {frmProjects: TForm},
  IssueListGrid in 'IssueListGrid.pas' {IssueListGridForm},
  IssueListSchedule in 'IssueListSchedule.pas' {frmSchedule: TForm},
  IssueListStyleData in 'IssueListStyleData.pas' {dmStyles: TDataModule},
  IssueListStyles in 'IssueListStyles.pas' {IssueListStylesForm},
  AboutDemoForm in '..\AboutDemoForm.pas' {formAboutDemo},
  BaseForm in '..\BaseForm.pas' {fmBaseForm},
  SkinDemoUtils in '..\SkinDemoUtils.pas',
  DemoUtils in '..\DemoUtils.pas';

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.Title := 'Issue List';
  Application.CreateForm(TdmMain, dmMain);
  Application.CreateForm(TIssueListMainForm, IssueListMainForm);
  Application.Run;
end.
