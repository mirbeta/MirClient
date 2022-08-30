unit IssueListProjects;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, IssueListForm, ImgList, ComCtrls, ToolWin, ExtCtrls, StdCtrls,
  cxControls, cxContainer, cxEdit, cxTextEdit, cxDBEdit, cxMaskEdit,
  cxDropDownEdit, cxLookupEdit, cxDBLookupComboBox, DBCtrls, cxGraphics,
  cxLookAndFeels, cxLookAndFeelPainters, cxDBLookupEdit, cxNavigator, cxLabel;

type
  TfrmProjects = class(TfrmBasic)
    cxDBTextEdit1: TcxDBTextEdit;
    Label5: TcxLabel;
    cxDBLookupComboBox3: TcxDBLookupComboBox;
    Label1: TcxLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmProjects: TfrmProjects;

implementation

{$R *.dfm}

uses
  IssueListForms, IssueListConst, IssueListData;

initialization

  FormManager.RegisterForm(ProjectsFormID, TfrmProjects, 'PROJECTS');


end.
