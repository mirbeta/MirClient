unit IssueListDepartments;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, IssueListForm, ImgList, ComCtrls, ToolWin, ExtCtrls, StdCtrls,
  cxControls, cxContainer, cxEdit, cxTextEdit, cxDBEdit, DBCtrls, cxGraphics,
  cxLookAndFeels, cxLookAndFeelPainters, cxNavigator, cxLabel;

type
  TfrmDepartments = class(TfrmBasic)
    cxDBTextEdit1: TcxDBTextEdit;
    Label1: TcxLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmDepartments: TfrmDepartments;

implementation

{$R *.dfm}

uses
  IssueListForms, IssueListConst;

initialization

  FormManager.RegisterForm(DepartmentsFormID, TfrmDepartments, 'DEPARTMENTS');


end.
