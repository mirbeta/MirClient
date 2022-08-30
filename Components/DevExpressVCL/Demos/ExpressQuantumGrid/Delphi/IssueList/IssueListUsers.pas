unit IssueListUsers;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, IssueListForm, ExtCtrls, StdCtrls, ComCtrls, ToolWin, ImgList,
  cxMaskEdit, cxDropDownEdit, cxLookupEdit, cxDBLookupComboBox, cxControls,
  cxContainer, cxEdit, cxTextEdit, cxDBEdit, DBCtrls, cxDBLookupEdit,
  cxNavigator, cxGraphics, cxLookAndFeels, cxLookAndFeelPainters, cxLabel;

type
  TfrmUsers = class(TfrmBasic)
    Label9: TcxLabel;
    cxDBTextEdit1: TcxDBTextEdit;
    Label1: TcxLabel;
    cxDBTextEdit2: TcxDBTextEdit;
    Label2: TcxLabel;
    cxDBTextEdit3: TcxDBTextEdit;
    Label3: TcxLabel;
    cxDBMaskEdit1: TcxDBMaskEdit;
    Label4: TcxLabel;
    Label5: TcxLabel;
    cxDBLookupComboBox3: TcxDBLookupComboBox;
    cxDBTextEdit4: TcxDBTextEdit;
  private
    { Private declarations }
  public
  end;

  
implementation

{$R *.dfm}

uses
  IssueListForms, IssueListConst, IssueListData;

{ TfrmUsers }

initialization

 FormManager.RegisterForm(UsersFormID, TfrmUsers, 'USERS');

end.
