unit IssueListTeams;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, IssueListForm, ImgList, ComCtrls, ToolWin, ExtCtrls, StdCtrls,
  cxMaskEdit, cxDropDownEdit, cxLookupEdit, cxDBLookupComboBox, cxControls,
  cxContainer, cxEdit, cxTextEdit, cxDBEdit, DBCtrls, cxDBLookupEdit,
  cxNavigator, cxGraphics, cxLookAndFeels, cxLookAndFeelPainters, cxLabel;

type
  TfrmTeams = class(TfrmBasic)
    Label9: TcxLabel;
    cxDBTextEdit1: TcxDBTextEdit;
    Label5: TcxLabel;
    cxDBLookupComboBox3: TcxDBLookupComboBox;
    Label1: TcxLabel;
    cxDBLookupComboBox1: TcxDBLookupComboBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmTeams: TfrmTeams;

implementation

{$R *.dfm}

uses
  IssueListForms, IssueListConst, IssueListData;

initialization

  FormManager.RegisterForm(TeamsFormID, TfrmTeams, 'PROJECT TEAMS');

end.
 
