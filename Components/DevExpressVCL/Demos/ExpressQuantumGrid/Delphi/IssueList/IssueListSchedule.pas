unit IssueListSchedule;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, IssueListForm, cxDropDownEdit, cxCalendar, cxDBEdit,
  cxImageComboBox, cxMaskEdit, cxLookupEdit, cxDBLookupComboBox, cxControls,
  cxContainer, cxEdit, cxTextEdit, ExtCtrls, StdCtrls, cxSpinEdit, DBCtrls,
  cxGraphics, cxLookAndFeels, cxLookAndFeelPainters, cxNavigator, cxLabel;

type
  TfrmSchedule = class(TfrmBasic)
    cxDBTextEdit1: TcxDBTextEdit;
    cxDBTextEdit2: TcxDBTextEdit;
    cxDBTextEdit3: TcxDBTextEdit;
    cxDBTextEdit4: TcxDBTextEdit;
    cxDBTextEdit5: TcxDBTextEdit;
    cxDBTextEdit6: TcxDBTextEdit;
    cxDBTextEdit7: TcxDBTextEdit;
    cxDBTextEdit8: TcxDBTextEdit;
    cxDBTextEdit9: TcxDBTextEdit;
    Label1: TcxLabel;
    Label2: TcxLabel;
    Label3: TcxLabel;
    Label4: TcxLabel;
    Label5: TcxLabel;
    Label6: TcxLabel;
    Label7: TcxLabel;
    Label8: TcxLabel;
    Label9: TcxLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmSchedule: TfrmSchedule;

implementation

{$R *.dfm}

uses
  IssueListForms, IssueListConst, IssueListData;


initialization

  FormManager.RegisterForm(ScheduleFormID, TfrmSchedule, 'SCHEDULE');

end.
