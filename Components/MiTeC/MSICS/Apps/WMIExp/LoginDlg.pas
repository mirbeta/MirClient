unit LoginDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Buttons;

type
  Tdlg_wmie_Login = class(TForm)
    Label1: TLabel;
    eMachine: TEdit;
    Label2: TLabel;
    eUser: TEdit;
    Label3: TLabel;
    ePwd: TEdit;
    Label4: TLabel;
    cbRoot: TComboBox;
    bOK: TButton;
    bCancel: TButton;
    Bevel1: TBevel;
  private
  public
  end;

function ShowLoginDlg(var AMachine,AUser,APwd,ARoot: string): Boolean;

var
  dlg_wmie_Login: Tdlg_wmie_Login;

implementation

{$R *.dfm}

function ShowLoginDlg;
begin
  with Tdlg_wmie_Login.Create(Application.MainForm) do
    try
      Result:=ShowModal=mrOK;
      if Result then begin
        AMachine:=eMachine.Text;
        AUser:=eUser.Text;
        APwd:=ePwd.Text;
        ARoot:=cbRoot.Text;
      end;
    finally
      Free;
    end;
end;


end.
