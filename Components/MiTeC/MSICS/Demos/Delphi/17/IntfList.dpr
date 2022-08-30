program IntfList;

uses
  Forms,
  Main in 'Main.pas' {wndMain},
  IntfDlg in 'IntfDlg.pas' {dlgIntf};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TwndMain, wndMain);
  Application.Run;
end.
