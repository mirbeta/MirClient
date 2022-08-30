program PerfView;

uses
  Forms,
  Main in 'Main.pas' {frmMain};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Performance Viewer';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
