program RBE;

uses
  Forms,
  Main in 'Main.pas' {appRBE},
  Details in 'Details.pas' {wndDetails};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'ROM BIOS Explorer';
  Application.CreateForm(TappRBE, appRBE);
  Application.Run;
end.
