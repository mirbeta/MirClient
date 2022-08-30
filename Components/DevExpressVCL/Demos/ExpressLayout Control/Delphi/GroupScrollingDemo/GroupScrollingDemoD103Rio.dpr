program GroupScrollingDemoD103Rio;

uses
  Forms,
  Main in 'Main.pas' {Form17},
  DemoDM in '..\Common\DemoDM.pas' {dmDemo: TDataModule};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm17, Form17);
  Application.CreateForm(TdmDemo, dmDemo);
  Application.Run;
end.
