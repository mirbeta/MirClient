program Office12ViewsDemoD103Rio;

uses
  Forms,
  Office12ViewsMain in 'Office12ViewsMain.pas' {fmMain};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
