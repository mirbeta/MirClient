program RtcFunctionsWizard;

uses
//  FastMM4,
  Forms,
  MainForm in 'MainForm.pas' {formMain},
  SigFuncs in 'SigFuncs.pas',
  gmoOptions in 'gmoOptions.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TformMain, formMain);
  Application.Run;
end.
