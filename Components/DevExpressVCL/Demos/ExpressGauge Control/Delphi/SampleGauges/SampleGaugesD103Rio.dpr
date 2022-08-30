program SampleGaugesD103Rio;

uses
  Forms,
  uSampleGauges in 'uSampleGauges.pas' {frmSampleGauges};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.Title := 'Sample Gauges ';
  Application.CreateForm(TfrmSampleGauges, frmSampleGauges);
  Application.Run;
end.
