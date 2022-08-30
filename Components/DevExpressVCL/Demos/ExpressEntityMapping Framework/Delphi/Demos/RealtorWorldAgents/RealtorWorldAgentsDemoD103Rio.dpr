program RealtorWorldAgentsDemoD103Rio;

uses
  Forms,
  RealtorWorldAgents in 'RealtorWorldAgents.pas' {frmAgents},
  RealtorWorldDM in '..\Common\RealtorWorldDM.pas' {DMRealtorWorld: TDataModule},
  RealtorWorld.Entities in '..\Common\RealtorWorld.Entities.pas',
  RealtorWorld.Linq in '..\Common\RealtorWorld.Linq.pas',
  AboutDemoForm in '..\AboutDemoForm.pas' {formAboutDemo};

  {$R *.res}
  {$R WindowsXP.res}


begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDMRealtorWorld, DMRealtorWorld);
  Application.CreateForm(TfrmAgents, frmAgents);
  Application.Run;
end.
