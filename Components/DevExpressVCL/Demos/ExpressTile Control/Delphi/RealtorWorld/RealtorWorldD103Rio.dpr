program RealtorWorldD103Rio;

uses
  Forms,
  RealtorWorldMain in 'RealtorWorldMain.pas' {frmRealtorWorld},
  RealtorWorldDM in 'RealtorWorldDM.pas' {DMRealtorWorld: TDataModule},
  RealtorWorldListing in 'RealtorWorldListing.pas' {frmListing: TFrame},
  RealtorWorldAgents in 'RealtorWorldAgents.pas' {frmAgents: TFrame},
  RealtorWorldResearch in 'RealtorWorldResearch.pas' {frmResearch: TFrame},
  RealtorWorldUnderConstruction in 'RealtorWorldUnderConstruction.pas' {frmUnderConstruction: TFrame},
  RealtorWorldLoanCalculator in 'RealtorWorldLoanCalculator.pas' {frmLoanCalculator: TFrame},
  RealtorWorldMortgageRate in 'RealtorWorldMortgageRate.pas' {frmMortgageRate: TFrame},
  RealtorWorldSystemInformation in 'RealtorWorldSystemInformation.pas' {frmSystemInformation: TfrmSystemInformation},
  RealtorWorldStatistic in 'RealtorWorldStatistic.pas' {frmStatistic: TFrame},
  RealtorWorldBaseFrame in 'RealtorWorldBaseFrame.pas' {frmBase: TFrame},
  RealtorWorldHomePhotosBase in 'RealtorWorldHomePhotosBase.pas' {frmHomePhotosBase: TFrame},
  RealtorWorldDataPath in 'RealtorWorldDataPath.pas';

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.CreateForm(TDMRealtorWorld, DMRealtorWorld);
  Application.CreateForm(TfrmRealtorWorld, frmRealtorWorld);
  Application.Run;
end.
