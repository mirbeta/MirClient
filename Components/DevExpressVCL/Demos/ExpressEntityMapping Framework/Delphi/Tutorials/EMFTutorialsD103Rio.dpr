program EMFTutorialsD103Rio;

uses
  Forms,
  uBaseDemoForm in 'uBaseDemoForm.pas' {BaseDemoForm},
  dxDemoTutorial in 'dxDemoTutorial.pas',
  uMain in 'uMain.pas' {frmMain},
  dxSplashUnit in 'dxSplashUnit.pas' {frmSplash},
  EMFDemoClasses in 'TutorialUnits\EMFDemoClasses.pas',
  EMFCustomTutorial in 'TutorialUnits\EMFCustomTutorial.pas' {frmEMFCustomTutorial},
  EMFCreateConnection in 'TutorialUnits\EMFCreateConnection.pas' {frmEMFCreateConnection},
  EMFQueryEntity in 'TutorialUnits\EMFQueryEntity.pas' {frmEMFQueryEntity},
  EMFCreateEntity in 'TutorialUnits\EMFCreateEntity.pas' {frmEMFCreateEntity},
  EMFUpdateEntity in 'TutorialUnits\EMFUpdateEntity.pas' {frmEMFUpdateEntity},
  EMFUpdateEntityOneToMany in 'TutorialUnits\EMFUpdateEntityOneToMany.pas' {frmEMFUpdateEntityOneToMany},
  EMFDeleteEntity in 'TutorialUnits\EMFDeleteEntity.pas' {frmEMFDeleteEntity},
  EMFDeleteEntityOneToMany in 'TutorialUnits\EMFDeleteEntityOneToMany.pas' {frmEMFDeleteEntityOneToMany},
  EMFCreateEntityOneToMany in 'TutorialUnits\EMFCreateEntityOneToMany.pas' {frmEMFCreateEntityOneToMany},
  EMFQueryEntities in 'TutorialUnits\EMFQueryEntities.pas' {frmEMFQueryEntities},
  EMFQueryEntitiesUsingCriteria in 'TutorialUnits\EMFQueryEntitiesUsingCriteria.pas' {frmEMFQueryEntitiesUsingCriteria},
  EMFQueryEntitiesUsingLINQ in 'TutorialUnits\EMFQueryEntitiesUsingLINQ.pas' {frmEMFQueryEntitiesUsingLINQ},
  EMFUsingDataset in 'TutorialUnits\EMFUsingDataset.pas' {frmEMFUsingDataset};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
