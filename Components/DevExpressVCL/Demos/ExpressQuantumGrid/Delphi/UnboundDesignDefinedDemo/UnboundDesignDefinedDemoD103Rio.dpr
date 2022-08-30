program UnboundDesignDefinedDemoD103Rio;

uses
  Forms,
  UnboundDesignDefinedDemoMain in 'UnboundDesignDefinedDemoMain.pas' {UnboundDesignDefinedDemoMainForm},
  BaseForm in '..\BaseForm.pas' {fmBaseForm},
  SkinDemoUtils in '..\SkinDemoUtils.pas',
  AboutDemoForm in '..\AboutDemoForm.pas' {formAboutDemo};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.Title := 'ExpressQuantumGrid UnboundDesignDefined Demo';
  Application.CreateForm(TUnboundDesignDefinedDemoMainForm, UnboundDesignDefinedDemoMainForm);
  Application.Run;
end.
