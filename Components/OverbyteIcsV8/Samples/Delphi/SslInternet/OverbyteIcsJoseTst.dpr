program OverbyteIcsJoseTst;

uses
  Forms,
  OverbyteIcsJoseTst1 in 'OverbyteIcsJoseTst1.pas' {JsonDemoForm};

{$R *.res}

begin
  Application.Initialize;
//  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TJsonDemoForm, JsonDemoForm);
  Application.Run;
end.
